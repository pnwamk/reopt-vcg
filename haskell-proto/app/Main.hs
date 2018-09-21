{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Main (MetaVCGConfig(..), main) where

import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad (forM_)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.ST
import           Control.Monad.State
import qualified Data.ByteString.Char8 as BSC
import           Data.List as List
import qualified Data.Macaw.Architecture.Info as M
import           Data.Macaw.CFG
import           Data.Macaw.Discovery
import qualified Data.Macaw.Types as M
import           Data.Macaw.X86
import           Data.Macaw.X86.X86Reg
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Parameterized.Nonce
import           Data.Parameterized.Some
import qualified Data.Yaml as Yaml
import           Lang.Crucible.Backend.Simple
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           Text.LLVM hiding ((:>), Value)
--import           Text.Read (readMaybe)
import           What4.Interface
import           What4.Solver.CVC4

import           Reopt.VCG.Config

import           VCGCommon
import qualified VCGLLVM as L
import qualified VCGMacaw as M

-- Maps LLVM block labels to the Macaw adddress they are associated with.
type BlockMapping = Map BlockLabel (MemSegmentOff 64)

data VCGConfig sym ids = VCGConfig
  { blockMapping :: !BlockMapping
  , llvmMod      :: Module
  , exprBuilder  :: sym
  , symIsBuilder :: (forall a . (IsSymExprBuilder sym => a) -> a)
  }

data PfState sym = PfState
  { preConds :: [Pred sym]
  , goals :: [Pred sym]
  }

--TODO: use newtype
type VCGMonad sym ids = ReaderT (VCGConfig sym ids) (StateT (PfState sym) IO)

addProofGoal :: IsSymExprBuilder sym => sym -> Pred sym -> VCGMonad sym ids ()
addProofGoal sym p = addProofGoals sym [p]

addProofGoals :: IsSymExprBuilder sym => sym -> [Pred sym] -> VCGMonad sym ids ()
addProofGoals _ ps = modify $ \s -> s { goals = ps ++ goals s }

addProofPreCond :: IsSymExprBuilder sym => sym -> Pred sym -> VCGMonad sym ids ()
addProofPreCond sym p = addProofPreConds sym [p]

addProofPreConds :: IsSymExprBuilder sym => sym -> [Pred sym] -> VCGMonad sym ids ()
addProofPreConds _sym ps = modify $ \s -> s { preConds = ps ++ preConds s }

{-
-- | Insert a false predicate to make sure the proof fail
proofFailed :: IsSymExprBuilder sym => sym -> VCGMonad sym ids ()
proofFailed sym = do
  liftIO $ putStrLn "Warning: proof failed."
  addProofGoal sym (falsePred sym)
-}

disjointConstraintsHelper :: IsSymExprBuilder sym => sym -> [Ptr sym] -> IO [Pred sym]
disjointConstraintsHelper _sym [] = return []
disjointConstraintsHelper sym (p:ptrs) = do
  let gen p2 = notPred sym =<< bvEq sym p p2
  c1 <- traverse gen ptrs
  c2 <- disjointConstraintsHelper sym ptrs
  return $ c1 ++ c2

-- Generate disjointness constraints. Because the addresses of LLVM and of Macaw share the
-- same SMT bitvector space, we also need to make sure they won't alias.
-- TODO: is there any other disjointness constraints?
disjointConstraints :: IsSymExprBuilder sym => sym -> L.LState sym -> M.MState sym ids -> IO [Pred sym]
disjointConstraints sym lst _mst = disjointConstraintsHelper sym (L.disjoint lst)

{-
-- | Return true if the givne LLVM blocklabel corresponds with the address of the x86 block.
matchLabel :: BlockMapping
           -> BlockLabel
           -> MemSegmentOff 64
           -> Bool
matchLabel m llbl mlbl =
  case Map.lookup llbl m of
    Just massoc -> mlbl == massoc
    Nothing -> error $ "No association for " ++ show llbl
-}

{-
jumpEventEq :: IsSymExprBuilder sym
            => sym
            -> L.LEvent sym
            -> M.MEvent sym ids
            -> VCGMonad sym ids ()
-- Conditional jumps
jumpEventEq sym (L.BranchEvent lcnd tlbl flbl) (M.BranchEvent mcnd (toff,_tregs) (foff,_fregs)) = do
  bm <- asks blockMapping
  pred1 <-
    if matchLabel bm tlbl toff && matchLabel bm flbl foff then
      liftIO $ isEq sym lcnd mcnd
     else
      pure $! falsePred sym
  pred2 <-
    if matchLabel bm tlbl foff && matchLabel bm flbl toff then
      liftIO $ isEq sym lcnd mcnd
     else
      pure $! falsePred sym
  -- TODO: Check registers
  p <- liftIO $ orPred sym pred1 pred2
  addProofGoal sym p
-- Unconditional jumps
jumpEventEq sym (L.JumpEvent lbl) (M.JumpEvent offset _) = do
  m <- asks blockMapping
  case Map.lookup lbl m of
    Just off' -> do
      when (off' /= offset) $ do
        proofFailed sym
    Nothing -> do
      error $ "Could not find map for block " ++ show lbl
jumpEventEq sym _ _ = proofFailed sym
-}

-- | Assert that the functions identified by the LLVM and macaw function pointers
-- are equivalent.
assertFnNameEq :: SymBV sym 64 -> SymBV sym 64 -> VCGMonad sym ids ()
assertFnNameEq llvmFun macawIP = undefined llvmFun macawIP

x86ArgRegs :: [X86Reg (M.BVType 64)]
x86ArgRegs = [ RDI, RSI, RDX, RCX, R8, R9 ]


eventsEq :: forall sym ids
         .  IsSymExprBuilder sym
         => sym
         -> [L.LEvent sym]
         -> [M.MEvent sym]
         -> VCGMonad sym ids ()
eventsEq _sym [] [] = return ()
eventsEq sym (L.AllocaEvent{}:levs) mevs =
  eventsEq sym levs mevs --Skip alloca events
--eventsEq sym levs (M.AllocaEvent{}:mevs) =
--  eventsEq sym levs mevs --Skip alloca events
eventsEq sym (L.InvokeEvent f lArgs lRet:levs)
             (M.FetchAndExecuteEvent regs:mevs) = do
  let M.MSymExpr mRegIP = regs ^. boundValue X86_IP
  assertFnNameEq f mRegIP
  -- Verify that the arguments should be same.
  -- Note: Here we take the number of arguments from LLVM side,
  -- since the number of arguments in Macaw side seems not explicit.
  -- Also assuming that the # of arguments of LLVM side is less or equal than six.
  when (length lArgs > length x86ArgRegs) $ do
    error $ "Too many arguments."

  let compareArg :: Some (SymExpr sym) -> X86Reg (M.BVType 64) -> VCGMonad sym ids ()
      compareArg (Some la) reg = do
        let M.MSymExpr ma = regs^.boundValue reg
        case testEquality (exprType la) (BaseBVRepr (knownNat @64)) of
          Just Refl -> do
            eq <- liftIO $ bvEq sym la ma
            addProofGoal sym eq
          Nothing -> do
            error $ "Unexpected argument type " ++ show (exprType la)
  zipWithM_ compareArg lArgs x86ArgRegs
  -- If LLVM side has a return value, then we assert lRet = mRet as precondition
  -- for the rest program.
  case lRet of
    Just (_id, Some lRetVal)
      | Just Refl <- testEquality (exprType lRetVal) (BaseBVRepr (knownNat @64)) -> do
          let M.MSymExpr mRetVal = regs^.boundValue RAX
          retEq <- liftIO $ bvEq sym lRetVal mRetVal
          -- We can assume return values are equal.
          addProofPreCond sym retEq
      | otherwise -> do
          error $ "Unexpected return type " ++ show (exprType lRetVal)
    Nothing -> pure ()
  eventsEq sym levs mevs
eventsEq sym (L.JumpEvent lbl:levs) (M.FetchAndExecuteEvent regs:mevs) = do
  m <- asks blockMapping
  case Map.lookup lbl m of
    Just off -> do
      let regionMap = error "region index map is not defined."
      llvmMemAddr <- liftIO  $ M.evalMemAddr sym regionMap (relativeSegmentAddr off)
      let M.MSymExpr mRegIP = regs ^. boundValue X86_IP
      eq <- liftIO $ bvEq sym llvmMemAddr mRegIP
      addProofGoal sym eq

    Nothing -> do
      error $ "Could not find map for block " ++ show lbl
  eventsEq sym levs mevs
eventsEq sym (L.ReturnEvent mlret:levs) (M.FetchAndExecuteEvent regs:mevs) = do
  case mlret of
    Nothing -> pure ()
    Just (Some lret) ->
      case testEquality (exprType lret) (BaseBVRepr (knownNat @64)) of
        Just Refl -> do
          let M.MSymExpr mret = regs^.boundValue RAX
          eq <- liftIO $ bvEq sym lret mret
          addProofGoal sym eq
        Nothing -> do
          error $ "Unexpected argument type " ++ show (exprType lret)
  eventsEq sym levs mevs
eventsEq _ (lev:_) [] = do
  error $ "LLVM after end of Macaw events:\n"
    ++ L.ppEvent lev
eventsEq _ [] (mev:_) = do
  error $ "Macaw event after end of LLVM events:\n"
    ++ M.ppEvent mev
eventsEq _ (lev:_) (mev:_) = do
  error $ "Incompatible LLVM and Macaw events:\n"
    ++ "LLVM:  " ++ L.ppEvent lev ++ "\n"
    ++ "Macaw: " ++ M.ppEvent mev




{-
retValugeEq :: IsSymExprBuilder sym
           => sym -> Maybe (SymBV64 sym) -> Maybe (SymBV64 sym)
           -> VCGMonad sym ids ()
retValueEq sym (Just bv1) (Just bv2) = do
  eq <- liftIO $ bvEq sym bv1 bv2
  addProofGoal sym eq
retValueEq _sym (Just _) _ = do
  liftIO $ warning "LLVM block does not have return value, proof failed"
retValueEq _sym _ (Just _) = do
  liftIO $ warning "Macaw block does not have return value, proof failed"
retValueEq _sym _ _ = do
  liftIO $ warning "return value is not provided"
-}

stateEq :: IsSymExprBuilder sym
        => sym -> L.LState sym -> M.MState sym ids
        -> VCGMonad sym ids ()
stateEq sym lstate mstate = do
  -- Check event traces equality
  let levs = reverse $ L.events lstate
  let mevs = reverse $ M.events mstate
  eventsEq sym levs mevs

{-
simulateAndCheck :: IsSymExprBuilder sym
                 => sym
                 -> VCGFunInfo
                 -> BasicBlock
                 -> DiscoveryFunInfo X86_64 ids
                 -> L.LState sym
                 -> M.MState sym ids
                 -> VCGMonad sym ids ()
simulateAndCheck sym _vfi bb discInfo ls0 ms0 = do
  liftIO $ putStrLn "Simulating LLVM program ..."
  ls' <- liftIO $ execStateT (L.bb2SMT sym bb) ls0

  liftIO $ putStrLn "Simulating Macaw program ..."
  ms' <- liftIO $ execStateT (M.blocks2SMT sym discInfo) ms0

  -- Build disjointness constraints and variable mapping constraints,
  -- which will be used as pre-conditions
  disCons <- liftIO $  disjointConstraints sym ls' ms'
  addProofPreConds sym disCons

  -- Check state equality
  stateEq sym ls' ms'
-}

runVCG :: VCGConfig sym ids -> VCGMonad sym ids () -> IO (PfState sym)
runVCG cfg action =
  let s = PfState { preConds = []
                  , goals = []
                  }
   in execStateT (runReaderT action cfg) s

data VCGArgs
   = VCGArgs { reoptYaml :: !(Maybe FilePath)
               -- ^ Location with
             , outputDir :: !(Maybe FilePath)
               -- ^ Where to write output.
             }

data VCGCommand
  = ShowHelp
  | RunVCG !VCGArgs

parseArgs :: [String] -> VCGArgs -> Except String VCGCommand
parseArgs cmdArgs args = seq args $
  case cmdArgs of
    [] -> pure $! RunVCG args
    ("--help":_) -> pure $! ShowHelp
    ("--output":path:rest) -> do
      when (isJust (outputDir args)) $ do
        throwError $ "Output directory defined multiple times."
      parseArgs rest $ args { outputDir = Just path }
    (path:rest) -> do
      when ("--" `isPrefixOf` path) $ do
        throwError $ "Unexpected flag " ++ path
      when (isJust (reoptYaml args)) $ do
        throwError $ "Multiple VCG files specified."
      parseArgs rest $ args { reoptYaml = Just path }

showHelp :: IO ()
showHelp = do
  putStrLn
    $  "reopt-vcg generates verification conditions to prove that reopt generated\n"
    ++ "   LLVM is faithful to the input binary.\n"
    ++ "Usage: reopt-vcg <input.yaml> --output <out-dir>"

showError :: String -> IO a
showError msg = do
  hPutStrLn stderr $ "Error: " ++ msg
  hPutStrLn stderr $ "Run `reopt-vcg --help` for additional information."
  exitFailure

parseVCGArgs :: IO (MetaVCGConfig, FilePath)
parseVCGArgs = do
  cmdArgs <- getArgs
  let initVCG = VCGArgs { reoptYaml = Nothing, outputDir = Nothing }
  args <-
    case runExcept (parseArgs cmdArgs initVCG) of
      Left msg ->
        showError msg
      Right ShowHelp -> do
        showHelp
        exitSuccess
      Right (RunVCG a) -> pure a
  cfg <- do
    -- Get path to YAML
    vcgPath <-
      case reoptYaml args of
        Nothing -> showError "Missing VCG file to run."
        Just path -> return path
    vcgResult <- Yaml.decodeFileWithWarnings vcgPath
    case vcgResult of
      Left err -> do
        hPutStrLn stderr $ "Error parsing Yaml: " ++ show err
        exitFailure
      Right (warnings, cfg) -> do
        when (not (null warnings)) $ do
          hPutStrLn stderr $ "Warnings when parsing Yaml file:"
          forM_ warnings $ \warn -> do
            hPutStrLn stderr $ "  " ++ show warn
          exitFailure
        pure cfg
  let out =
        case outputDir args of
          Just d  -> d
          Nothing -> replaceExtension (llvmBCFilePath cfg) "vcg"
  r <- try $ createDirectoryIfMissing True out
  case r of
    Right () -> do
      putStrLn $ "Writing output to " ++ out
    Left e -> do
      hPutStrLn stderr $ "Error creating output directory: " ++ out
      hPutStrLn stderr $ "  " ++ show (e :: IOError)
      exitFailure
  pure (cfg, out)


ppBlock :: BlockLabel -> String
ppBlock (Named (Ident s)) = s
ppBlock (Anon i) = show i

{-
verifyBlock :: IO ()
verifyBlock = do
-}

verifyFunction :: Module
               -> DiscoveryState X86_64
               -> Map BSC.ByteString (MemSegmentOff 64)
                  -- ^ Maps symbol names to addresses
                  --
                  -- Used so user-generated verification files can refer to names rather than addresses.
               -> FilePath
               -> VCGFunInfo
               -> IO ()
verifyFunction lMod discState funMap outDir vfi = do
  let mem = memory discState
  addr <-
    case Map.lookup (BSC.pack (macawFunName vfi)) funMap of
      Just addr ->
        pure addr
      Nothing ->
        fatalError
            $  "Unknown Macaw function: " ++ macawFunName vfi ++ "\n"
            ++ "Available functions:\n"
            ++ unlines ((\x -> "  " ++ BSC.unpack x) <$> Map.keys funMap)

  hPutStrLn stderr $ "Analyzing " ++ macawFunName vfi

  let Just lFun = L.getDefineByName lMod (llvmFunName vfi)

  (firstBlock, restBlocks) <-
    case defBody lFun of
      [] -> error $ "Expected function to have at least one basic block."
      f:r -> pure (f,r)


  Some gen <- newIONonceGenerator
  sym <- newSimpleBackend gen

  llvmVals <- forM (defArgs lFun) $ \(Typed tp (Ident arg)) -> do
    when (tp /= PrimType (Integer 64)) $ do
      error $ "Unexpected type " ++ show tp
    freshConstant sym (newUserSymbol ("arg" ++ arg)) bv64

  when (length (defArgs lFun) > length x86ArgRegs) $ do
    error $ "Too many arguments."

  Some stGen <- liftIO $ stToIO $ newSTNonceGenerator

  ls0 <- L.inject sym $ zip (typedValue <$> defArgs lFun) (Some <$> llvmVals)
  ms0 <- M.inject sym


  let assertEq lval reg = do
        let M.MSymExpr mval = M.initRegs ms0^.boundValue reg
        bvEq sym lval mval
  eqConds <- zipWithM assertEq llvmVals x86ArgRegs

  let vcgCfg =  VCGConfig { blockMapping = Map.empty
                          , llvmMod = lMod
                          , exprBuilder = sym
                          , symIsBuilder = \x -> x
                          }
  pfSt <- runVCG vcgCfg $ do
    let bb = firstBlock

    liftIO $ putStrLn "Simulating LLVM program ..."
    ls' <- liftIO $ execStateT (L.bb2SMT sym bb) ls0



    let initAbsState = M.mkInitialAbsState x86_64_linux_info mem addr
    (blocks,_cnt, _merr) <- liftIO $ stToIO $
      M.disassembleFn x86_64_linux_info stGen addr maxBound initAbsState

    let blockMap = Map.fromList [ (M.blockLabel b, b) | b <- blocks ]
    let Just macawParsedBlock = Map.lookup 0 blockMap

    liftIO $ putStrLn "Simulating Macaw program ..."
    ms' <- liftIO $ execStateT (M.block2SMT blockMap macawParsedBlock) ms0

    -- Build disjointness constraints and variable mapping constraints,
    -- which will be used as pre-conditions
    disCons <- liftIO $  disjointConstraints sym ls' ms'
    addProofPreConds sym disCons

    -- Check state equality
    stateEq sym ls' ms'




  -- Build preconditions and proof goals
  let t = truePred sym
  conjPreConds <- foldM (andPred sym) t (preConds pfSt ++ eqConds)
  conjGoals <- foldM (andPred sym) t (goals pfSt)
  negConjGoals <- notPred sym conjGoals

  -- write proof to smt file
  putStrLn $ "Writing SMT formulas to " ++ outDir
  let Just lbl = bbLabel firstBlock
  let fname = llvmFunName vfi ++ "_" ++ ppBlock lbl ++ ".smt"
  bracket (openFile (outDir </> fname) ReadWriteMode) hClose $ \h -> do
    writeMultiAsmpCVC4SMT2File sym h [conjPreConds, negConjGoals]


  forM_ restBlocks $ \_bb -> do
    pure ()


main :: IO ()
main = do
  (metaCfg, outDir) <- parseVCGArgs
  putStrLn $ show metaCfg
  mDiscState <- M.getDiscState $ binFilePath metaCfg
  lMod <- L.getLLVMMod (llvmBCFilePath metaCfg)
  let funMap = Map.fromList [ (sym, addr) | (addr,sym) <- Map.toList $ symbolNames mDiscState ]
  forM_ (functions metaCfg) $ \vfi -> do
    verifyFunction lMod mDiscState funMap outDir vfi