import LeanLLVM.AST
import Main.Elf
import ReoptVCG.Annotations
import SMTLIB.Syntax


namespace ReoptVCG

-- FIXME double check on which interface/API/etc to use here =\
-- The SMTLIB.Raw namespace feels like it has the direct equivalents
-- to the What4.Protocol.SMTLib2.Syntax module in Haskell, but...
-- it's the "Raw" interface which feels a little off...
open SMTLIB.Raw 

inductive VerificationMode
| defaultMode : VerificationMode
| exportMode : String → VerificationMode
| runSolverMode : String → List String → VerificationMode

def isDefaultMode : VerificationMode → Bool
| VerificationMode.defaultMode => true
| _ => false


-- Like VCGArgs in Main but with all mandatory fields no longer as Options.
structure VCGConfig :=
(annFile : String)
(mode : VerificationMode)
(verbose : Bool)
          
def FunctionName := String
def BlockLabel := llvm.ident

structure ProverInterface :=
(addCommandCallback : command → IO Unit)
(proveFalseCallback : term const_sort.smt_bool → String → IO Unit)
(proveTrueCallback  : term const_sort.smt_bool → String → IO Unit)
-- (blockErrorCallback : Int → MemSegmentOff 64 → String → IO Unit)

structure ProverSessionGenerator :=
(blockCallback : FunctionName → BlockLabel → (ProverInterface → IO Unit) → IO Unit)
(sessionComplete : IO Unit)

def exportModeCallbacks {α : Type} (outDir : String) (fn : FunctionName) (lbl : BlockLabel) (action : ProverInterface → IO α) : IO α :=
-- FIXME
action $ ProverInterface.mk (λ _ => pure ()) (λ _ _ => pure ()) (λ _ _ => pure ())

-- This runs an action with a proof session generator, and reports
-- the final proof results.
def interactiveSMTGenerator (annPath solverPath : String) (solverArgs : List String) : IO ProverSessionGenerator :=
-- FIXME
pure $ ProverSessionGenerator.mk (λ _ _ _ => pure ()) (pure ())


-- | Lift an Except to IO, throwing any occurring error with the given prefix at the front of the message.
def elseThrowPrefixed {ε α : Type} [HasToString ε] (e : Except ε α) (pfx : String) : IO α :=
match e with
| Except.ok a    => pure a
| Except.error e => throw (IO.userError $ pfx ++ (toString e))

-- FIXME remove this comment: FYI was `withVCGArgs`
def setupWithConfig (cfg : VCGConfig) : IO (ModuleAnnotations × ProverSessionGenerator) := do
-- Read in the annotation file.
annContents ← IO.FS.readFile cfg.annFile;
modAnn ← elseThrowPrefixed (Lean.Json.parse annContents >>= parseAnnotations)
         $ "Encountered an error while parsing the Json in `"++ cfg.annFile ++"`: ";
when cfg.verbose $
  IO.println $ "Parsed the JSON annotation file `"++cfg.annFile++"` successfully!";
-- Dispatch on the user-requested mode to setup the prover sesstion generator.
match cfg.mode with
-- Default: just use cvc4 with default args.
| VerificationMode.defaultMode => do
  let args := ["--lang=smt2", "--dedicated-eqrange-quant", "--incremental"];
  psGen ← interactiveSMTGenerator cfg.annFile "cvc4" args;
  pure (modAnn, psGen)
-- Use the user-specified solver and args.
| VerificationMode.runSolverMode solverCmd solverArgs => do
  psGen ← interactiveSMTGenerator cfg.annFile "cvc4" solverArgs;
  pure (modAnn, psGen)
-- Output into the specified directory.
| VerificationMode.exportMode outDir => do
  outDirExists ← IO.isDir outDir;
  unless outDirExists $ throw $ IO.userError $ "Output directory `"++outDir++"` does not exists.";
  -- FIXME create the directory if it's missing? (It's not clear there's a lean4 API for that yet)
  let psGen := ProverSessionGenerator.mk (exportModeCallbacks outDir) (pure ());
  pure (modAnn, psGen)
  


def runVCG (cfg : VCGConfig) : IO UInt32 := do
(ann, gen) ← setupWithConfig cfg;
-- Load Elf file and emit warnings
(⟨elfHdr, prgmHdrs⟩, elfMem) ← elf.read_info_from_file ann.binFilePath;
-- Check the Elf file is for a x86_64
unless (elfHdr.machine == elf.machine.EM_X86_64) $
  throw $ IO.userError $ "Expected elf header machine type EM_X86_64 but got `"++ elfHdr.machine.name ++"`.";
-- Check that it's a linux binary
unless (elfHdr.info.osabi == elf.osabi.ELFOSABI_SYSV || elfHdr.info.osabi == elf.osabi.ELFOSABI_LINUX) $
  throw $ IO.userError $ "Expected Linux binary but got `"++ toString elfHdr.info.osabi.name ++"`.";
-- -- Get LLVM module
-- lMod <- getLLVMModule (Ann.llvmFilePath ann)
-- -- Create verification coontext for module.
-- errorRef <- newIORef 0
-- let modCtx = ModuleVCGContext { moduleAnn = ann
--                               , moduleMem = mem
--                               , symbolAddrMap = Map.fromList
--                                                 [ (memSymbolName sym, memSymbolStart sym)
--                                                 | sym <- symbols
--                                                 ]
--                               , writeStderr = True
--                               , errorCount = errorRef
--                               , proverGen = gen
--                               , moduleTypeMap = Map.fromList
--                                   [ (nm,tp)
--                                   | L.TypeDecl nm tp <- L.modTypes lMod
--                                   ]
--                               }
-- -- Run verification.
-- runModuleVCG modCtx $ do
--   forM_ (Ann.functions ann) $ \funAnn -> do
--     moduleCatch $ verifyFunction lMod funAnn
-- -- print out results
-- errorCnt <- readIORef errorRef
-- if errorCnt > 0 then do
--   hPutStrLn stderr "Errors during verification."
--   exitFailure
--  else
--   sessionComplete gen
pure 0
  

end ReoptVCG
