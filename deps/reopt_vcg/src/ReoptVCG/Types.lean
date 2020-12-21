import Galois.Data.ByteArray
import LeanLLVM.AST
import LeanLLVM.PP
import ReoptVCG.Elf
import ReoptVCG.Annotations

import SmtLib.Smt
import DecodeX86.DecodeX86

-- TODO move these (or similar fns) to lean-llvm
def LLVM.Ident.pp : LLVM.Ident → String := LLVM.Doc.render ∘ LLVM.HasPP.pp
def LLVM.LLVMType.pp : LLVM.LLVMType → String := LLVM.Doc.render ∘ LLVM.HasPP.pp
def LLVM.BlockLabel.pp : LLVM.BlockLabel → String := LLVM.Doc.render ∘ LLVM.HasPP.pp

open Smt (SmtSort SmtSort.bv64 SmtSort.bool SmtSort.bitvec SmtSort.array Term SmtM Command)



-------------------------------------------------------
-- Alloca Terms
-------------------------------------------------------
structure AllocaLlvm :=
(baseAddress : Smt.Term SmtSort.bv64)
-- ^ LLVM alloca base.
(endAddress : Smt.Term SmtSort.bv64)
-- ^ LLVM alloca end/
(returnRegister  : Smt.Term SmtSort.bv64)
-- ^ Register alloca is returned to.
(isInAlloca : Smt.Term SmtSort.bv64 → Smt.Term SmtSort.bv64 → Smt.Term  SmtSort.bool)
-- ^ Predicate @isInAlloca start end @ checks if a range 
--   [start,end) is contained within the alloca.

structure AllocaMC :=
(baseAddress   : Smt.Term SmtSort.bv64)
-- ^ Alloca base.
(endAddress    : Smt.Term SmtSort.bv64)
-- ^ Alloca end.
(isInAlloca : Smt.Term SmtSort.bv64 → Smt.Term  SmtSort.bv64 → Smt.Term  SmtSort.bool)
-- ^ Predicate @isInAlloca start end @ checks if a range 
--   [start,end) is contained within the alloca.



-------------------------------------------------------
-- MC Types
-------------------------------------------------------

namespace x86
namespace vcg

open ReoptVCG

structure RegState : Type :=
  (gpregs : Array (Term (SmtSort.bitvec 64))) -- 16
  (flags  : Array (Term SmtSort.bool)) -- 32
  (ip     : (Term (SmtSort.bitvec 64)))



-- This mirrors the Haskell prototype as far as possible, hence the slightly verbose names.
inductive Event
  | Command : Smt.Command -> Event
  | Warning : String -> Event
    -- ^ We added a warning about an issue in the VCG
  | MCOnlyStackReadEvent : Term (SmtSort.bitvec 64) -> forall (n : Nat), Term (SmtSort.bitvec n) -> Event
    -- ^ `MCOnlyReadEvent a w v` indicates that we read `w` bytes
    -- from `a`, and assign the value returned to `v`.  This only
    -- appears in the binary code.
  | JointStackReadEvent : Term (SmtSort.bitvec 64) -> forall (n : Nat), Term (SmtSort.bitvec n) -> LocalIdent -> Event
    -- ^ `JointReadEvent a w v llvmAlloca` indicates that we read `w` bytes from `a`,
    -- and assign the value returned to `v`.  This appears in the both the binary
    -- and LLVM.  The alloca name refers to the LLVM allocation this is part of,
    -- and otherwise this is a binary only read.
  | NonStackReadEvent : Term (SmtSort.bitvec 64) -> forall (n : Nat), Term (SmtSort.bitvec n) -> Event
    -- ^ `NonStackReadEvent a w v` indicates that we read `w` bytes
    -- from `a`, and assign the value returned to `v`.  The address `a` should not
    -- be in the stack.
  | MCOnlyStackWriteEvent : Term (SmtSort.bitvec 64) -> forall (n : Nat), Term (SmtSort.bitvec n) -> Event
    -- ^ `MCOnlyStackWriteEvent a tp v` indicates that we write the `w` byte value `v`  to `a`.
    --
    -- This has side effects, so we record the event.
  | JointStackWriteEvent : Term (SmtSort.bitvec 64) -> forall (n : Nat), Term (SmtSort.bitvec n) -> LocalIdent -> Event
    -- ^ `JointStackWriteEvent a w v` indicates that we write the `w` byte value `v`  to `a`.
    -- The write affects the alloca pointed to by Allocaname.
    --
    -- This has side effects, so we record the event.
  | NonStackWriteEvent : Term (SmtSort.bitvec 64) -> forall (n : Nat), Term (SmtSort.bitvec n) -> Event
    -- ^ `NonStackWriteEvent a w v` indicates that we write the `w` byte value `v`  to `a`.  The
    -- address `a` should not be in the stack.
    --
    -- This has side effects, so we record the event.
  | FetchAndExecuteEvent : RegState -> Event
    -- ^ A fetch and execute


abbrev memory_t := SmtSort.array (SmtSort.bitvec 64) (SmtSort.bitvec 8)
def memory := Term memory_t



structure SupportedMemType (s : SmtSort) :=
  (readMem  : memory -> Term (SmtSort.bitvec 64) -> Smt.Term s)
  (writeMem : memory -> Term (SmtSort.bitvec 64) -> Smt.Term s -> memory)


-- FIXME: the name is wrong, maybe something like MCSMTContext or something?
-- cf. `mcMemDecls`
structure MCStdLib :=
  (memOps        : forall (w : WordSize), SupportedMemType w.sort)
  (funStartRegs  : RegState)
  (blockStartMem : memory)
  (onStack       : Term (SmtSort.bitvec 64) -> Term (SmtSort.bitvec 64) -> Term SmtSort.bool)
  (allocaMap     : Std.RBMap LocalIdent AllocaMC (λ x y => x < y))
  (notInStack    : Term (SmtSort.bitvec 64) -> Term (SmtSort.bitvec 64) -> Term SmtSort.bool)
  (isInMCOnlyStackRange : Term (SmtSort.bitvec 64) -> Term (SmtSort.bitvec 64) -> Term SmtSort.bool)


end vcg
end x86

namespace LLVM

namespace BlockLabel

def lt : forall (x y : BlockLabel), Prop
  | { label := x }, {label := y } => x < y

instance : Less BlockLabel := ⟨lt⟩
 
instance decideableBlockLabelLt : ∀(x y:BlockLabel), Decidable (x < y)
| { label := x }, { label := y } =>
  (match Ident.decideLt x y with
   | Decidable.isTrue  p => Decidable.isTrue p
   | Decidable.isFalse p => Decidable.isFalse p
   )

end BlockLabel
end LLVM

namespace ReoptVCG

-- FIXME double check on which interface/API/etc to use here =\
-- The Smt.Raw namespace feels like it has the direct equivalents
-- to the What4.Protocol.SMTLib2.Syntax module in Haskell, but...
-- it's the "Raw" interface which feels a little off...

open Smt


@[reducible]
def FnName := String


inductive VerificationMode
| defaultMode : VerificationMode
| exportMode : String /- path to write .smt2 files -/ → VerificationMode
| runSolverMode : String /- solver -/ → List String /- solver args -/ → VerificationMode

def VerificationMode.isDefault : VerificationMode → Bool
| VerificationMode.defaultMode => true
| _ => false


-- Like VCGArgs in Main but with all mandatory fields no longer as Options.
structure VCGConfig :=
(annFile : String)
(mode : VerificationMode)
(verbose : Bool)


abbrev MemAddr := Nat
abbrev MCBlockAnnMap := Std.RBMap MemAddr MemoryAnn (λ x y => x < y)

@[reducible]
def LLVMTypeMap := Std.RBMap String (Option LLVM.LLVMType) Lean.strLt


structure ModuleVCGContext :=
(annotations : ModuleAnnotations)
-- ^ Annotations for module.
(decoder : decodex86.decoder)
-- ^ Machine code memory / decoder state
(symbolAddrMap : Std.RBMap String (elf.word elf.elf_class.ELF64) Lean.strLt)
-- ^ Maps bytes to the symbol name
(moduleTypeMap : LLVMTypeMap)
-- ^ type map for module.


-------------------------------------------------------
-- Error/Exception Data
-------------------------------------------------------

def ppBlockLabel (lbl:LLVM.BlockLabel) : String :=
match lbl.label with
| LLVM.Ident.named str => str
| LLVM.Ident.anon n => "anon" ++ n.repr


-- Information describing where we were during block VCG when 
-- something happened (e.g., an error occured, a goal was
-- generated, etc).
structure BlockLocation :=
  (fnName : String)
  -- ^ Function whose blocks are being verified
  (blockLbl  : LLVM.BlockLabel)
  -- ^ Block being verified
  (llvmInstrIdx : Nat)
  -- ^ Index of focused LLVM instruction
  (mcAddr : Nat)
  -- ^ Machine code address

namespace BlockLocation

def pp (l : BlockLocation) : String :=
  l.fnName
  ++"."++(ppBlockLabel l.blockLbl)
  ++"."++l.llvmInstrIdx.repr
  ++" @ "++l.mcAddr.ppHex
  

end BlockLocation


inductive BlockErrorTag
  | blockAddrInvalid : BlockErrorTag
  | mcRanOutOfEvents : BlockErrorTag
  | mcInstrEventError : BlockErrorTag
  | unsupportedMemType : BlockErrorTag
  | mcEventAfterFetchAndExe : BlockErrorTag
  | mcEarlyEnvOfBlock : BlockErrorTag
  | unimplementedFeature : BlockErrorTag
  | unexpectedSort : BlockErrorTag
  | outOfFuel : BlockErrorTag
  | aliasCycleDetected : BlockErrorTag
  | llvmTypeNotFound : BlockErrorTag
  | llvmTypeNotBound : BlockErrorTag
  | invalidAlignment : BlockErrorTag
  | unknownAlloc : BlockErrorTag
  | unexpectedEvent : BlockErrorTag
  | llvmInvokeArgError : BlockErrorTag
  | invalidLLVMInstr : BlockErrorTag
  | llvmVarNoInitVal : BlockErrorTag
  | unexpectedPhiVar : BlockErrorTag
  | llvmInvalidLoad : BlockErrorTag
  | allocNameCollision : BlockErrorTag
  | llvmRanOutOfEvents : BlockErrorTag
  | llvmMissingReturn : BlockErrorTag
  | missingAnnotations : BlockErrorTag

namespace BlockErrorTag

def index : BlockErrorTag → Nat
  | blockAddrInvalid => 0
  | mcRanOutOfEvents => 1
  | mcInstrEventError => 2
  | unsupportedMemType => 3
  | mcEventAfterFetchAndExe => 4
  | mcEarlyEnvOfBlock => 5
  | unimplementedFeature => 6
  | unexpectedSort => 7
  | outOfFuel => 8
  | aliasCycleDetected => 9
  | llvmTypeNotFound => 10
  | llvmTypeNotBound => 11
  | invalidAlignment => 12
  | unknownAlloc => 13
  | unexpectedEvent => 14
  | llvmInvokeArgError => 15
  | invalidLLVMInstr => 16
  | llvmVarNoInitVal => 17
  | unexpectedPhiVar => 18
  | llvmInvalidLoad => 19
  | allocNameCollision => 20
  | llvmRanOutOfEvents => 21
  | llvmMissingReturn => 22
  | missingAnnotations => 23

def lt (x y : BlockErrorTag) : Bool :=
  x.index < y.index

def description : BlockErrorTag → String
| blockAddrInvalid =>
  "annotated block address invalid"
| mcRanOutOfEvents =>
  "unexpected end of machine code events"
| mcInstrEventError =>
  "error while computing machine code instruction events"
| unsupportedMemType =>
  "unsupported memory type"
| mcEventAfterFetchAndExe =>
  "machine code event after fetch and execute"
| mcEarlyEnvOfBlock =>
  "unexpected end of block"
| unimplementedFeature =>
  "unimplemented feature"
| unexpectedSort =>
  "unexpected sort"
| outOfFuel =>
  "ran of out fuel while performing computation"
| aliasCycleDetected =>
  "detected a cycle while resolving LLVM type alias"
| llvmTypeNotFound =>
  "LLVM type was not in the environment"
| llvmTypeNotBound =>
  "LLVM type had no associated binding in the environment"
| invalidAlignment =>
  "alignment is not a power of 2"
| unknownAlloc =>
  "unknown allocation"
| unexpectedEvent =>
  "unexpected event"
| llvmInvokeArgError =>
  "error with arguments while invoking LLVM function"
| invalidLLVMInstr =>
  "invalid LLVM instruction"
| llvmVarNoInitVal =>
  "no initial value for LLVM variable"
| unexpectedPhiVar =>
  "unexpected phi variable"
| llvmInvalidLoad =>
  "invalid LLVM load"
| allocNameCollision =>
  "name already use in an allocation"
| llvmRanOutOfEvents =>
  "unexpected end of LLVM events"
| llvmMissingReturn =>
  "missing an LLVM return"
| missingAnnotations =>
  "block missing annotation"

end BlockErrorTag

structure BlockError :=
(loc    : BlockLocation)
-- ^ Location during VCG the error occurred.
(tag  : BlockErrorTag)
-- ^ Unique tag for the error.
(extraInfo : String)
-- ^ Extra parenthetical info to accompany the property's description if desired
  

namespace BlockError

def pp (e : BlockError) : String :=
  let extra : String := if e.extraInfo == "" then "" else " ("++e.extraInfo++")"
  e.loc.pp++": "++e.tag.description++extra

end BlockError

-- Information describing where we were during module VCG
-- something happened (but not within a block where more
-- precise location information is available.
structure ModuleLocation :=
  (fnName : Option String)
  -- ^ Function being examined (if applicable)
  (blockLbl  : Option LLVM.BlockLabel)
  -- ^ Block being examined (if applicable)

namespace ModuleLocation

def pp (loc : ModuleLocation) : String :=
  match loc.fnName, loc.blockLbl with
  | some nm, some lbl => "function " ++ nm ++ " block " ++ ppBlockLabel lbl
  | some nm, none => "function " ++ nm
  | none, some lbl => "block " ++ ppBlockLabel lbl
  | none, none => "unknown location"

end ModuleLocation


inductive ModuleErrorTag
  | fnNotFound : ModuleErrorTag
  | annParseFailure : ModuleErrorTag
  | argTypeUnsupported : ModuleErrorTag
  | missingEntryBlock : ModuleErrorTag
  | entryUnreachable : ModuleErrorTag
  | unsupportedPhiVarType : ModuleErrorTag
  | blockMissingAnnotations : ModuleErrorTag
  | maxFnArgCntSurpassed : ModuleErrorTag
  | fnAnnAddrWrong : ModuleErrorTag
  | fatal : ModuleErrorTag

namespace ModuleErrorTag

def index : ModuleErrorTag → Nat
  | fnNotFound => 0
  | annParseFailure => 1
  | argTypeUnsupported => 2
  | missingEntryBlock => 3
  | entryUnreachable => 4
  | unsupportedPhiVarType => 5
  | blockMissingAnnotations => 6
  | maxFnArgCntSurpassed => 7
  | fnAnnAddrWrong => 8
  | fatal => 9

def lt (x y : ModuleErrorTag) : Bool :=
  x.index < y.index

def description : ModuleErrorTag → String
  | fnNotFound => 
    "could not find function in LLVM module"
  | annParseFailure =>
    "annotation parse failure"
  | argTypeUnsupported => 
    "argument has unsupported type"
  | missingEntryBlock => 
    "function body missing an entry block"
  | entryUnreachable =>
    "function entry marked unreachable"
  | unsupportedPhiVarType =>
    "unsupported phi variable type"
  | blockMissingAnnotations =>
    "block missing annotations"
  | maxFnArgCntSurpassed => 
    "LLVM function given more arguments than are supported"
  | fnAnnAddrWrong =>
    "function address in annotation does not match address in symbol table"
  | fatal =>
    "fatal error encountered"

end ModuleErrorTag

structure ModuleError :=
  (loc : ModuleLocation)
  -- ^ Where were we when the error occured
  (tag : ModuleErrorTag)
  -- ^ What kind of error was it
  (extraInfo : String)
  -- ^ Is there some additional info worth reporting? `""` if no.

namespace ModuleError

def pp (e : ModuleError) : String :=
  let extra : String := if e.extraInfo == "" then "" else " ("++e.extraInfo++")"
  e.loc.pp++": "++e.tag.description++extra

end ModuleError



-----------------------------------------------------------
-- Verification Data
--
-- I.e., the data structures produced during VCG which
-- describe what SMT queries are necessary to prove things
-- are good-to-go.
-----------------------------------------------------------

-- Correctness properties as a data type (makes categorizing/reporting/etc easier)
-- See Property.lean for inductive Property
inductive GoalTag
  | mcOnlyReadFromUnallocStack : GoalTag
  | mcOnlyWriteToUnreservedStack : GoalTag
  | llvmWriteTargetsAlloc : GoalTag
  | mcWriteTargetsAlloc : GoalTag
  | nonStackReadAddrValid : GoalTag
  | blockUnreachable : GoalTag
  | expectedInstrPtrVal : GoalTag
  | blockPrecondition : GoalTag
  | llvmAndMCReadOffsetsMatch : GoalTag
  | llvmAndMCLoadAddrsMatch : GoalTag
  | llvmAndMCStoreAddrsMatch : GoalTag
  | llvmAndMCStoreEq : GoalTag
  | stackHeightPreserved : GoalTag
  | returnAddressPreserved : GoalTag
  | registerPreserved : GoalTag
  | returnAddrNextInstr : GoalTag
  | llvmAndMCReturnValuesEq : GoalTag
  | argAndRegEq : GoalTag

namespace GoalTag

def index : GoalTag → UInt32
  | mcOnlyReadFromUnallocStack => 0
  | mcOnlyWriteToUnreservedStack => 1
  | llvmWriteTargetsAlloc => 2
  | mcWriteTargetsAlloc => 3
  | nonStackReadAddrValid => 4
  | blockUnreachable => 5
  | expectedInstrPtrVal => 6
  | blockPrecondition => 7
  | llvmAndMCReadOffsetsMatch => 8
  | llvmAndMCLoadAddrsMatch => 9
  | llvmAndMCStoreAddrsMatch => 10
  | llvmAndMCStoreEq => 11
  | stackHeightPreserved => 12
  | returnAddressPreserved => 13
  | registerPreserved => 14
  | returnAddrNextInstr => 15
  | llvmAndMCReturnValuesEq => 16
  | argAndRegEq => 17

def lt (x y : GoalTag) : Bool := x.index < y.index

def description : GoalTag → String
  | mcOnlyReadFromUnallocStack =>
    "machine code reads from unallocated stack space"
  | mcOnlyWriteToUnreservedStack =>
    "machine code writes to unreserve stack space"
  | llvmWriteTargetsAlloc =>
    "LLVM write targets intended allocation"
  | mcWriteTargetsAlloc =>
    "machine code write targets intended allocation"
  | nonStackReadAddrValid =>
    "read heap address not in stack space"
  | blockUnreachable =>
    "block is unreachable"
  | expectedInstrPtrVal =>
    "instruction pointer is expected value"
  | blockPrecondition =>
    "precondition"
  | llvmAndMCReadOffsetsMatch =>
    "LLVM and machine code read from same allocation offset"
  | llvmAndMCLoadAddrsMatch =>
    "LLVM and machine code load from same heap address"
  | llvmAndMCStoreAddrsMatch =>
    "LLVM and machine code store to same heap address"
  | llvmAndMCStoreEq =>
    "LLVM and machine code store the same value to the heap"
  | stackHeightPreserved =>
    "stack height preserved"
  | returnAddressPreserved =>
    "return address preserved"
  | registerPreserved =>
    "register value preserved"
  | returnAddrNextInstr =>
    "return address is next instruction"
  | llvmAndMCReturnValuesEq =>
    "LLVM and machine code return values match"
  | argAndRegEq =>
    "argument matches register"

end GoalTag

-- A verification goal for a block.
structure VerificationGoal :=
(loc    : BlockLocation)
-- ^ Location info at which we check this goal.
(index : Nat)
-- ^ Index of the goal for this block.
(tag  : GoalTag)
-- ^ Unique tag for the proposition.
(extraInfo : String)
-- ^ Extra parenthetical info to accompany the property's description if desired 
--   ("" means no such info is relevant)
(goal   : SmtM (Term SmtSort.bool))
-- ^ SMT script which, if unsat, proves the goal


def VerificationGoal.fullDescription (vg : VerificationGoal) : String :=
  vg.loc.pp++": "++vg.tag.description
  ++(if vg.extraInfo == ""
     then ""
     else " ("++vg.extraInfo++")")

-- | Log messages interleaved with verification
-- goal generation for humans.
structure VerificationMsg :=
(msg : String)

-- | The sequential events that are generated during
-- | verification (e.g., SMT queries, logging info, etc)
inductive VerificationEvent
| goal : VerificationGoal → VerificationEvent
| msg  : VerificationMsg → VerificationEvent

-- | Describes the conditions which would verify this block.
structure BlockVerification :=
(llvmFunName : String)
(blockLbl : LLVM.BlockLabel)
(blockVerificationEvents : List VerificationEvent)


-------------------------------------------------------
-- ModuleVCG (monad and some basic helpers)
-------------------------------------------------------

-- | Describes the result of verifying a block.
inductive BlockVerificationEvent
| block  : BlockVerification → BlockVerificationEvent
| error  : BlockError → BlockVerificationEvent


inductive FnVerificationEvent
| fn : FnName → List BlockVerificationEvent → FnVerificationEvent
| error : ModuleError → FnVerificationEvent

structure ModuleVCGState :=
  (blockErrors  : List BlockError)
  (moduleErrors : List ModuleError)

-- A monad for running verification of an entire module
-- TODO / FIXME we'll want to move away from EIO at, see
-- https://github.com/GaloisInc/reopt-vcg/pull/53#discussion_r408440682 
@[reducible]
def ModuleVCG := ReaderT ModuleVCGContext (EStateM ModuleError ModuleVCGState)

namespace ModuleVCG

instance : Functor ModuleVCG := 
  inferInstanceAs (Functor (ReaderT ModuleVCGContext (EStateM ModuleError ModuleVCGState)))
instance : Applicative ModuleVCG :=
  inferInstanceAs (Applicative (ReaderT ModuleVCGContext (EStateM ModuleError ModuleVCGState)))
instance : Monad ModuleVCG :=
  inferInstanceAs (Monad (ReaderT ModuleVCGContext (EStateM ModuleError ModuleVCGState)))

-- Run "standard" IO by wrapping any exceptions thrown in our Module.Error.IO wrapper.
-- def liftIO {α} (m : IO α) : ModuleVCG α := 
--   monadLift $ m.adaptExcept ModuleError.io

-- instance : HasMonadLiftT IO ModuleVCG := {monadLift := @ModuleVCG.liftIO}
instance : MonadReader ModuleVCGContext ModuleVCG :=
  inferInstanceAs (MonadReader ModuleVCGContext (ReaderT ModuleVCGContext (EStateM ModuleError ModuleVCGState)))

-- def throwIO {α} (err : IO.Error) : ModuleVCG α := throw $ ModuleError.io err
-- def catchIO {α} (m : ModuleVCG α) (h : IO.Error → ModuleVCG α) : ModuleVCG α := 
-- let handler : ModuleError → ModuleVCG α := 
--   λ e => match e with
--          | ModuleError.io e => h e
--          | _ => throw e;
-- catch m handler

-- instance : MonadExcept IO.Error ModuleVCG :=
--   {throw := @ModuleVCG.throwIO,
--    catch := @ModuleVCG.catchIO }


-- instance : MonadIO ModuleVCG :=
--   {throw := @ModuleVCG.throwIO,
--    catch := @ModuleVCG.catchIO,
--    monadLift := @ModuleVCG.liftIO}

-- instance : MonadIO ModuleVCG :=
-- inferInstanceAs (MonadIO (ReaderT ModuleVCGContext (EStateM ModuleError ModuleVCGState)))

end ModuleVCG


def runModuleVCG {α} (ctx : ModuleVCGContext) (m : ModuleVCG α) : Except ModuleError (α × ModuleVCGState) :=
  let initState : ModuleVCGState := {blockErrors := [], moduleErrors := []}
  match (m.run ctx).run initState with
  | EStateM.Result.ok v finalState => Except.ok (v, finalState)
  | EStateM.Result.error e _ => Except.error e

-- A warning that stops execution until catch.
def moduleThrow {α} (loc : ModuleLocation) (tag : ModuleErrorTag) (extraInfo : String) : ModuleVCG α :=
  throw $ {loc := loc, tag := tag, extraInfo := extraInfo}


-------------------------------------------------------
-- Annotated Block
-------------------------------------------------------

@[reducible]
def BlockLabelValMap := Std.RBMap LLVM.BlockLabel LLVM.Value (λ x y => x < y)

abbrev PhiVarMap := Std.RBMap LLVM.Ident (LLVM.LLVMType × BlockLabelValMap) (λ x y => x<y)

structure AnnotatedBlock :=
(annotation: BlockAnn)
(label : LLVM.BlockLabel)
(phiVarMap : PhiVarMap)
(stmts : List LLVM.Stmt)


/-  Maps LLM block labels to their associated annotations. -/
@[reducible]
def ReachableBlockAnnMap := Std.RBMap LLVM.BlockLabel AnnotatedBlock (λ x y => x<y)

-- | Find a block with the given label in the config.
def findBlock (m : ReachableBlockAnnMap) (lbl: LLVM.BlockLabel) : Option (BlockAnn × PhiVarMap) := do
  let ab ← m.find? lbl;
  pure (ab.annotation, ab.phiVarMap)

-------------------------------------------------------
-- BlockVCG
-------------------------------------------------------

-- Information that does not change during execution of a BlockVCG action.
structure BlockVCGContext :=
(mcModuleVCGContext : ModuleVCGContext)
  -- ^ Information at module level about CFG.
(llvmFunName : String)
  -- ^ Annotations for the current function.
(funBlkAnnotations : ReachableBlockAnnMap)
  -- ^ Annotations for blocks in the CFG.
(firstBlockLabel : LLVM.BlockLabel)
  -- ^ Label for first block in this function
(currentBlock : LLVM.BlockLabel)
  -- ^ Label for block we are verifying.
(mcBlockEndAddr : MemAddr)
  -- ^ The end address of the block.
(mcBlockMap : MCBlockAnnMap)
  -- ^ Map from addresses to annotations of events on that address.
(mcStdLib     : x86.vcg.MCStdLib)
-- ^ Machine-code specific declarations.

-- State that changes during execution of a BlockVCG action.
structure BlockVCGState :=
(mcCurAddr : MemAddr)
  -- ^ Address of the current instruction
(mcCurSize : Nat)
  -- ^ Size of current instruction.
--(mcX87Top : Nat) -- TODO...? later
  -- ^ Top index in x86 stack (starts at 7 and grows down).
-- (mcDF : Bool) -- FIXME
  -- ^ Direction flag
(mcCurRegs : x86.vcg.RegState)
  -- ^ Map registers to the SMT term.
(mcCurMem : x86.vcg.memory)
  -- ^ Current memory object
(mcEvents : List x86.vcg.Event)
  -- ^ Unprocessed events from last instruction.
(idGen : IdGen)
  -- ^ Used to generate unique/fresh local variables for machine code SMT terms.
(mcPendingAllocaOffsetMap : Std.RBMap LocalIdent AllocaAnn (λ x y => x < y))
  -- ^ This is a map from allocation names to the annotations about their
  -- size and offset.
(activeAllocaMap : Std.RBMap LocalIdent AllocaLlvm (fun x y => x < y))
 -- ^ Allocation names that are active in this block and their associated LLVM terms.
(llvmInstrIndex : Nat)
  -- ^ Index of next LLVM instruction within block to execute
  -- Used for error reporting
(llvmIdentMap : Std.RBMap LLVM.Ident (Sigma Smt.Term) (fun x y => x < y))
 -- ^ Mapping from llvm ident to their SMT equivalent.
(smtContext : SmtM Unit)
-- ^ Logical context defining the block.
(goalIndex : Nat)
-- ^ Counter for goals in a block.
(verificationEvents : List VerificationEvent)
-- ^ SMT scripts that end in a check-sat-assuming for the
--   goals necessary for verifying a block as well as log
--   messages and the like for human benefit. TODO: each of
--   the scripts will essentially share a "prelude"
--   defining the block, and then have their own ending
--   sequence of commands to verify the thing... perhaps it
--   is desirable to share that "prelude" explicitly?
--   Although that might make reasoning about each goal more
--   complicated... ¯\_(ツ)_/¯


inductive BlockVCGError
| localErr : BlockError → BlockVCGError
-- ^ The was an error processing the current block which,
--   has halted its verification, but it is reasonable to
--   continue on with the next block's verification.
| globalErr : String → BlockVCGError
-- ^ There is a globally fatal error; it is not reasonable to continue
-- verifying blocks.


def BlockVCG := ReaderT BlockVCGContext (EStateM BlockVCGError BlockVCGState)

namespace BlockVCG

instance : Monad BlockVCG :=
  inferInstanceAs (Monad (ReaderT BlockVCGContext (EStateM BlockVCGError BlockVCGState)))

instance : MonadReader BlockVCGContext BlockVCG :=
  inferInstanceAs (MonadReader BlockVCGContext (ReaderT BlockVCGContext (EStateM BlockVCGError BlockVCGState)))

-- instance : MonadState BlockVCGState BlockVCG :=
--   inferInstanceAs (MonadState BlockVCGState (ReaderT BlockVCGContext (EStateM BlockVCGError BlockVCGState)))

instance : MonadStateOf BlockVCGState BlockVCG :=
  inferInstanceAs (MonadStateOf BlockVCGState (ReaderT BlockVCGContext (EStateM BlockVCGError BlockVCGState)))


instance : MonadExcept BlockVCGError BlockVCG :=
  inferInstanceAs (MonadExcept BlockVCGError (ReaderT BlockVCGContext (EStateM BlockVCGError BlockVCGState)))


-- | Thow an error to terminate the current block's verification, but continue with
-- other blocks verification.
def localThrow {a} (e : BlockError) : BlockVCG a := throw $ BlockVCGError.localErr e

-- | Thow an error to terminate all verification.
def fatalThrow {a} (msg : String) : BlockVCG a := throw $ BlockVCGError.globalErr msg


end BlockVCG

-- FIXME: move
/- Lift an Except to IO, throwing any occurring error with the given prefix at the front of the message. -/
def elseThrowPrefixed {ε α : Type} [ToString ε] (e : Except ε α) (pfx : String) : IO α :=
  match e with
  | Except.ok a    => pure a
  | Except.error e => throw (IO.userError $ pfx ++ (toString e))



/- Maps between LLVM argument and machine code name. -/
structure LLVMMCArgBinding :=
(llvmArgName : LLVM.Ident)
(smtSort: Smt.SmtSort)
(register: x86.reg64)


end ReoptVCG
