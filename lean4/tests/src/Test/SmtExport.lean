

import ReoptVCG.Annotations
import ReoptVCG.SmtParser
import ReoptVCG.Smt
import SmtLib.Smt


namespace Test

namespace SmtExport

open ReoptVCG


def smtGoal1 : Smt.SmtM (Smt.Term SmtSort.bool) := do
tt ← Smt.declareFun "tt" [] SmtSort.bool;
tt0 ← Smt.declareFun "tt" [] SmtSort.bool;
tt2 ← Smt.nameTerm "tt2" Smt.true;
tt2_0 ← Smt.nameTerm "tt2_0" Smt.true;
tt2_1 ← Smt.nameTerm "tt2" Smt.true;
tt2_2 ← Smt.nameTerm "tt2" Smt.true;
tt2_0_0 ← Smt.nameTerm "tt2_0" Smt.true;
ff ← Smt.declareFun "ff" [] SmtSort.bool;
negb ← Smt.declareFun "negb" [SmtSort.bool] SmtSort.bool;
andb ← Smt.defineFun "andb" [SmtSort.bool, SmtSort.bool] SmtSort.bool (λ x y => Smt.and x y);
Smt.assert $ Smt.eq tt Smt.true;
Smt.assert $ Smt.eq tt tt2;
Smt.assert $ Smt.eq ff Smt.false;
Smt.assert $ Smt.eq (negb ff) Smt.true;
Smt.assert $ Smt.eq (negb tt) Smt.false;
Smt.assert $ andb tt (negb ff);
pure Smt.false

def proverAction1 (p : ProverInterface) : IO Unit :=
p.checkSatAssuming "false-is-false\n(true-is-true?)" smtGoal1

def testExportCallbacks : IO Unit := do
let outDir := ".";
let outFnNm := "foo";
let blockLbl := LLVM.BlockLabel.mk $ LLVM.Ident.named "bar";
exportCallbacks outDir outFnNm blockLbl proverAction1;
let outFile := outDir ++ [System.FilePath.pathSeparator].asString ++ (standaloneGoalFilename outFnNm blockLbl 0);
outFileContents ← IO.FS.readFile outFile;
IO.println outFileContents

def test : IO UInt32 := do
testExportCallbacks;
pure 0

end SmtExport

end Test