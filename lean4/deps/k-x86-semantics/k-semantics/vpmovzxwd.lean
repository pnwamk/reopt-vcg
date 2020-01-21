def vpmovzxwd : instruction :=
  definst "vpmovzxwd" $ do
    pattern fun (mem_0 : Mem) (xmm_1 : reg (bv 128)) => do
      v_2 <- evaluateAddress mem_0;
      v_3 <- load v_2 8;
      setRegister (lhs.of_reg xmm_1) (concat (expression.bv_nat 16 0) (concat (extract v_3 0 16) (concat (expression.bv_nat 16 0) (concat (extract v_3 16 32) (concat (expression.bv_nat 16 0) (concat (extract v_3 32 48) (concat (expression.bv_nat 16 0) (extract v_3 48 64))))))));
      pure ()
    pat_end;
    pattern fun (mem_0 : Mem) (ymm_1 : reg (bv 256)) => do
      v_2 <- evaluateAddress mem_0;
      v_3 <- load v_2 16;
      setRegister (lhs.of_reg ymm_1) (concat (expression.bv_nat 16 0) (concat (extract v_3 0 16) (concat (expression.bv_nat 16 0) (concat (extract v_3 16 32) (concat (expression.bv_nat 16 0) (concat (extract v_3 32 48) (concat (expression.bv_nat 16 0) (concat (extract v_3 48 64) (concat (expression.bv_nat 16 0) (concat (extract v_3 64 80) (concat (expression.bv_nat 16 0) (concat (extract v_3 80 96) (concat (expression.bv_nat 16 0) (concat (extract v_3 96 112) (concat (expression.bv_nat 16 0) (extract v_3 112 128))))))))))))))));
      pure ()
    pat_end;
    pattern fun (xmm_0 : reg (bv 128)) (xmm_1 : reg (bv 128)) => do
      v_2 <- getRegister (lhs.of_reg xmm_0);
      setRegister (lhs.of_reg xmm_1) (concat (expression.bv_nat 16 0) (concat (extract v_2 64 80) (concat (expression.bv_nat 16 0) (concat (extract v_2 80 96) (concat (expression.bv_nat 16 0) (concat (extract v_2 96 112) (concat (expression.bv_nat 16 0) (extract v_2 112 128))))))));
      pure ()
    pat_end;
    pattern fun (xmm_0 : reg (bv 128)) (ymm_1 : reg (bv 256)) => do
      v_2 <- getRegister (lhs.of_reg xmm_0);
      setRegister (lhs.of_reg ymm_1) (concat (expression.bv_nat 16 0) (concat (extract v_2 0 16) (concat (expression.bv_nat 16 0) (concat (extract v_2 16 32) (concat (expression.bv_nat 16 0) (concat (extract v_2 32 48) (concat (expression.bv_nat 16 0) (concat (extract v_2 48 64) (concat (expression.bv_nat 16 0) (concat (extract v_2 64 80) (concat (expression.bv_nat 16 0) (concat (extract v_2 80 96) (concat (expression.bv_nat 16 0) (concat (extract v_2 96 112) (concat (expression.bv_nat 16 0) (extract v_2 112 128))))))))))))))));
      pure ()
    pat_end
