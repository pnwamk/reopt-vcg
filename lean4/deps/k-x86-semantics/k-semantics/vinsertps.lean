def vinsertps : instruction :=
  definst "vinsertps" $ do
    pattern fun (imm_0 : imm int) (mem_1 : Mem) (xmm_2 : reg (bv 128)) (xmm_3 : reg (bv 128)) => do
      v_4 <- eval (handleImmediateWithSignExtend imm_0 8 8);
      v_5 <- eval (extract v_4 2 4);
      v_6 <- eval (eq v_5 (expression.bv_nat 2 0));
      v_7 <- getRegister (lhs.of_reg xmm_2);
      v_8 <- eval (extract v_7 0 32);
      v_9 <- eval (eq v_5 (expression.bv_nat 2 1));
      v_10 <- eval (eq v_5 (expression.bv_nat 2 2));
      v_11 <- evaluateAddress mem_1;
      v_12 <- load v_11 4;
      v_13 <- eval (extract v_7 32 64);
      v_14 <- eval (extract v_7 64 96);
      setRegister (lhs.of_reg xmm_3) (concat (concat (concat (mux (isBitSet v_4 4) (expression.bv_nat 32 0) (mux v_6 v_8 (mux v_9 v_8 (mux v_10 v_8 v_12)))) (mux (isBitSet v_4 5) (expression.bv_nat 32 0) (mux v_6 v_13 (mux v_9 v_13 (mux v_10 v_12 v_13))))) (mux (isBitSet v_4 6) (expression.bv_nat 32 0) (mux v_6 v_14 (mux v_9 v_12 v_14)))) (mux (isBitSet v_4 7) (expression.bv_nat 32 0) (mux v_6 v_12 (extract v_7 96 128))));
      pure ()
    pat_end;
    pattern fun (imm_0 : imm int) (xmm_1 : reg (bv 128)) (xmm_2 : reg (bv 128)) (xmm_3 : reg (bv 128)) => do
      v_4 <- eval (handleImmediateWithSignExtend imm_0 8 8);
      v_5 <- eval (extract v_4 2 4);
      v_6 <- eval (eq v_5 (expression.bv_nat 2 0));
      v_7 <- getRegister (lhs.of_reg xmm_2);
      v_8 <- eval (extract v_7 0 32);
      v_9 <- eval (eq v_5 (expression.bv_nat 2 1));
      v_10 <- eval (eq v_5 (expression.bv_nat 2 2));
      v_11 <- eval (extract v_4 0 2);
      v_12 <- getRegister (lhs.of_reg xmm_1);
      v_13 <- eval (mux (eq v_11 (expression.bv_nat 2 0)) (extract v_12 96 128) (mux (eq v_11 (expression.bv_nat 2 1)) (extract v_12 64 96) (mux (eq v_11 (expression.bv_nat 2 2)) (extract v_12 32 64) (extract v_12 0 32))));
      v_14 <- eval (extract v_7 32 64);
      v_15 <- eval (extract v_7 64 96);
      setRegister (lhs.of_reg xmm_3) (concat (concat (concat (mux (isBitSet v_4 4) (expression.bv_nat 32 0) (mux v_6 v_8 (mux v_9 v_8 (mux v_10 v_8 v_13)))) (mux (isBitSet v_4 5) (expression.bv_nat 32 0) (mux v_6 v_14 (mux v_9 v_14 (mux v_10 v_13 v_14))))) (mux (isBitSet v_4 6) (expression.bv_nat 32 0) (mux v_6 v_15 (mux v_9 v_13 v_15)))) (mux (isBitSet v_4 7) (expression.bv_nat 32 0) (mux v_6 v_13 (extract v_7 96 128))));
      pure ()
    pat_end
