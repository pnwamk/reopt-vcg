def psignd : instruction :=
  definst "psignd" $ do
    pattern fun (mem_0 : Mem) (xmm_1 : reg (bv 128)) => do
      v_2 <- evaluateAddress mem_0;
      v_3 <- load v_2 16;
      v_4 <- eval (extract v_3 0 32);
      v_5 <- getRegister (lhs.of_reg xmm_1);
      v_6 <- eval (extract v_5 0 32);
      v_7 <- eval (extract v_3 32 64);
      v_8 <- eval (extract v_5 32 64);
      v_9 <- eval (extract v_3 64 96);
      v_10 <- eval (extract v_5 64 96);
      v_11 <- eval (extract v_3 96 128);
      v_12 <- eval (extract v_5 96 128);
      setRegister (lhs.of_reg xmm_1) (concat (mux (sgt v_4 (expression.bv_nat 32 0)) v_6 (mux (eq v_4 (expression.bv_nat 32 0)) (expression.bv_nat 32 0) (add (expression.bv_nat 32 1) (bv_xor v_6 (expression.bv_nat 32 4294967295))))) (concat (mux (sgt v_7 (expression.bv_nat 32 0)) v_8 (mux (eq v_7 (expression.bv_nat 32 0)) (expression.bv_nat 32 0) (add (expression.bv_nat 32 1) (bv_xor v_8 (expression.bv_nat 32 4294967295))))) (concat (mux (sgt v_9 (expression.bv_nat 32 0)) v_10 (mux (eq v_9 (expression.bv_nat 32 0)) (expression.bv_nat 32 0) (add (expression.bv_nat 32 1) (bv_xor v_10 (expression.bv_nat 32 4294967295))))) (mux (sgt v_11 (expression.bv_nat 32 0)) v_12 (mux (eq v_11 (expression.bv_nat 32 0)) (expression.bv_nat 32 0) (add (expression.bv_nat 32 1) (bv_xor v_12 (expression.bv_nat 32 4294967295))))))));
      pure ()
    pat_end;
    pattern fun (xmm_0 : reg (bv 128)) (xmm_1 : reg (bv 128)) => do
      v_2 <- getRegister (lhs.of_reg xmm_0);
      v_3 <- eval (extract v_2 0 32);
      v_4 <- getRegister (lhs.of_reg xmm_1);
      v_5 <- eval (extract v_4 0 32);
      v_6 <- eval (extract v_2 32 64);
      v_7 <- eval (extract v_4 32 64);
      v_8 <- eval (extract v_2 64 96);
      v_9 <- eval (extract v_4 64 96);
      v_10 <- eval (extract v_2 96 128);
      v_11 <- eval (extract v_4 96 128);
      setRegister (lhs.of_reg xmm_1) (concat (mux (sgt v_3 (expression.bv_nat 32 0)) v_5 (mux (eq v_3 (expression.bv_nat 32 0)) (expression.bv_nat 32 0) (add (expression.bv_nat 32 1) (bv_xor v_5 (expression.bv_nat 32 4294967295))))) (concat (mux (sgt v_6 (expression.bv_nat 32 0)) v_7 (mux (eq v_6 (expression.bv_nat 32 0)) (expression.bv_nat 32 0) (add (expression.bv_nat 32 1) (bv_xor v_7 (expression.bv_nat 32 4294967295))))) (concat (mux (sgt v_8 (expression.bv_nat 32 0)) v_9 (mux (eq v_8 (expression.bv_nat 32 0)) (expression.bv_nat 32 0) (add (expression.bv_nat 32 1) (bv_xor v_9 (expression.bv_nat 32 4294967295))))) (mux (sgt v_10 (expression.bv_nat 32 0)) v_11 (mux (eq v_10 (expression.bv_nat 32 0)) (expression.bv_nat 32 0) (add (expression.bv_nat 32 1) (bv_xor v_11 (expression.bv_nat 32 4294967295))))))));
      pure ()
    pat_end
