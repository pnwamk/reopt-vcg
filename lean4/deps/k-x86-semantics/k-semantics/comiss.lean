def comiss : instruction :=
  definst "comiss" $ do
    pattern fun (mem_0 : Mem) (xmm_1 : reg (bv 128)) => do
      v_2 <- getRegister (lhs.of_reg xmm_1);
      v_3 <- evaluateAddress mem_0;
      v_4 <- load v_3 4;
      v_5 <- eval (_(_,_)_MINT-WRAPPER-SYNTAX comiss (extract v_2 96 128) v_4);
      v_6 <- eval (eq v_5 (expression.bv_nat 2 0));
      setRegister af bit_zero;
      setRegister cf (bit_or v_6 (eq v_5 (expression.bv_nat 2 2)));
      setRegister of bit_zero;
      setRegister pf v_6;
      setRegister sf bit_zero;
      setRegister zf (bit_or v_6 (eq v_5 (expression.bv_nat 2 3)));
      pure ()
    pat_end;
    pattern fun (xmm_0 : reg (bv 128)) (xmm_1 : reg (bv 128)) => do
      v_2 <- getRegister (lhs.of_reg xmm_1);
      v_3 <- getRegister (lhs.of_reg xmm_0);
      v_4 <- eval (_(_,_)_MINT-WRAPPER-SYNTAX comiss (extract v_2 96 128) (extract v_3 96 128));
      v_5 <- eval (eq v_4 (expression.bv_nat 2 0));
      setRegister af bit_zero;
      setRegister cf (bit_or v_5 (eq v_4 (expression.bv_nat 2 2)));
      setRegister of bit_zero;
      setRegister pf v_5;
      setRegister sf bit_zero;
      setRegister zf (bit_or v_5 (eq v_4 (expression.bv_nat 2 3)));
      pure ()
    pat_end
