def btcl : instruction :=
  definst "btcl" $ do
    pattern fun (imm_0 : imm int) (mem_1 : Mem) => do
      v_2 <- evaluateAddress mem_1;
      v_3 <- eval (handleImmediateWithSignExtend imm_0 8 8);
      v_4 <- eval (add v_2 (concat (expression.bv_nat 59 0) (bv_and (extract v_3 0 5) (expression.bv_nat 5 3))));
      v_5 <- load v_4 1;
      v_6 <- eval (concat (expression.bv_nat 5 0) (bv_and (extract v_3 5 8) (expression.bv_nat 3 7)));
      store v_4 (bv_xor v_5 (extract (shl (expression.bv_nat 8 1) v_6) 0 8)) 1;
      setRegister af undef;
      setRegister cf (isBitSet (lshr v_5 v_6) 7);
      setRegister of undef;
      setRegister pf undef;
      setRegister sf undef;
      pure ()
    pat_end;
    pattern fun (imm_0 : imm int) (r32_1 : reg (bv 32)) => do
      v_2 <- getRegister (lhs.of_reg r32_1);
      v_3 <- eval (sext (bv_and (handleImmediateWithSignExtend imm_0 8 8) (expression.bv_nat 8 31)) 32);
      setRegister (lhs.of_reg r32_1) (bv_xor v_2 (extract (shl (expression.bv_nat 32 1) v_3) 0 32));
      setRegister af undef;
      setRegister cf (isBitSet (lshr v_2 v_3) 31);
      setRegister of undef;
      setRegister pf undef;
      setRegister sf undef;
      pure ()
    pat_end;
    pattern fun (r32_0 : reg (bv 32)) (mem_1 : Mem) => do
      v_2 <- evaluateAddress mem_1;
      v_3 <- getRegister (lhs.of_reg r32_0);
      v_4 <- eval (add v_2 (concat (expression.bv_nat 3 0) (extract (sext v_3 64) 0 61)));
      v_5 <- load v_4 1;
      v_6 <- eval (concat (expression.bv_nat 5 0) (extract v_3 29 32));
      store v_4 (bv_xor v_5 (extract (shl (expression.bv_nat 8 1) v_6) 0 8)) 1;
      setRegister af undef;
      setRegister cf (isBitSet (lshr v_5 v_6) 7);
      setRegister of undef;
      setRegister pf undef;
      setRegister sf undef;
      pure ()
    pat_end;
    pattern fun (r32_0 : reg (bv 32)) (r32_1 : reg (bv 32)) => do
      v_2 <- getRegister (lhs.of_reg r32_1);
      v_3 <- getRegister (lhs.of_reg r32_0);
      v_4 <- eval (bv_and v_3 (expression.bv_nat 32 31));
      setRegister (lhs.of_reg r32_1) (bv_xor v_2 (extract (shl (expression.bv_nat 32 1) v_4) 0 32));
      setRegister af undef;
      setRegister cf (isBitSet (lshr v_2 v_4) 31);
      setRegister of undef;
      setRegister pf undef;
      setRegister sf undef;
      pure ()
    pat_end
