def vpslld : instruction :=
  definst "vpslld" $ do
    pattern fun (imm_0 : imm int) (xmm_1 : reg (bv 128)) (xmm_2 : reg (bv 128)) => do
      v_3 <- eval (handleImmediateWithSignExtend imm_0 8 8);
      v_4 <- getRegister (lhs.of_reg xmm_1);
      v_5 <- eval (concat (expression.bv_nat 24 0) v_3);
      setRegister (lhs.of_reg xmm_2) (mux (ugt (concat (expression.bv_nat 56 0) v_3) (expression.bv_nat 64 31)) (expression.bv_nat 128 0) (concat (extract (shl (extract v_4 0 32) v_5) 0 32) (concat (extract (shl (extract v_4 32 64) v_5) 0 32) (concat (extract (shl (extract v_4 64 96) v_5) 0 32) (extract (shl (extract v_4 96 128) v_5) 0 32)))));
      pure ()
    pat_end;
    pattern fun (imm_0 : imm int) (ymm_1 : reg (bv 256)) (ymm_2 : reg (bv 256)) => do
      v_3 <- eval (handleImmediateWithSignExtend imm_0 8 8);
      v_4 <- getRegister (lhs.of_reg ymm_1);
      v_5 <- eval (concat (expression.bv_nat 24 0) v_3);
      setRegister (lhs.of_reg ymm_2) (mux (ugt (concat (expression.bv_nat 56 0) v_3) (expression.bv_nat 64 31)) (expression.bv_nat 256 0) (concat (extract (shl (extract v_4 0 32) v_5) 0 32) (concat (extract (shl (extract v_4 32 64) v_5) 0 32) (concat (extract (shl (extract v_4 64 96) v_5) 0 32) (concat (extract (shl (extract v_4 96 128) v_5) 0 32) (concat (extract (shl (extract v_4 128 160) v_5) 0 32) (concat (extract (shl (extract v_4 160 192) v_5) 0 32) (concat (extract (shl (extract v_4 192 224) v_5) 0 32) (extract (shl (extract v_4 224 256) v_5) 0 32)))))))));
      pure ()
    pat_end;
    pattern fun (mem_0 : Mem) (xmm_1 : reg (bv 128)) (xmm_2 : reg (bv 128)) => do
      v_3 <- evaluateAddress mem_0;
      v_4 <- load v_3 16;
      v_5 <- getRegister (lhs.of_reg xmm_1);
      v_6 <- eval (extract v_4 96 128);
      setRegister (lhs.of_reg xmm_2) (mux (ugt (extract v_4 64 128) (expression.bv_nat 64 31)) (expression.bv_nat 128 0) (concat (extract (shl (extract v_5 0 32) v_6) 0 32) (concat (extract (shl (extract v_5 32 64) v_6) 0 32) (concat (extract (shl (extract v_5 64 96) v_6) 0 32) (extract (shl (extract v_5 96 128) v_6) 0 32)))));
      pure ()
    pat_end;
    pattern fun (mem_0 : Mem) (ymm_1 : reg (bv 256)) (ymm_2 : reg (bv 256)) => do
      v_3 <- evaluateAddress mem_0;
      v_4 <- load v_3 16;
      v_5 <- getRegister (lhs.of_reg ymm_1);
      v_6 <- eval (extract v_4 96 128);
      setRegister (lhs.of_reg ymm_2) (mux (ugt (extract v_4 64 128) (expression.bv_nat 64 31)) (expression.bv_nat 256 0) (concat (extract (shl (extract v_5 0 32) v_6) 0 32) (concat (extract (shl (extract v_5 32 64) v_6) 0 32) (concat (extract (shl (extract v_5 64 96) v_6) 0 32) (concat (extract (shl (extract v_5 96 128) v_6) 0 32) (concat (extract (shl (extract v_5 128 160) v_6) 0 32) (concat (extract (shl (extract v_5 160 192) v_6) 0 32) (concat (extract (shl (extract v_5 192 224) v_6) 0 32) (extract (shl (extract v_5 224 256) v_6) 0 32)))))))));
      pure ()
    pat_end;
    pattern fun (xmm_0 : reg (bv 128)) (xmm_1 : reg (bv 128)) (xmm_2 : reg (bv 128)) => do
      v_3 <- getRegister (lhs.of_reg xmm_0);
      v_4 <- getRegister (lhs.of_reg xmm_1);
      v_5 <- eval (extract v_3 96 128);
      setRegister (lhs.of_reg xmm_2) (mux (ugt (extract v_3 64 128) (expression.bv_nat 64 31)) (expression.bv_nat 128 0) (concat (extract (shl (extract v_4 0 32) v_5) 0 32) (concat (extract (shl (extract v_4 32 64) v_5) 0 32) (concat (extract (shl (extract v_4 64 96) v_5) 0 32) (extract (shl (extract v_4 96 128) v_5) 0 32)))));
      pure ()
    pat_end;
    pattern fun (xmm_0 : reg (bv 128)) (ymm_1 : reg (bv 256)) (ymm_2 : reg (bv 256)) => do
      v_3 <- getRegister (lhs.of_reg xmm_0);
      v_4 <- getRegister (lhs.of_reg ymm_1);
      v_5 <- eval (extract v_3 96 128);
      setRegister (lhs.of_reg ymm_2) (mux (ugt (extract v_3 64 128) (expression.bv_nat 64 31)) (expression.bv_nat 256 0) (concat (extract (shl (extract v_4 0 32) v_5) 0 32) (concat (extract (shl (extract v_4 32 64) v_5) 0 32) (concat (extract (shl (extract v_4 64 96) v_5) 0 32) (concat (extract (shl (extract v_4 96 128) v_5) 0 32) (concat (extract (shl (extract v_4 128 160) v_5) 0 32) (concat (extract (shl (extract v_4 160 192) v_5) 0 32) (concat (extract (shl (extract v_4 192 224) v_5) 0 32) (extract (shl (extract v_4 224 256) v_5) 0 32)))))))));
      pure ()
    pat_end
