def vpmulhrsw : instruction :=
  definst "vpmulhrsw" $ do
    pattern fun (mem_0 : Mem) (xmm_1 : reg (bv 128)) (xmm_2 : reg (bv 128)) => do
      v_3 <- getRegister (lhs.of_reg xmm_1);
      v_4 <- evaluateAddress mem_0;
      v_5 <- load v_4 16;
      setRegister (lhs.of_reg xmm_2) (concat (extract (add (lshr (mul (sext (extract v_3 0 16) 32) (sext (extract v_5 0 16) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (concat (extract (add (lshr (mul (sext (extract v_3 16 32) 32) (sext (extract v_5 16 32) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (concat (extract (add (lshr (mul (sext (extract v_3 32 48) 32) (sext (extract v_5 32 48) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (concat (extract (add (lshr (mul (sext (extract v_3 48 64) 32) (sext (extract v_5 48 64) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (concat (extract (add (lshr (mul (sext (extract v_3 64 80) 32) (sext (extract v_5 64 80) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (concat (extract (add (lshr (mul (sext (extract v_3 80 96) 32) (sext (extract v_5 80 96) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (concat (extract (add (lshr (mul (sext (extract v_3 96 112) 32) (sext (extract v_5 96 112) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (extract (add (lshr (mul (sext (extract v_3 112 128) 32) (sext (extract v_5 112 128) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31))))))));
      pure ()
    pat_end;
    pattern fun (mem_0 : Mem) (ymm_1 : reg (bv 256)) (ymm_2 : reg (bv 256)) => do
      v_3 <- getRegister (lhs.of_reg ymm_1);
      v_4 <- evaluateAddress mem_0;
      v_5 <- load v_4 32;
      setRegister (lhs.of_reg ymm_2) (concat (extract (add (lshr (mul (sext (extract v_3 0 16) 32) (sext (extract v_5 0 16) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (concat (extract (add (lshr (mul (sext (extract v_3 16 32) 32) (sext (extract v_5 16 32) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (concat (extract (add (lshr (mul (sext (extract v_3 32 48) 32) (sext (extract v_5 32 48) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (concat (extract (add (lshr (mul (sext (extract v_3 48 64) 32) (sext (extract v_5 48 64) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (concat (extract (add (lshr (mul (sext (extract v_3 64 80) 32) (sext (extract v_5 64 80) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (concat (extract (add (lshr (mul (sext (extract v_3 80 96) 32) (sext (extract v_5 80 96) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (concat (extract (add (lshr (mul (sext (extract v_3 96 112) 32) (sext (extract v_5 96 112) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (concat (extract (add (lshr (mul (sext (extract v_3 112 128) 32) (sext (extract v_5 112 128) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (concat (extract (add (lshr (mul (sext (extract v_3 128 144) 32) (sext (extract v_5 128 144) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (concat (extract (add (lshr (mul (sext (extract v_3 144 160) 32) (sext (extract v_5 144 160) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (concat (extract (add (lshr (mul (sext (extract v_3 160 176) 32) (sext (extract v_5 160 176) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (concat (extract (add (lshr (mul (sext (extract v_3 176 192) 32) (sext (extract v_5 176 192) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (concat (extract (add (lshr (mul (sext (extract v_3 192 208) 32) (sext (extract v_5 192 208) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (concat (extract (add (lshr (mul (sext (extract v_3 208 224) 32) (sext (extract v_5 208 224) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (concat (extract (add (lshr (mul (sext (extract v_3 224 240) 32) (sext (extract v_5 224 240) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (extract (add (lshr (mul (sext (extract v_3 240 256) 32) (sext (extract v_5 240 256) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31))))))))))))))));
      pure ()
    pat_end;
    pattern fun (xmm_0 : reg (bv 128)) (xmm_1 : reg (bv 128)) (xmm_2 : reg (bv 128)) => do
      v_3 <- getRegister (lhs.of_reg xmm_1);
      v_4 <- getRegister (lhs.of_reg xmm_0);
      setRegister (lhs.of_reg xmm_2) (concat (extract (add (lshr (mul (sext (extract v_3 0 16) 32) (sext (extract v_4 0 16) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (concat (extract (add (lshr (mul (sext (extract v_3 16 32) 32) (sext (extract v_4 16 32) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (concat (extract (add (lshr (mul (sext (extract v_3 32 48) 32) (sext (extract v_4 32 48) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (concat (extract (add (lshr (mul (sext (extract v_3 48 64) 32) (sext (extract v_4 48 64) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (concat (extract (add (lshr (mul (sext (extract v_3 64 80) 32) (sext (extract v_4 64 80) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (concat (extract (add (lshr (mul (sext (extract v_3 80 96) 32) (sext (extract v_4 80 96) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (concat (extract (add (lshr (mul (sext (extract v_3 96 112) 32) (sext (extract v_4 96 112) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (extract (add (lshr (mul (sext (extract v_3 112 128) 32) (sext (extract v_4 112 128) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31))))))));
      pure ()
    pat_end;
    pattern fun (ymm_0 : reg (bv 256)) (ymm_1 : reg (bv 256)) (ymm_2 : reg (bv 256)) => do
      v_3 <- getRegister (lhs.of_reg ymm_1);
      v_4 <- getRegister (lhs.of_reg ymm_0);
      setRegister (lhs.of_reg ymm_2) (concat (extract (add (lshr (mul (sext (extract v_3 0 16) 32) (sext (extract v_4 0 16) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (concat (extract (add (lshr (mul (sext (extract v_3 16 32) 32) (sext (extract v_4 16 32) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (concat (extract (add (lshr (mul (sext (extract v_3 32 48) 32) (sext (extract v_4 32 48) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (concat (extract (add (lshr (mul (sext (extract v_3 48 64) 32) (sext (extract v_4 48 64) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (concat (extract (add (lshr (mul (sext (extract v_3 64 80) 32) (sext (extract v_4 64 80) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (concat (extract (add (lshr (mul (sext (extract v_3 80 96) 32) (sext (extract v_4 80 96) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (concat (extract (add (lshr (mul (sext (extract v_3 96 112) 32) (sext (extract v_4 96 112) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (concat (extract (add (lshr (mul (sext (extract v_3 112 128) 32) (sext (extract v_4 112 128) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (concat (extract (add (lshr (mul (sext (extract v_3 128 144) 32) (sext (extract v_4 128 144) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (concat (extract (add (lshr (mul (sext (extract v_3 144 160) 32) (sext (extract v_4 144 160) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (concat (extract (add (lshr (mul (sext (extract v_3 160 176) 32) (sext (extract v_4 160 176) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (concat (extract (add (lshr (mul (sext (extract v_3 176 192) 32) (sext (extract v_4 176 192) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (concat (extract (add (lshr (mul (sext (extract v_3 192 208) 32) (sext (extract v_4 192 208) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (concat (extract (add (lshr (mul (sext (extract v_3 208 224) 32) (sext (extract v_4 208 224) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (concat (extract (add (lshr (mul (sext (extract v_3 224 240) 32) (sext (extract v_4 224 240) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31) (extract (add (lshr (mul (sext (extract v_3 240 256) 32) (sext (extract v_4 240 256) 32)) (expression.bv_nat 32 14)) (expression.bv_nat 32 1)) 15 31))))))))))))))));
      pure ()
    pat_end
