def vsubps : instruction :=
  definst "vsubps" $ do
    pattern fun (mem_0 : Mem) (xmm_1 : reg (bv 128)) (xmm_2 : reg (bv 128)) => do
      v_3 <- getRegister (lhs.of_reg xmm_1);
      v_4 <- evaluateAddress mem_0;
      v_5 <- load v_4 16;
      setRegister (lhs.of_reg xmm_2) (concat (Float2MInt (_-Float__FLOAT (MInt2Float (extract v_3 0 32) 24 8) (MInt2Float (extract v_5 0 32) 24 8)) 32) (concat (Float2MInt (_-Float__FLOAT (MInt2Float (extract v_3 32 64) 24 8) (MInt2Float (extract v_5 32 64) 24 8)) 32) (concat (Float2MInt (_-Float__FLOAT (MInt2Float (extract v_3 64 96) 24 8) (MInt2Float (extract v_5 64 96) 24 8)) 32) (Float2MInt (_-Float__FLOAT (MInt2Float (extract v_3 96 128) 24 8) (MInt2Float (extract v_5 96 128) 24 8)) 32))));
      pure ()
    pat_end;
    pattern fun (mem_0 : Mem) (ymm_1 : reg (bv 256)) (ymm_2 : reg (bv 256)) => do
      v_3 <- getRegister (lhs.of_reg ymm_1);
      v_4 <- evaluateAddress mem_0;
      v_5 <- load v_4 32;
      setRegister (lhs.of_reg ymm_2) (concat (Float2MInt (_-Float__FLOAT (MInt2Float (extract v_3 0 32) 24 8) (MInt2Float (extract v_5 0 32) 24 8)) 32) (concat (Float2MInt (_-Float__FLOAT (MInt2Float (extract v_3 32 64) 24 8) (MInt2Float (extract v_5 32 64) 24 8)) 32) (concat (Float2MInt (_-Float__FLOAT (MInt2Float (extract v_3 64 96) 24 8) (MInt2Float (extract v_5 64 96) 24 8)) 32) (concat (Float2MInt (_-Float__FLOAT (MInt2Float (extract v_3 96 128) 24 8) (MInt2Float (extract v_5 96 128) 24 8)) 32) (concat (Float2MInt (_-Float__FLOAT (MInt2Float (extract v_3 128 160) 24 8) (MInt2Float (extract v_5 128 160) 24 8)) 32) (concat (Float2MInt (_-Float__FLOAT (MInt2Float (extract v_3 160 192) 24 8) (MInt2Float (extract v_5 160 192) 24 8)) 32) (concat (Float2MInt (_-Float__FLOAT (MInt2Float (extract v_3 192 224) 24 8) (MInt2Float (extract v_5 192 224) 24 8)) 32) (Float2MInt (_-Float__FLOAT (MInt2Float (extract v_3 224 256) 24 8) (MInt2Float (extract v_5 224 256) 24 8)) 32))))))));
      pure ()
    pat_end;
    pattern fun (xmm_0 : reg (bv 128)) (xmm_1 : reg (bv 128)) (xmm_2 : reg (bv 128)) => do
      v_3 <- getRegister (lhs.of_reg xmm_1);
      v_4 <- getRegister (lhs.of_reg xmm_0);
      setRegister (lhs.of_reg xmm_2) (concat (Float2MInt (_-Float__FLOAT (MInt2Float (extract v_3 0 32) 24 8) (MInt2Float (extract v_4 0 32) 24 8)) 32) (concat (Float2MInt (_-Float__FLOAT (MInt2Float (extract v_3 32 64) 24 8) (MInt2Float (extract v_4 32 64) 24 8)) 32) (concat (Float2MInt (_-Float__FLOAT (MInt2Float (extract v_3 64 96) 24 8) (MInt2Float (extract v_4 64 96) 24 8)) 32) (Float2MInt (_-Float__FLOAT (MInt2Float (extract v_3 96 128) 24 8) (MInt2Float (extract v_4 96 128) 24 8)) 32))));
      pure ()
    pat_end;
    pattern fun (ymm_0 : reg (bv 256)) (ymm_1 : reg (bv 256)) (ymm_2 : reg (bv 256)) => do
      v_3 <- getRegister (lhs.of_reg ymm_1);
      v_4 <- getRegister (lhs.of_reg ymm_0);
      setRegister (lhs.of_reg ymm_2) (concat (Float2MInt (_-Float__FLOAT (MInt2Float (extract v_3 0 32) 24 8) (MInt2Float (extract v_4 0 32) 24 8)) 32) (concat (Float2MInt (_-Float__FLOAT (MInt2Float (extract v_3 32 64) 24 8) (MInt2Float (extract v_4 32 64) 24 8)) 32) (concat (Float2MInt (_-Float__FLOAT (MInt2Float (extract v_3 64 96) 24 8) (MInt2Float (extract v_4 64 96) 24 8)) 32) (concat (Float2MInt (_-Float__FLOAT (MInt2Float (extract v_3 96 128) 24 8) (MInt2Float (extract v_4 96 128) 24 8)) 32) (concat (Float2MInt (_-Float__FLOAT (MInt2Float (extract v_3 128 160) 24 8) (MInt2Float (extract v_4 128 160) 24 8)) 32) (concat (Float2MInt (_-Float__FLOAT (MInt2Float (extract v_3 160 192) 24 8) (MInt2Float (extract v_4 160 192) 24 8)) 32) (concat (Float2MInt (_-Float__FLOAT (MInt2Float (extract v_3 192 224) 24 8) (MInt2Float (extract v_4 192 224) 24 8)) 32) (Float2MInt (_-Float__FLOAT (MInt2Float (extract v_3 224 256) 24 8) (MInt2Float (extract v_4 224 256) 24 8)) 32))))))));
      pure ()
    pat_end
