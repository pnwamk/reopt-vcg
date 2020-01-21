def cvtdq2pd : instruction :=
  definst "cvtdq2pd" $ do
    pattern fun (mem_0 : Mem) (xmm_1 : reg (bv 128)) => do
      v_2 <- evaluateAddress mem_0;
      v_3 <- load v_2 8;
      setRegister (lhs.of_reg xmm_1) (concat (Float2MInt (Int2Float (svalueMInt (extract v_3 0 32)) 53 11) 64) (Float2MInt (Int2Float (svalueMInt (extract v_3 32 64)) 53 11) 64));
      pure ()
    pat_end;
    pattern fun (xmm_0 : reg (bv 128)) (xmm_1 : reg (bv 128)) => do
      v_2 <- getRegister (lhs.of_reg xmm_0);
      setRegister (lhs.of_reg xmm_1) (concat (Float2MInt (Int2Float (svalueMInt (extract v_2 64 96)) 53 11) 64) (Float2MInt (Int2Float (svalueMInt (extract v_2 96 128)) 53 11) 64));
      pure ()
    pat_end
