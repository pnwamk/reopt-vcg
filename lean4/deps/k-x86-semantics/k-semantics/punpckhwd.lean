def punpckhwd : instruction :=
  definst "punpckhwd" $ do
    pattern fun (mem_0 : Mem) (xmm_1 : reg (bv 128)) => do
      v_2 <- evaluateAddress mem_0;
      v_3 <- load v_2 16;
      v_4 <- getRegister (lhs.of_reg xmm_1);
      setRegister (lhs.of_reg xmm_1) (concat (concat (extract v_3 0 16) (extract v_4 0 16)) (concat (concat (extract v_3 16 32) (extract v_4 16 32)) (concat (concat (extract v_3 32 48) (extract v_4 32 48)) (concat (extract v_3 48 64) (extract v_4 48 64)))));
      pure ()
    pat_end;
    pattern fun (xmm_0 : reg (bv 128)) (xmm_1 : reg (bv 128)) => do
      v_2 <- getRegister (lhs.of_reg xmm_0);
      v_3 <- getRegister (lhs.of_reg xmm_1);
      setRegister (lhs.of_reg xmm_1) (concat (concat (extract v_2 0 16) (extract v_3 0 16)) (concat (concat (extract v_2 16 32) (extract v_3 16 32)) (concat (concat (extract v_2 32 48) (extract v_3 32 48)) (concat (extract v_2 48 64) (extract v_3 48 64)))));
      pure ()
    pat_end
