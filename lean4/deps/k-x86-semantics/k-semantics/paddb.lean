def paddb : instruction :=
  definst "paddb" $ do
    pattern fun (mem_0 : Mem) (xmm_1 : reg (bv 128)) => do
      v_2 <- getRegister (lhs.of_reg xmm_1);
      v_3 <- evaluateAddress mem_0;
      v_4 <- load v_3 16;
      setRegister (lhs.of_reg xmm_1) (concat (add (extract v_2 0 8) (extract v_4 0 8)) (concat (add (extract v_2 8 16) (extract v_4 8 16)) (concat (add (extract v_2 16 24) (extract v_4 16 24)) (concat (add (extract v_2 24 32) (extract v_4 24 32)) (concat (add (extract v_2 32 40) (extract v_4 32 40)) (concat (add (extract v_2 40 48) (extract v_4 40 48)) (concat (add (extract v_2 48 56) (extract v_4 48 56)) (concat (add (extract v_2 56 64) (extract v_4 56 64)) (concat (add (extract v_2 64 72) (extract v_4 64 72)) (concat (add (extract v_2 72 80) (extract v_4 72 80)) (concat (add (extract v_2 80 88) (extract v_4 80 88)) (concat (add (extract v_2 88 96) (extract v_4 88 96)) (concat (add (extract v_2 96 104) (extract v_4 96 104)) (concat (add (extract v_2 104 112) (extract v_4 104 112)) (concat (add (extract v_2 112 120) (extract v_4 112 120)) (add (extract v_2 120 128) (extract v_4 120 128)))))))))))))))));
      pure ()
    pat_end;
    pattern fun (xmm_0 : reg (bv 128)) (xmm_1 : reg (bv 128)) => do
      v_2 <- getRegister (lhs.of_reg xmm_1);
      v_3 <- getRegister (lhs.of_reg xmm_0);
      setRegister (lhs.of_reg xmm_1) (concat (add (extract v_2 0 8) (extract v_3 0 8)) (concat (add (extract v_2 8 16) (extract v_3 8 16)) (concat (add (extract v_2 16 24) (extract v_3 16 24)) (concat (add (extract v_2 24 32) (extract v_3 24 32)) (concat (add (extract v_2 32 40) (extract v_3 32 40)) (concat (add (extract v_2 40 48) (extract v_3 40 48)) (concat (add (extract v_2 48 56) (extract v_3 48 56)) (concat (add (extract v_2 56 64) (extract v_3 56 64)) (concat (add (extract v_2 64 72) (extract v_3 64 72)) (concat (add (extract v_2 72 80) (extract v_3 72 80)) (concat (add (extract v_2 80 88) (extract v_3 80 88)) (concat (add (extract v_2 88 96) (extract v_3 88 96)) (concat (add (extract v_2 96 104) (extract v_3 96 104)) (concat (add (extract v_2 104 112) (extract v_3 104 112)) (concat (add (extract v_2 112 120) (extract v_3 112 120)) (add (extract v_2 120 128) (extract v_3 120 128)))))))))))))))));
      pure ()
    pat_end
