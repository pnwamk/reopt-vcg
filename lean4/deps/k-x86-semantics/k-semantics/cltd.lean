def cltd1 : instruction :=
  definst "cltd" $ do
    pattern fun => do
      v_6800 <- getRegister rax;
      setRegister edx (extract (sext (extract v_6800 32 64) 64) 0 32);
      pure ()
    pat_end