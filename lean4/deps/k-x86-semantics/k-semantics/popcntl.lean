def popcntl : instruction :=
  definst "popcntl" $ do
    pattern fun (mem_0 : Mem) (r32_1 : reg (bv 32)) => do
      v_2 <- evaluateAddress mem_0;
      v_3 <- load v_2 4;
      setRegister (lhs.of_reg r32_1) (add (concat (expression.bv_nat 16 0) (add (concat (expression.bv_nat 8 0) (add (concat (expression.bv_nat 4 0) (add (concat (expression.bv_nat 2 0) (add (concat (expression.bv_nat 1 0) (extract v_3 0 1)) (concat (expression.bv_nat 1 0) (extract v_3 1 2)))) (concat (expression.bv_nat 2 0) (add (concat (expression.bv_nat 1 0) (extract v_3 2 3)) (concat (expression.bv_nat 1 0) (extract v_3 3 4)))))) (concat (expression.bv_nat 4 0) (add (concat (expression.bv_nat 2 0) (add (concat (expression.bv_nat 1 0) (extract v_3 4 5)) (concat (expression.bv_nat 1 0) (extract v_3 5 6)))) (concat (expression.bv_nat 2 0) (add (concat (expression.bv_nat 1 0) (extract v_3 6 7)) (concat (expression.bv_nat 1 0) (extract v_3 7 8)))))))) (concat (expression.bv_nat 8 0) (add (concat (expression.bv_nat 4 0) (add (concat (expression.bv_nat 2 0) (add (concat (expression.bv_nat 1 0) (extract v_3 8 9)) (concat (expression.bv_nat 1 0) (extract v_3 9 10)))) (concat (expression.bv_nat 2 0) (add (concat (expression.bv_nat 1 0) (extract v_3 10 11)) (concat (expression.bv_nat 1 0) (extract v_3 11 12)))))) (concat (expression.bv_nat 4 0) (add (concat (expression.bv_nat 2 0) (add (concat (expression.bv_nat 1 0) (extract v_3 12 13)) (concat (expression.bv_nat 1 0) (extract v_3 13 14)))) (concat (expression.bv_nat 2 0) (add (concat (expression.bv_nat 1 0) (extract v_3 14 15)) (concat (expression.bv_nat 1 0) (extract v_3 15 16)))))))))) (concat (expression.bv_nat 16 0) (add (concat (expression.bv_nat 8 0) (add (concat (expression.bv_nat 4 0) (add (concat (expression.bv_nat 2 0) (add (concat (expression.bv_nat 1 0) (extract v_3 16 17)) (concat (expression.bv_nat 1 0) (extract v_3 17 18)))) (concat (expression.bv_nat 2 0) (add (concat (expression.bv_nat 1 0) (extract v_3 18 19)) (concat (expression.bv_nat 1 0) (extract v_3 19 20)))))) (concat (expression.bv_nat 4 0) (add (concat (expression.bv_nat 2 0) (add (concat (expression.bv_nat 1 0) (extract v_3 20 21)) (concat (expression.bv_nat 1 0) (extract v_3 21 22)))) (concat (expression.bv_nat 2 0) (add (concat (expression.bv_nat 1 0) (extract v_3 22 23)) (concat (expression.bv_nat 1 0) (extract v_3 23 24)))))))) (concat (expression.bv_nat 8 0) (add (concat (expression.bv_nat 4 0) (add (concat (expression.bv_nat 2 0) (add (concat (expression.bv_nat 1 0) (extract v_3 24 25)) (concat (expression.bv_nat 1 0) (extract v_3 25 26)))) (concat (expression.bv_nat 2 0) (add (concat (expression.bv_nat 1 0) (extract v_3 26 27)) (concat (expression.bv_nat 1 0) (extract v_3 27 28)))))) (concat (expression.bv_nat 4 0) (add (concat (expression.bv_nat 2 0) (add (concat (expression.bv_nat 1 0) (extract v_3 28 29)) (concat (expression.bv_nat 1 0) (extract v_3 29 30)))) (concat (expression.bv_nat 2 0) (add (concat (expression.bv_nat 1 0) (extract v_3 30 31)) (concat (expression.bv_nat 1 0) (extract v_3 31 32)))))))))));
      setRegister af bit_zero;
      setRegister cf bit_zero;
      setRegister of bit_zero;
      setRegister pf bit_zero;
      setRegister sf bit_zero;
      setRegister zf (zeroFlag v_3);
      pure ()
    pat_end;
    pattern fun (r32_0 : reg (bv 32)) (r32_1 : reg (bv 32)) => do
      v_2 <- getRegister (lhs.of_reg r32_0);
      setRegister (lhs.of_reg r32_1) (add (concat (expression.bv_nat 16 0) (add (concat (expression.bv_nat 8 0) (add (concat (expression.bv_nat 4 0) (add (concat (expression.bv_nat 2 0) (add (concat (expression.bv_nat 1 0) (extract v_2 0 1)) (concat (expression.bv_nat 1 0) (extract v_2 1 2)))) (concat (expression.bv_nat 2 0) (add (concat (expression.bv_nat 1 0) (extract v_2 2 3)) (concat (expression.bv_nat 1 0) (extract v_2 3 4)))))) (concat (expression.bv_nat 4 0) (add (concat (expression.bv_nat 2 0) (add (concat (expression.bv_nat 1 0) (extract v_2 4 5)) (concat (expression.bv_nat 1 0) (extract v_2 5 6)))) (concat (expression.bv_nat 2 0) (add (concat (expression.bv_nat 1 0) (extract v_2 6 7)) (concat (expression.bv_nat 1 0) (extract v_2 7 8)))))))) (concat (expression.bv_nat 8 0) (add (concat (expression.bv_nat 4 0) (add (concat (expression.bv_nat 2 0) (add (concat (expression.bv_nat 1 0) (extract v_2 8 9)) (concat (expression.bv_nat 1 0) (extract v_2 9 10)))) (concat (expression.bv_nat 2 0) (add (concat (expression.bv_nat 1 0) (extract v_2 10 11)) (concat (expression.bv_nat 1 0) (extract v_2 11 12)))))) (concat (expression.bv_nat 4 0) (add (concat (expression.bv_nat 2 0) (add (concat (expression.bv_nat 1 0) (extract v_2 12 13)) (concat (expression.bv_nat 1 0) (extract v_2 13 14)))) (concat (expression.bv_nat 2 0) (add (concat (expression.bv_nat 1 0) (extract v_2 14 15)) (concat (expression.bv_nat 1 0) (extract v_2 15 16)))))))))) (concat (expression.bv_nat 16 0) (add (concat (expression.bv_nat 8 0) (add (concat (expression.bv_nat 4 0) (add (concat (expression.bv_nat 2 0) (add (concat (expression.bv_nat 1 0) (extract v_2 16 17)) (concat (expression.bv_nat 1 0) (extract v_2 17 18)))) (concat (expression.bv_nat 2 0) (add (concat (expression.bv_nat 1 0) (extract v_2 18 19)) (concat (expression.bv_nat 1 0) (extract v_2 19 20)))))) (concat (expression.bv_nat 4 0) (add (concat (expression.bv_nat 2 0) (add (concat (expression.bv_nat 1 0) (extract v_2 20 21)) (concat (expression.bv_nat 1 0) (extract v_2 21 22)))) (concat (expression.bv_nat 2 0) (add (concat (expression.bv_nat 1 0) (extract v_2 22 23)) (concat (expression.bv_nat 1 0) (extract v_2 23 24)))))))) (concat (expression.bv_nat 8 0) (add (concat (expression.bv_nat 4 0) (add (concat (expression.bv_nat 2 0) (add (concat (expression.bv_nat 1 0) (extract v_2 24 25)) (concat (expression.bv_nat 1 0) (extract v_2 25 26)))) (concat (expression.bv_nat 2 0) (add (concat (expression.bv_nat 1 0) (extract v_2 26 27)) (concat (expression.bv_nat 1 0) (extract v_2 27 28)))))) (concat (expression.bv_nat 4 0) (add (concat (expression.bv_nat 2 0) (add (concat (expression.bv_nat 1 0) (extract v_2 28 29)) (concat (expression.bv_nat 1 0) (extract v_2 29 30)))) (concat (expression.bv_nat 2 0) (add (concat (expression.bv_nat 1 0) (extract v_2 30 31)) (concat (expression.bv_nat 1 0) (extract v_2 31 32)))))))))));
      setRegister af bit_zero;
      setRegister cf bit_zero;
      setRegister of bit_zero;
      setRegister pf bit_zero;
      setRegister sf bit_zero;
      setRegister zf (zeroFlag v_2);
      pure ()
    pat_end
