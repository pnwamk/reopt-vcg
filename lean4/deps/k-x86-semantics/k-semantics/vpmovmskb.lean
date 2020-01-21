def vpmovmskb : instruction :=
  definst "vpmovmskb" $ do
    pattern fun (xmm_0 : reg (bv 128)) (r32_1 : reg (bv 32)) => do
      v_2 <- getRegister (lhs.of_reg xmm_0);
      setRegister (lhs.of_reg r32_1) (concat (expression.bv_nat 16 0) (concat (extract v_2 0 1) (concat (extract v_2 8 9) (concat (extract v_2 16 17) (concat (extract v_2 24 25) (concat (extract v_2 32 33) (concat (extract v_2 40 41) (concat (extract v_2 48 49) (concat (extract v_2 56 57) (concat (extract v_2 64 65) (concat (extract v_2 72 73) (concat (extract v_2 80 81) (concat (extract v_2 88 89) (concat (extract v_2 96 97) (concat (extract v_2 104 105) (concat (extract v_2 112 113) (extract v_2 120 121)))))))))))))))));
      pure ()
    pat_end;
    pattern fun (xmm_0 : reg (bv 128)) (r64_1 : reg (bv 64)) => do
      v_2 <- getRegister (lhs.of_reg xmm_0);
      setRegister (lhs.of_reg r64_1) (concat (expression.bv_nat 48 0) (concat (extract v_2 0 1) (concat (extract v_2 8 9) (concat (extract v_2 16 17) (concat (extract v_2 24 25) (concat (extract v_2 32 33) (concat (extract v_2 40 41) (concat (extract v_2 48 49) (concat (extract v_2 56 57) (concat (extract v_2 64 65) (concat (extract v_2 72 73) (concat (extract v_2 80 81) (concat (extract v_2 88 89) (concat (extract v_2 96 97) (concat (extract v_2 104 105) (concat (extract v_2 112 113) (extract v_2 120 121)))))))))))))))));
      pure ()
    pat_end;
    pattern fun (ymm_0 : reg (bv 256)) (r32_1 : reg (bv 32)) => do
      v_2 <- getRegister (lhs.of_reg ymm_0);
      setRegister (lhs.of_reg r32_1) (concat (extract v_2 0 1) (concat (extract v_2 8 9) (concat (extract v_2 16 17) (concat (extract v_2 24 25) (concat (extract v_2 32 33) (concat (extract v_2 40 41) (concat (extract v_2 48 49) (concat (extract v_2 56 57) (concat (extract v_2 64 65) (concat (extract v_2 72 73) (concat (extract v_2 80 81) (concat (extract v_2 88 89) (concat (extract v_2 96 97) (concat (extract v_2 104 105) (concat (extract v_2 112 113) (concat (extract v_2 120 121) (concat (extract v_2 128 129) (concat (extract v_2 136 137) (concat (extract v_2 144 145) (concat (extract v_2 152 153) (concat (extract v_2 160 161) (concat (extract v_2 168 169) (concat (extract v_2 176 177) (concat (extract v_2 184 185) (concat (extract v_2 192 193) (concat (extract v_2 200 201) (concat (extract v_2 208 209) (concat (extract v_2 216 217) (concat (extract v_2 224 225) (concat (extract v_2 232 233) (concat (extract v_2 240 241) (extract v_2 248 249))))))))))))))))))))))))))))))));
      pure ()
    pat_end;
    pattern fun (ymm_0 : reg (bv 256)) (r64_1 : reg (bv 64)) => do
      v_2 <- getRegister (lhs.of_reg ymm_0);
      setRegister (lhs.of_reg r64_1) (concat (expression.bv_nat 32 0) (concat (extract v_2 0 1) (concat (extract v_2 8 9) (concat (extract v_2 16 17) (concat (extract v_2 24 25) (concat (extract v_2 32 33) (concat (extract v_2 40 41) (concat (extract v_2 48 49) (concat (extract v_2 56 57) (concat (extract v_2 64 65) (concat (extract v_2 72 73) (concat (extract v_2 80 81) (concat (extract v_2 88 89) (concat (extract v_2 96 97) (concat (extract v_2 104 105) (concat (extract v_2 112 113) (concat (extract v_2 120 121) (concat (extract v_2 128 129) (concat (extract v_2 136 137) (concat (extract v_2 144 145) (concat (extract v_2 152 153) (concat (extract v_2 160 161) (concat (extract v_2 168 169) (concat (extract v_2 176 177) (concat (extract v_2 184 185) (concat (extract v_2 192 193) (concat (extract v_2 200 201) (concat (extract v_2 208 209) (concat (extract v_2 216 217) (concat (extract v_2 224 225) (concat (extract v_2 232 233) (concat (extract v_2 240 241) (extract v_2 248 249)))))))))))))))))))))))))))))))));
      pure ()
    pat_end
