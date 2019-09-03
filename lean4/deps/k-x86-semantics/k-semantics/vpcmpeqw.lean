def vpcmpeqw1 : instruction :=
  definst "vpcmpeqw" $ do
    pattern fun (v_2836 : reg (bv 128)) (v_2837 : reg (bv 128)) (v_2838 : reg (bv 128)) => do
      v_7494 <- getRegister v_2837;
      v_7496 <- getRegister v_2836;
      setRegister (lhs.of_reg v_2838) (concat (mux (eq (extract v_7494 0 16) (extract v_7496 0 16)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (concat (mux (eq (extract v_7494 16 32) (extract v_7496 16 32)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (concat (mux (eq (extract v_7494 32 48) (extract v_7496 32 48)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (concat (mux (eq (extract v_7494 48 64) (extract v_7496 48 64)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (concat (mux (eq (extract v_7494 64 80) (extract v_7496 64 80)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (concat (mux (eq (extract v_7494 80 96) (extract v_7496 80 96)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (concat (mux (eq (extract v_7494 96 112) (extract v_7496 96 112)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (mux (eq (extract v_7494 112 128) (extract v_7496 112 128)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)))))))));
      pure ()
    pat_end;
    pattern fun (v_2850 : reg (bv 256)) (v_2851 : reg (bv 256)) (v_2852 : reg (bv 256)) => do
      v_7541 <- getRegister v_2851;
      v_7543 <- getRegister v_2850;
      setRegister (lhs.of_reg v_2852) (concat (mux (eq (extract v_7541 0 16) (extract v_7543 0 16)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (concat (mux (eq (extract v_7541 16 32) (extract v_7543 16 32)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (concat (mux (eq (extract v_7541 32 48) (extract v_7543 32 48)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (concat (mux (eq (extract v_7541 48 64) (extract v_7543 48 64)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (concat (mux (eq (extract v_7541 64 80) (extract v_7543 64 80)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (concat (mux (eq (extract v_7541 80 96) (extract v_7543 80 96)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (concat (mux (eq (extract v_7541 96 112) (extract v_7543 96 112)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (concat (mux (eq (extract v_7541 112 128) (extract v_7543 112 128)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (concat (mux (eq (extract v_7541 128 144) (extract v_7543 128 144)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (concat (mux (eq (extract v_7541 144 160) (extract v_7543 144 160)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (concat (mux (eq (extract v_7541 160 176) (extract v_7543 160 176)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (concat (mux (eq (extract v_7541 176 192) (extract v_7543 176 192)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (concat (mux (eq (extract v_7541 192 208) (extract v_7543 192 208)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (concat (mux (eq (extract v_7541 208 224) (extract v_7543 208 224)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (concat (mux (eq (extract v_7541 224 240) (extract v_7543 224 240)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (mux (eq (extract v_7541 240 256) (extract v_7543 240 256)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)))))))))))))))));
      pure ()
    pat_end;
    pattern fun (v_2835 : Mem) (v_2831 : reg (bv 128)) (v_2832 : reg (bv 128)) => do
      v_16334 <- getRegister v_2831;
      v_16336 <- evaluateAddress v_2835;
      v_16337 <- load v_16336 16;
      setRegister (lhs.of_reg v_2832) (concat (mux (eq (extract v_16334 0 16) (extract v_16337 0 16)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (concat (mux (eq (extract v_16334 16 32) (extract v_16337 16 32)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (concat (mux (eq (extract v_16334 32 48) (extract v_16337 32 48)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (concat (mux (eq (extract v_16334 48 64) (extract v_16337 48 64)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (concat (mux (eq (extract v_16334 64 80) (extract v_16337 64 80)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (concat (mux (eq (extract v_16334 80 96) (extract v_16337 80 96)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (concat (mux (eq (extract v_16334 96 112) (extract v_16337 96 112)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (mux (eq (extract v_16334 112 128) (extract v_16337 112 128)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)))))))));
      pure ()
    pat_end;
    pattern fun (v_2844 : Mem) (v_2845 : reg (bv 256)) (v_2846 : reg (bv 256)) => do
      v_16377 <- getRegister v_2845;
      v_16379 <- evaluateAddress v_2844;
      v_16380 <- load v_16379 32;
      setRegister (lhs.of_reg v_2846) (concat (mux (eq (extract v_16377 0 16) (extract v_16380 0 16)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (concat (mux (eq (extract v_16377 16 32) (extract v_16380 16 32)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (concat (mux (eq (extract v_16377 32 48) (extract v_16380 32 48)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (concat (mux (eq (extract v_16377 48 64) (extract v_16380 48 64)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (concat (mux (eq (extract v_16377 64 80) (extract v_16380 64 80)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (concat (mux (eq (extract v_16377 80 96) (extract v_16380 80 96)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (concat (mux (eq (extract v_16377 96 112) (extract v_16380 96 112)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (concat (mux (eq (extract v_16377 112 128) (extract v_16380 112 128)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (concat (mux (eq (extract v_16377 128 144) (extract v_16380 128 144)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (concat (mux (eq (extract v_16377 144 160) (extract v_16380 144 160)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (concat (mux (eq (extract v_16377 160 176) (extract v_16380 160 176)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (concat (mux (eq (extract v_16377 176 192) (extract v_16380 176 192)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (concat (mux (eq (extract v_16377 192 208) (extract v_16380 192 208)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (concat (mux (eq (extract v_16377 208 224) (extract v_16380 208 224)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (concat (mux (eq (extract v_16377 224 240) (extract v_16380 224 240)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)) (mux (eq (extract v_16377 240 256) (extract v_16380 240 256)) (expression.bv_nat 16 65535) (expression.bv_nat 16 0)))))))))))))))));
      pure ()
    pat_end
