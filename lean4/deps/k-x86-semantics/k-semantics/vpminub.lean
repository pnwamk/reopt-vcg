def vpminub : instruction :=
  definst "vpminub" $ do
    pattern fun (mem_0 : Mem) (xmm_1 : reg (bv 128)) (xmm_2 : reg (bv 128)) => do
      v_3 <- getRegister (lhs.of_reg xmm_1);
      v_4 <- eval (extract v_3 0 8);
      v_5 <- evaluateAddress mem_0;
      v_6 <- load v_5 16;
      v_7 <- eval (extract v_6 0 8);
      v_8 <- eval (extract v_3 8 16);
      v_9 <- eval (extract v_6 8 16);
      v_10 <- eval (extract v_3 16 24);
      v_11 <- eval (extract v_6 16 24);
      v_12 <- eval (extract v_3 24 32);
      v_13 <- eval (extract v_6 24 32);
      v_14 <- eval (extract v_3 32 40);
      v_15 <- eval (extract v_6 32 40);
      v_16 <- eval (extract v_3 40 48);
      v_17 <- eval (extract v_6 40 48);
      v_18 <- eval (extract v_3 48 56);
      v_19 <- eval (extract v_6 48 56);
      v_20 <- eval (extract v_3 56 64);
      v_21 <- eval (extract v_6 56 64);
      v_22 <- eval (extract v_3 64 72);
      v_23 <- eval (extract v_6 64 72);
      v_24 <- eval (extract v_3 72 80);
      v_25 <- eval (extract v_6 72 80);
      v_26 <- eval (extract v_3 80 88);
      v_27 <- eval (extract v_6 80 88);
      v_28 <- eval (extract v_3 88 96);
      v_29 <- eval (extract v_6 88 96);
      v_30 <- eval (extract v_3 96 104);
      v_31 <- eval (extract v_6 96 104);
      v_32 <- eval (extract v_3 104 112);
      v_33 <- eval (extract v_6 104 112);
      v_34 <- eval (extract v_3 112 120);
      v_35 <- eval (extract v_6 112 120);
      v_36 <- eval (extract v_3 120 128);
      v_37 <- eval (extract v_6 120 128);
      setRegister (lhs.of_reg xmm_2) (concat (mux (ult v_4 v_7) v_4 v_7) (concat (mux (ult v_8 v_9) v_8 v_9) (concat (mux (ult v_10 v_11) v_10 v_11) (concat (mux (ult v_12 v_13) v_12 v_13) (concat (mux (ult v_14 v_15) v_14 v_15) (concat (mux (ult v_16 v_17) v_16 v_17) (concat (mux (ult v_18 v_19) v_18 v_19) (concat (mux (ult v_20 v_21) v_20 v_21) (concat (mux (ult v_22 v_23) v_22 v_23) (concat (mux (ult v_24 v_25) v_24 v_25) (concat (mux (ult v_26 v_27) v_26 v_27) (concat (mux (ult v_28 v_29) v_28 v_29) (concat (mux (ult v_30 v_31) v_30 v_31) (concat (mux (ult v_32 v_33) v_32 v_33) (concat (mux (ult v_34 v_35) v_34 v_35) (mux (ult v_36 v_37) v_36 v_37))))))))))))))));
      pure ()
    pat_end;
    pattern fun (mem_0 : Mem) (ymm_1 : reg (bv 256)) (ymm_2 : reg (bv 256)) => do
      v_3 <- getRegister (lhs.of_reg ymm_1);
      v_4 <- eval (extract v_3 0 8);
      v_5 <- evaluateAddress mem_0;
      v_6 <- load v_5 32;
      v_7 <- eval (extract v_6 0 8);
      v_8 <- eval (extract v_3 8 16);
      v_9 <- eval (extract v_6 8 16);
      v_10 <- eval (extract v_3 16 24);
      v_11 <- eval (extract v_6 16 24);
      v_12 <- eval (extract v_3 24 32);
      v_13 <- eval (extract v_6 24 32);
      v_14 <- eval (extract v_3 32 40);
      v_15 <- eval (extract v_6 32 40);
      v_16 <- eval (extract v_3 40 48);
      v_17 <- eval (extract v_6 40 48);
      v_18 <- eval (extract v_3 48 56);
      v_19 <- eval (extract v_6 48 56);
      v_20 <- eval (extract v_3 56 64);
      v_21 <- eval (extract v_6 56 64);
      v_22 <- eval (extract v_3 64 72);
      v_23 <- eval (extract v_6 64 72);
      v_24 <- eval (extract v_3 72 80);
      v_25 <- eval (extract v_6 72 80);
      v_26 <- eval (extract v_3 80 88);
      v_27 <- eval (extract v_6 80 88);
      v_28 <- eval (extract v_3 88 96);
      v_29 <- eval (extract v_6 88 96);
      v_30 <- eval (extract v_3 96 104);
      v_31 <- eval (extract v_6 96 104);
      v_32 <- eval (extract v_3 104 112);
      v_33 <- eval (extract v_6 104 112);
      v_34 <- eval (extract v_3 112 120);
      v_35 <- eval (extract v_6 112 120);
      v_36 <- eval (extract v_3 120 128);
      v_37 <- eval (extract v_6 120 128);
      v_38 <- eval (extract v_3 128 136);
      v_39 <- eval (extract v_6 128 136);
      v_40 <- eval (extract v_3 136 144);
      v_41 <- eval (extract v_6 136 144);
      v_42 <- eval (extract v_3 144 152);
      v_43 <- eval (extract v_6 144 152);
      v_44 <- eval (extract v_3 152 160);
      v_45 <- eval (extract v_6 152 160);
      v_46 <- eval (extract v_3 160 168);
      v_47 <- eval (extract v_6 160 168);
      v_48 <- eval (extract v_3 168 176);
      v_49 <- eval (extract v_6 168 176);
      v_50 <- eval (extract v_3 176 184);
      v_51 <- eval (extract v_6 176 184);
      v_52 <- eval (extract v_3 184 192);
      v_53 <- eval (extract v_6 184 192);
      v_54 <- eval (extract v_3 192 200);
      v_55 <- eval (extract v_6 192 200);
      v_56 <- eval (extract v_3 200 208);
      v_57 <- eval (extract v_6 200 208);
      v_58 <- eval (extract v_3 208 216);
      v_59 <- eval (extract v_6 208 216);
      v_60 <- eval (extract v_3 216 224);
      v_61 <- eval (extract v_6 216 224);
      v_62 <- eval (extract v_3 224 232);
      v_63 <- eval (extract v_6 224 232);
      v_64 <- eval (extract v_3 232 240);
      v_65 <- eval (extract v_6 232 240);
      v_66 <- eval (extract v_3 240 248);
      v_67 <- eval (extract v_6 240 248);
      v_68 <- eval (extract v_3 248 256);
      v_69 <- eval (extract v_6 248 256);
      setRegister (lhs.of_reg ymm_2) (concat (mux (ult v_4 v_7) v_4 v_7) (concat (mux (ult v_8 v_9) v_8 v_9) (concat (mux (ult v_10 v_11) v_10 v_11) (concat (mux (ult v_12 v_13) v_12 v_13) (concat (mux (ult v_14 v_15) v_14 v_15) (concat (mux (ult v_16 v_17) v_16 v_17) (concat (mux (ult v_18 v_19) v_18 v_19) (concat (mux (ult v_20 v_21) v_20 v_21) (concat (mux (ult v_22 v_23) v_22 v_23) (concat (mux (ult v_24 v_25) v_24 v_25) (concat (mux (ult v_26 v_27) v_26 v_27) (concat (mux (ult v_28 v_29) v_28 v_29) (concat (mux (ult v_30 v_31) v_30 v_31) (concat (mux (ult v_32 v_33) v_32 v_33) (concat (mux (ult v_34 v_35) v_34 v_35) (concat (mux (ult v_36 v_37) v_36 v_37) (concat (mux (ult v_38 v_39) v_38 v_39) (concat (mux (ult v_40 v_41) v_40 v_41) (concat (mux (ult v_42 v_43) v_42 v_43) (concat (mux (ult v_44 v_45) v_44 v_45) (concat (mux (ult v_46 v_47) v_46 v_47) (concat (mux (ult v_48 v_49) v_48 v_49) (concat (mux (ult v_50 v_51) v_50 v_51) (concat (mux (ult v_52 v_53) v_52 v_53) (concat (mux (ult v_54 v_55) v_54 v_55) (concat (mux (ult v_56 v_57) v_56 v_57) (concat (mux (ult v_58 v_59) v_58 v_59) (concat (mux (ult v_60 v_61) v_60 v_61) (concat (mux (ult v_62 v_63) v_62 v_63) (concat (mux (ult v_64 v_65) v_64 v_65) (concat (mux (ult v_66 v_67) v_66 v_67) (mux (ult v_68 v_69) v_68 v_69))))))))))))))))))))))))))))))));
      pure ()
    pat_end;
    pattern fun (xmm_0 : reg (bv 128)) (xmm_1 : reg (bv 128)) (xmm_2 : reg (bv 128)) => do
      v_3 <- getRegister (lhs.of_reg xmm_1);
      v_4 <- eval (extract v_3 0 8);
      v_5 <- getRegister (lhs.of_reg xmm_0);
      v_6 <- eval (extract v_5 0 8);
      v_7 <- eval (extract v_3 8 16);
      v_8 <- eval (extract v_5 8 16);
      v_9 <- eval (extract v_3 16 24);
      v_10 <- eval (extract v_5 16 24);
      v_11 <- eval (extract v_3 24 32);
      v_12 <- eval (extract v_5 24 32);
      v_13 <- eval (extract v_3 32 40);
      v_14 <- eval (extract v_5 32 40);
      v_15 <- eval (extract v_3 40 48);
      v_16 <- eval (extract v_5 40 48);
      v_17 <- eval (extract v_3 48 56);
      v_18 <- eval (extract v_5 48 56);
      v_19 <- eval (extract v_3 56 64);
      v_20 <- eval (extract v_5 56 64);
      v_21 <- eval (extract v_3 64 72);
      v_22 <- eval (extract v_5 64 72);
      v_23 <- eval (extract v_3 72 80);
      v_24 <- eval (extract v_5 72 80);
      v_25 <- eval (extract v_3 80 88);
      v_26 <- eval (extract v_5 80 88);
      v_27 <- eval (extract v_3 88 96);
      v_28 <- eval (extract v_5 88 96);
      v_29 <- eval (extract v_3 96 104);
      v_30 <- eval (extract v_5 96 104);
      v_31 <- eval (extract v_3 104 112);
      v_32 <- eval (extract v_5 104 112);
      v_33 <- eval (extract v_3 112 120);
      v_34 <- eval (extract v_5 112 120);
      v_35 <- eval (extract v_3 120 128);
      v_36 <- eval (extract v_5 120 128);
      setRegister (lhs.of_reg xmm_2) (concat (mux (ult v_4 v_6) v_4 v_6) (concat (mux (ult v_7 v_8) v_7 v_8) (concat (mux (ult v_9 v_10) v_9 v_10) (concat (mux (ult v_11 v_12) v_11 v_12) (concat (mux (ult v_13 v_14) v_13 v_14) (concat (mux (ult v_15 v_16) v_15 v_16) (concat (mux (ult v_17 v_18) v_17 v_18) (concat (mux (ult v_19 v_20) v_19 v_20) (concat (mux (ult v_21 v_22) v_21 v_22) (concat (mux (ult v_23 v_24) v_23 v_24) (concat (mux (ult v_25 v_26) v_25 v_26) (concat (mux (ult v_27 v_28) v_27 v_28) (concat (mux (ult v_29 v_30) v_29 v_30) (concat (mux (ult v_31 v_32) v_31 v_32) (concat (mux (ult v_33 v_34) v_33 v_34) (mux (ult v_35 v_36) v_35 v_36))))))))))))))));
      pure ()
    pat_end;
    pattern fun (ymm_0 : reg (bv 256)) (ymm_1 : reg (bv 256)) (ymm_2 : reg (bv 256)) => do
      v_3 <- getRegister (lhs.of_reg ymm_1);
      v_4 <- eval (extract v_3 0 8);
      v_5 <- getRegister (lhs.of_reg ymm_0);
      v_6 <- eval (extract v_5 0 8);
      v_7 <- eval (extract v_3 8 16);
      v_8 <- eval (extract v_5 8 16);
      v_9 <- eval (extract v_3 16 24);
      v_10 <- eval (extract v_5 16 24);
      v_11 <- eval (extract v_3 24 32);
      v_12 <- eval (extract v_5 24 32);
      v_13 <- eval (extract v_3 32 40);
      v_14 <- eval (extract v_5 32 40);
      v_15 <- eval (extract v_3 40 48);
      v_16 <- eval (extract v_5 40 48);
      v_17 <- eval (extract v_3 48 56);
      v_18 <- eval (extract v_5 48 56);
      v_19 <- eval (extract v_3 56 64);
      v_20 <- eval (extract v_5 56 64);
      v_21 <- eval (extract v_3 64 72);
      v_22 <- eval (extract v_5 64 72);
      v_23 <- eval (extract v_3 72 80);
      v_24 <- eval (extract v_5 72 80);
      v_25 <- eval (extract v_3 80 88);
      v_26 <- eval (extract v_5 80 88);
      v_27 <- eval (extract v_3 88 96);
      v_28 <- eval (extract v_5 88 96);
      v_29 <- eval (extract v_3 96 104);
      v_30 <- eval (extract v_5 96 104);
      v_31 <- eval (extract v_3 104 112);
      v_32 <- eval (extract v_5 104 112);
      v_33 <- eval (extract v_3 112 120);
      v_34 <- eval (extract v_5 112 120);
      v_35 <- eval (extract v_3 120 128);
      v_36 <- eval (extract v_5 120 128);
      v_37 <- eval (extract v_3 128 136);
      v_38 <- eval (extract v_5 128 136);
      v_39 <- eval (extract v_3 136 144);
      v_40 <- eval (extract v_5 136 144);
      v_41 <- eval (extract v_3 144 152);
      v_42 <- eval (extract v_5 144 152);
      v_43 <- eval (extract v_3 152 160);
      v_44 <- eval (extract v_5 152 160);
      v_45 <- eval (extract v_3 160 168);
      v_46 <- eval (extract v_5 160 168);
      v_47 <- eval (extract v_3 168 176);
      v_48 <- eval (extract v_5 168 176);
      v_49 <- eval (extract v_3 176 184);
      v_50 <- eval (extract v_5 176 184);
      v_51 <- eval (extract v_3 184 192);
      v_52 <- eval (extract v_5 184 192);
      v_53 <- eval (extract v_3 192 200);
      v_54 <- eval (extract v_5 192 200);
      v_55 <- eval (extract v_3 200 208);
      v_56 <- eval (extract v_5 200 208);
      v_57 <- eval (extract v_3 208 216);
      v_58 <- eval (extract v_5 208 216);
      v_59 <- eval (extract v_3 216 224);
      v_60 <- eval (extract v_5 216 224);
      v_61 <- eval (extract v_3 224 232);
      v_62 <- eval (extract v_5 224 232);
      v_63 <- eval (extract v_3 232 240);
      v_64 <- eval (extract v_5 232 240);
      v_65 <- eval (extract v_3 240 248);
      v_66 <- eval (extract v_5 240 248);
      v_67 <- eval (extract v_3 248 256);
      v_68 <- eval (extract v_5 248 256);
      setRegister (lhs.of_reg ymm_2) (concat (mux (ult v_4 v_6) v_4 v_6) (concat (mux (ult v_7 v_8) v_7 v_8) (concat (mux (ult v_9 v_10) v_9 v_10) (concat (mux (ult v_11 v_12) v_11 v_12) (concat (mux (ult v_13 v_14) v_13 v_14) (concat (mux (ult v_15 v_16) v_15 v_16) (concat (mux (ult v_17 v_18) v_17 v_18) (concat (mux (ult v_19 v_20) v_19 v_20) (concat (mux (ult v_21 v_22) v_21 v_22) (concat (mux (ult v_23 v_24) v_23 v_24) (concat (mux (ult v_25 v_26) v_25 v_26) (concat (mux (ult v_27 v_28) v_27 v_28) (concat (mux (ult v_29 v_30) v_29 v_30) (concat (mux (ult v_31 v_32) v_31 v_32) (concat (mux (ult v_33 v_34) v_33 v_34) (concat (mux (ult v_35 v_36) v_35 v_36) (concat (mux (ult v_37 v_38) v_37 v_38) (concat (mux (ult v_39 v_40) v_39 v_40) (concat (mux (ult v_41 v_42) v_41 v_42) (concat (mux (ult v_43 v_44) v_43 v_44) (concat (mux (ult v_45 v_46) v_45 v_46) (concat (mux (ult v_47 v_48) v_47 v_48) (concat (mux (ult v_49 v_50) v_49 v_50) (concat (mux (ult v_51 v_52) v_51 v_52) (concat (mux (ult v_53 v_54) v_53 v_54) (concat (mux (ult v_55 v_56) v_55 v_56) (concat (mux (ult v_57 v_58) v_57 v_58) (concat (mux (ult v_59 v_60) v_59 v_60) (concat (mux (ult v_61 v_62) v_61 v_62) (concat (mux (ult v_63 v_64) v_63 v_64) (concat (mux (ult v_65 v_66) v_65 v_66) (mux (ult v_67 v_68) v_67 v_68))))))))))))))))))))))))))))))));
      pure ()
    pat_end
