def vpminuw1 : instruction :=
  definst "vpminuw" $ do
    pattern fun (v_2615 : reg (bv 128)) (v_2616 : reg (bv 128)) (v_2617 : reg (bv 128)) => do
      v_5136 <- getRegister v_2616;
      v_5137 <- eval (extract v_5136 0 16);
      v_5138 <- getRegister v_2615;
      v_5139 <- eval (extract v_5138 0 16);
      v_5142 <- eval (extract v_5136 16 32);
      v_5143 <- eval (extract v_5138 16 32);
      v_5146 <- eval (extract v_5136 32 48);
      v_5147 <- eval (extract v_5138 32 48);
      v_5150 <- eval (extract v_5136 48 64);
      v_5151 <- eval (extract v_5138 48 64);
      v_5154 <- eval (extract v_5136 64 80);
      v_5155 <- eval (extract v_5138 64 80);
      v_5158 <- eval (extract v_5136 80 96);
      v_5159 <- eval (extract v_5138 80 96);
      v_5162 <- eval (extract v_5136 96 112);
      v_5163 <- eval (extract v_5138 96 112);
      v_5166 <- eval (extract v_5136 112 128);
      v_5167 <- eval (extract v_5138 112 128);
      setRegister (lhs.of_reg v_2617) (concat (mux (ult v_5137 v_5139) v_5137 v_5139) (concat (mux (ult v_5142 v_5143) v_5142 v_5143) (concat (mux (ult v_5146 v_5147) v_5146 v_5147) (concat (mux (ult v_5150 v_5151) v_5150 v_5151) (concat (mux (ult v_5154 v_5155) v_5154 v_5155) (concat (mux (ult v_5158 v_5159) v_5158 v_5159) (concat (mux (ult v_5162 v_5163) v_5162 v_5163) (mux (ult v_5166 v_5167) v_5166 v_5167))))))));
      pure ()
    pat_end;
    pattern fun (v_2626 : reg (bv 256)) (v_2627 : reg (bv 256)) (v_2628 : reg (bv 256)) => do
      v_5183 <- getRegister v_2627;
      v_5184 <- eval (extract v_5183 0 16);
      v_5185 <- getRegister v_2626;
      v_5186 <- eval (extract v_5185 0 16);
      v_5189 <- eval (extract v_5183 16 32);
      v_5190 <- eval (extract v_5185 16 32);
      v_5193 <- eval (extract v_5183 32 48);
      v_5194 <- eval (extract v_5185 32 48);
      v_5197 <- eval (extract v_5183 48 64);
      v_5198 <- eval (extract v_5185 48 64);
      v_5201 <- eval (extract v_5183 64 80);
      v_5202 <- eval (extract v_5185 64 80);
      v_5205 <- eval (extract v_5183 80 96);
      v_5206 <- eval (extract v_5185 80 96);
      v_5209 <- eval (extract v_5183 96 112);
      v_5210 <- eval (extract v_5185 96 112);
      v_5213 <- eval (extract v_5183 112 128);
      v_5214 <- eval (extract v_5185 112 128);
      v_5217 <- eval (extract v_5183 128 144);
      v_5218 <- eval (extract v_5185 128 144);
      v_5221 <- eval (extract v_5183 144 160);
      v_5222 <- eval (extract v_5185 144 160);
      v_5225 <- eval (extract v_5183 160 176);
      v_5226 <- eval (extract v_5185 160 176);
      v_5229 <- eval (extract v_5183 176 192);
      v_5230 <- eval (extract v_5185 176 192);
      v_5233 <- eval (extract v_5183 192 208);
      v_5234 <- eval (extract v_5185 192 208);
      v_5237 <- eval (extract v_5183 208 224);
      v_5238 <- eval (extract v_5185 208 224);
      v_5241 <- eval (extract v_5183 224 240);
      v_5242 <- eval (extract v_5185 224 240);
      v_5245 <- eval (extract v_5183 240 256);
      v_5246 <- eval (extract v_5185 240 256);
      setRegister (lhs.of_reg v_2628) (concat (mux (ult v_5184 v_5186) v_5184 v_5186) (concat (mux (ult v_5189 v_5190) v_5189 v_5190) (concat (mux (ult v_5193 v_5194) v_5193 v_5194) (concat (mux (ult v_5197 v_5198) v_5197 v_5198) (concat (mux (ult v_5201 v_5202) v_5201 v_5202) (concat (mux (ult v_5205 v_5206) v_5205 v_5206) (concat (mux (ult v_5209 v_5210) v_5209 v_5210) (concat (mux (ult v_5213 v_5214) v_5213 v_5214) (concat (mux (ult v_5217 v_5218) v_5217 v_5218) (concat (mux (ult v_5221 v_5222) v_5221 v_5222) (concat (mux (ult v_5225 v_5226) v_5225 v_5226) (concat (mux (ult v_5229 v_5230) v_5229 v_5230) (concat (mux (ult v_5233 v_5234) v_5233 v_5234) (concat (mux (ult v_5237 v_5238) v_5237 v_5238) (concat (mux (ult v_5241 v_5242) v_5241 v_5242) (mux (ult v_5245 v_5246) v_5245 v_5246))))))))))))))));
      pure ()
    pat_end;
    pattern fun (v_2609 : Mem) (v_2610 : reg (bv 128)) (v_2611 : reg (bv 128)) => do
      v_11773 <- getRegister v_2610;
      v_11774 <- eval (extract v_11773 0 16);
      v_11775 <- evaluateAddress v_2609;
      v_11776 <- load v_11775 16;
      v_11777 <- eval (extract v_11776 0 16);
      v_11780 <- eval (extract v_11773 16 32);
      v_11781 <- eval (extract v_11776 16 32);
      v_11784 <- eval (extract v_11773 32 48);
      v_11785 <- eval (extract v_11776 32 48);
      v_11788 <- eval (extract v_11773 48 64);
      v_11789 <- eval (extract v_11776 48 64);
      v_11792 <- eval (extract v_11773 64 80);
      v_11793 <- eval (extract v_11776 64 80);
      v_11796 <- eval (extract v_11773 80 96);
      v_11797 <- eval (extract v_11776 80 96);
      v_11800 <- eval (extract v_11773 96 112);
      v_11801 <- eval (extract v_11776 96 112);
      v_11804 <- eval (extract v_11773 112 128);
      v_11805 <- eval (extract v_11776 112 128);
      setRegister (lhs.of_reg v_2611) (concat (mux (ult v_11774 v_11777) v_11774 v_11777) (concat (mux (ult v_11780 v_11781) v_11780 v_11781) (concat (mux (ult v_11784 v_11785) v_11784 v_11785) (concat (mux (ult v_11788 v_11789) v_11788 v_11789) (concat (mux (ult v_11792 v_11793) v_11792 v_11793) (concat (mux (ult v_11796 v_11797) v_11796 v_11797) (concat (mux (ult v_11800 v_11801) v_11800 v_11801) (mux (ult v_11804 v_11805) v_11804 v_11805))))))));
      pure ()
    pat_end;
    pattern fun (v_2620 : Mem) (v_2621 : reg (bv 256)) (v_2622 : reg (bv 256)) => do
      v_11816 <- getRegister v_2621;
      v_11817 <- eval (extract v_11816 0 16);
      v_11818 <- evaluateAddress v_2620;
      v_11819 <- load v_11818 32;
      v_11820 <- eval (extract v_11819 0 16);
      v_11823 <- eval (extract v_11816 16 32);
      v_11824 <- eval (extract v_11819 16 32);
      v_11827 <- eval (extract v_11816 32 48);
      v_11828 <- eval (extract v_11819 32 48);
      v_11831 <- eval (extract v_11816 48 64);
      v_11832 <- eval (extract v_11819 48 64);
      v_11835 <- eval (extract v_11816 64 80);
      v_11836 <- eval (extract v_11819 64 80);
      v_11839 <- eval (extract v_11816 80 96);
      v_11840 <- eval (extract v_11819 80 96);
      v_11843 <- eval (extract v_11816 96 112);
      v_11844 <- eval (extract v_11819 96 112);
      v_11847 <- eval (extract v_11816 112 128);
      v_11848 <- eval (extract v_11819 112 128);
      v_11851 <- eval (extract v_11816 128 144);
      v_11852 <- eval (extract v_11819 128 144);
      v_11855 <- eval (extract v_11816 144 160);
      v_11856 <- eval (extract v_11819 144 160);
      v_11859 <- eval (extract v_11816 160 176);
      v_11860 <- eval (extract v_11819 160 176);
      v_11863 <- eval (extract v_11816 176 192);
      v_11864 <- eval (extract v_11819 176 192);
      v_11867 <- eval (extract v_11816 192 208);
      v_11868 <- eval (extract v_11819 192 208);
      v_11871 <- eval (extract v_11816 208 224);
      v_11872 <- eval (extract v_11819 208 224);
      v_11875 <- eval (extract v_11816 224 240);
      v_11876 <- eval (extract v_11819 224 240);
      v_11879 <- eval (extract v_11816 240 256);
      v_11880 <- eval (extract v_11819 240 256);
      setRegister (lhs.of_reg v_2622) (concat (mux (ult v_11817 v_11820) v_11817 v_11820) (concat (mux (ult v_11823 v_11824) v_11823 v_11824) (concat (mux (ult v_11827 v_11828) v_11827 v_11828) (concat (mux (ult v_11831 v_11832) v_11831 v_11832) (concat (mux (ult v_11835 v_11836) v_11835 v_11836) (concat (mux (ult v_11839 v_11840) v_11839 v_11840) (concat (mux (ult v_11843 v_11844) v_11843 v_11844) (concat (mux (ult v_11847 v_11848) v_11847 v_11848) (concat (mux (ult v_11851 v_11852) v_11851 v_11852) (concat (mux (ult v_11855 v_11856) v_11855 v_11856) (concat (mux (ult v_11859 v_11860) v_11859 v_11860) (concat (mux (ult v_11863 v_11864) v_11863 v_11864) (concat (mux (ult v_11867 v_11868) v_11867 v_11868) (concat (mux (ult v_11871 v_11872) v_11871 v_11872) (concat (mux (ult v_11875 v_11876) v_11875 v_11876) (mux (ult v_11879 v_11880) v_11879 v_11880))))))))))))))));
      pure ()
    pat_end
