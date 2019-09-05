def vpsubusw1 : instruction :=
  definst "vpsubusw" $ do
    pattern fun (v_2582 : reg (bv 128)) (v_2583 : reg (bv 128)) (v_2584 : reg (bv 128)) => do
      v_5647 <- getRegister v_2583;
      v_5650 <- getRegister v_2582;
      v_5653 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_5647 0 16)) (concat (expression.bv_nat 2 0) (extract v_5650 0 16)));
      v_5663 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_5647 16 32)) (concat (expression.bv_nat 2 0) (extract v_5650 16 32)));
      v_5673 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_5647 32 48)) (concat (expression.bv_nat 2 0) (extract v_5650 32 48)));
      v_5683 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_5647 48 64)) (concat (expression.bv_nat 2 0) (extract v_5650 48 64)));
      v_5693 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_5647 64 80)) (concat (expression.bv_nat 2 0) (extract v_5650 64 80)));
      v_5703 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_5647 80 96)) (concat (expression.bv_nat 2 0) (extract v_5650 80 96)));
      v_5713 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_5647 96 112)) (concat (expression.bv_nat 2 0) (extract v_5650 96 112)));
      v_5723 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_5647 112 128)) (concat (expression.bv_nat 2 0) (extract v_5650 112 128)));
      setRegister (lhs.of_reg v_2584) (concat (mux (sgt v_5653 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_5653 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_5653 2 18))) (concat (mux (sgt v_5663 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_5663 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_5663 2 18))) (concat (mux (sgt v_5673 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_5673 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_5673 2 18))) (concat (mux (sgt v_5683 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_5683 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_5683 2 18))) (concat (mux (sgt v_5693 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_5693 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_5693 2 18))) (concat (mux (sgt v_5703 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_5703 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_5703 2 18))) (concat (mux (sgt v_5713 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_5713 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_5713 2 18))) (mux (sgt v_5723 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_5723 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_5723 2 18))))))))));
      pure ()
    pat_end;
    pattern fun (v_2593 : reg (bv 256)) (v_2594 : reg (bv 256)) (v_2595 : reg (bv 256)) => do
      v_5742 <- getRegister v_2594;
      v_5745 <- getRegister v_2593;
      v_5748 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_5742 0 16)) (concat (expression.bv_nat 2 0) (extract v_5745 0 16)));
      v_5758 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_5742 16 32)) (concat (expression.bv_nat 2 0) (extract v_5745 16 32)));
      v_5768 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_5742 32 48)) (concat (expression.bv_nat 2 0) (extract v_5745 32 48)));
      v_5778 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_5742 48 64)) (concat (expression.bv_nat 2 0) (extract v_5745 48 64)));
      v_5788 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_5742 64 80)) (concat (expression.bv_nat 2 0) (extract v_5745 64 80)));
      v_5798 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_5742 80 96)) (concat (expression.bv_nat 2 0) (extract v_5745 80 96)));
      v_5808 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_5742 96 112)) (concat (expression.bv_nat 2 0) (extract v_5745 96 112)));
      v_5818 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_5742 112 128)) (concat (expression.bv_nat 2 0) (extract v_5745 112 128)));
      v_5828 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_5742 128 144)) (concat (expression.bv_nat 2 0) (extract v_5745 128 144)));
      v_5838 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_5742 144 160)) (concat (expression.bv_nat 2 0) (extract v_5745 144 160)));
      v_5848 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_5742 160 176)) (concat (expression.bv_nat 2 0) (extract v_5745 160 176)));
      v_5858 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_5742 176 192)) (concat (expression.bv_nat 2 0) (extract v_5745 176 192)));
      v_5868 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_5742 192 208)) (concat (expression.bv_nat 2 0) (extract v_5745 192 208)));
      v_5878 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_5742 208 224)) (concat (expression.bv_nat 2 0) (extract v_5745 208 224)));
      v_5888 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_5742 224 240)) (concat (expression.bv_nat 2 0) (extract v_5745 224 240)));
      v_5898 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_5742 240 256)) (concat (expression.bv_nat 2 0) (extract v_5745 240 256)));
      setRegister (lhs.of_reg v_2595) (concat (mux (sgt v_5748 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_5748 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_5748 2 18))) (concat (mux (sgt v_5758 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_5758 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_5758 2 18))) (concat (mux (sgt v_5768 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_5768 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_5768 2 18))) (concat (mux (sgt v_5778 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_5778 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_5778 2 18))) (concat (mux (sgt v_5788 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_5788 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_5788 2 18))) (concat (mux (sgt v_5798 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_5798 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_5798 2 18))) (concat (mux (sgt v_5808 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_5808 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_5808 2 18))) (concat (mux (sgt v_5818 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_5818 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_5818 2 18))) (concat (mux (sgt v_5828 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_5828 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_5828 2 18))) (concat (mux (sgt v_5838 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_5838 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_5838 2 18))) (concat (mux (sgt v_5848 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_5848 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_5848 2 18))) (concat (mux (sgt v_5858 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_5858 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_5858 2 18))) (concat (mux (sgt v_5868 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_5868 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_5868 2 18))) (concat (mux (sgt v_5878 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_5878 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_5878 2 18))) (concat (mux (sgt v_5888 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_5888 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_5888 2 18))) (mux (sgt v_5898 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_5898 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_5898 2 18))))))))))))))))));
      pure ()
    pat_end;
    pattern fun (v_2576 : Mem) (v_2577 : reg (bv 128)) (v_2578 : reg (bv 128)) => do
      v_11759 <- getRegister v_2577;
      v_11762 <- evaluateAddress v_2576;
      v_11763 <- load v_11762 16;
      v_11766 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_11759 0 16)) (concat (expression.bv_nat 2 0) (extract v_11763 0 16)));
      v_11776 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_11759 16 32)) (concat (expression.bv_nat 2 0) (extract v_11763 16 32)));
      v_11786 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_11759 32 48)) (concat (expression.bv_nat 2 0) (extract v_11763 32 48)));
      v_11796 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_11759 48 64)) (concat (expression.bv_nat 2 0) (extract v_11763 48 64)));
      v_11806 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_11759 64 80)) (concat (expression.bv_nat 2 0) (extract v_11763 64 80)));
      v_11816 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_11759 80 96)) (concat (expression.bv_nat 2 0) (extract v_11763 80 96)));
      v_11826 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_11759 96 112)) (concat (expression.bv_nat 2 0) (extract v_11763 96 112)));
      v_11836 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_11759 112 128)) (concat (expression.bv_nat 2 0) (extract v_11763 112 128)));
      setRegister (lhs.of_reg v_2578) (concat (mux (sgt v_11766 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_11766 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_11766 2 18))) (concat (mux (sgt v_11776 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_11776 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_11776 2 18))) (concat (mux (sgt v_11786 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_11786 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_11786 2 18))) (concat (mux (sgt v_11796 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_11796 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_11796 2 18))) (concat (mux (sgt v_11806 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_11806 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_11806 2 18))) (concat (mux (sgt v_11816 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_11816 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_11816 2 18))) (concat (mux (sgt v_11826 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_11826 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_11826 2 18))) (mux (sgt v_11836 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_11836 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_11836 2 18))))))))));
      pure ()
    pat_end;
    pattern fun (v_2587 : Mem) (v_2588 : reg (bv 256)) (v_2589 : reg (bv 256)) => do
      v_11850 <- getRegister v_2588;
      v_11853 <- evaluateAddress v_2587;
      v_11854 <- load v_11853 32;
      v_11857 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_11850 0 16)) (concat (expression.bv_nat 2 0) (extract v_11854 0 16)));
      v_11867 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_11850 16 32)) (concat (expression.bv_nat 2 0) (extract v_11854 16 32)));
      v_11877 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_11850 32 48)) (concat (expression.bv_nat 2 0) (extract v_11854 32 48)));
      v_11887 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_11850 48 64)) (concat (expression.bv_nat 2 0) (extract v_11854 48 64)));
      v_11897 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_11850 64 80)) (concat (expression.bv_nat 2 0) (extract v_11854 64 80)));
      v_11907 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_11850 80 96)) (concat (expression.bv_nat 2 0) (extract v_11854 80 96)));
      v_11917 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_11850 96 112)) (concat (expression.bv_nat 2 0) (extract v_11854 96 112)));
      v_11927 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_11850 112 128)) (concat (expression.bv_nat 2 0) (extract v_11854 112 128)));
      v_11937 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_11850 128 144)) (concat (expression.bv_nat 2 0) (extract v_11854 128 144)));
      v_11947 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_11850 144 160)) (concat (expression.bv_nat 2 0) (extract v_11854 144 160)));
      v_11957 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_11850 160 176)) (concat (expression.bv_nat 2 0) (extract v_11854 160 176)));
      v_11967 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_11850 176 192)) (concat (expression.bv_nat 2 0) (extract v_11854 176 192)));
      v_11977 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_11850 192 208)) (concat (expression.bv_nat 2 0) (extract v_11854 192 208)));
      v_11987 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_11850 208 224)) (concat (expression.bv_nat 2 0) (extract v_11854 208 224)));
      v_11997 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_11850 224 240)) (concat (expression.bv_nat 2 0) (extract v_11854 224 240)));
      v_12007 <- eval (sub (concat (expression.bv_nat 2 0) (extract v_11850 240 256)) (concat (expression.bv_nat 2 0) (extract v_11854 240 256)));
      setRegister (lhs.of_reg v_2589) (concat (mux (sgt v_11857 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_11857 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_11857 2 18))) (concat (mux (sgt v_11867 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_11867 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_11867 2 18))) (concat (mux (sgt v_11877 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_11877 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_11877 2 18))) (concat (mux (sgt v_11887 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_11887 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_11887 2 18))) (concat (mux (sgt v_11897 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_11897 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_11897 2 18))) (concat (mux (sgt v_11907 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_11907 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_11907 2 18))) (concat (mux (sgt v_11917 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_11917 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_11917 2 18))) (concat (mux (sgt v_11927 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_11927 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_11927 2 18))) (concat (mux (sgt v_11937 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_11937 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_11937 2 18))) (concat (mux (sgt v_11947 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_11947 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_11947 2 18))) (concat (mux (sgt v_11957 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_11957 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_11957 2 18))) (concat (mux (sgt v_11967 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_11967 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_11967 2 18))) (concat (mux (sgt v_11977 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_11977 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_11977 2 18))) (concat (mux (sgt v_11987 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_11987 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_11987 2 18))) (concat (mux (sgt v_11997 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_11997 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_11997 2 18))) (mux (sgt v_12007 (expression.bv_nat 18 65535)) (expression.bv_nat 16 65535) (mux (slt v_12007 (expression.bv_nat 18 0)) (expression.bv_nat 16 0) (extract v_12007 2 18))))))))))))))))));
      pure ()
    pat_end
