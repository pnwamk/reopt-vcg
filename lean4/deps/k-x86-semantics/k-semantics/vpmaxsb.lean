def vpmaxsb1 : instruction :=
  definst "vpmaxsb" $ do
    pattern fun (v_3491 : reg (bv 128)) (v_3492 : reg (bv 128)) (v_3493 : reg (bv 128)) => do
      v_10359 <- getRegister v_3492;
      v_10360 <- eval (extract v_10359 0 8);
      v_10361 <- getRegister v_3491;
      v_10362 <- eval (extract v_10361 0 8);
      v_10365 <- eval (extract v_10359 8 16);
      v_10366 <- eval (extract v_10361 8 16);
      v_10369 <- eval (extract v_10359 16 24);
      v_10370 <- eval (extract v_10361 16 24);
      v_10373 <- eval (extract v_10359 24 32);
      v_10374 <- eval (extract v_10361 24 32);
      v_10377 <- eval (extract v_10359 32 40);
      v_10378 <- eval (extract v_10361 32 40);
      v_10381 <- eval (extract v_10359 40 48);
      v_10382 <- eval (extract v_10361 40 48);
      v_10385 <- eval (extract v_10359 48 56);
      v_10386 <- eval (extract v_10361 48 56);
      v_10389 <- eval (extract v_10359 56 64);
      v_10390 <- eval (extract v_10361 56 64);
      v_10393 <- eval (extract v_10359 64 72);
      v_10394 <- eval (extract v_10361 64 72);
      v_10397 <- eval (extract v_10359 72 80);
      v_10398 <- eval (extract v_10361 72 80);
      v_10401 <- eval (extract v_10359 80 88);
      v_10402 <- eval (extract v_10361 80 88);
      v_10405 <- eval (extract v_10359 88 96);
      v_10406 <- eval (extract v_10361 88 96);
      v_10409 <- eval (extract v_10359 96 104);
      v_10410 <- eval (extract v_10361 96 104);
      v_10413 <- eval (extract v_10359 104 112);
      v_10414 <- eval (extract v_10361 104 112);
      v_10417 <- eval (extract v_10359 112 120);
      v_10418 <- eval (extract v_10361 112 120);
      v_10421 <- eval (extract v_10359 120 128);
      v_10422 <- eval (extract v_10361 120 128);
      setRegister (lhs.of_reg v_3493) (concat (mux (sgt v_10360 v_10362) v_10360 v_10362) (concat (mux (sgt v_10365 v_10366) v_10365 v_10366) (concat (mux (sgt v_10369 v_10370) v_10369 v_10370) (concat (mux (sgt v_10373 v_10374) v_10373 v_10374) (concat (mux (sgt v_10377 v_10378) v_10377 v_10378) (concat (mux (sgt v_10381 v_10382) v_10381 v_10382) (concat (mux (sgt v_10385 v_10386) v_10385 v_10386) (concat (mux (sgt v_10389 v_10390) v_10389 v_10390) (concat (mux (sgt v_10393 v_10394) v_10393 v_10394) (concat (mux (sgt v_10397 v_10398) v_10397 v_10398) (concat (mux (sgt v_10401 v_10402) v_10401 v_10402) (concat (mux (sgt v_10405 v_10406) v_10405 v_10406) (concat (mux (sgt v_10409 v_10410) v_10409 v_10410) (concat (mux (sgt v_10413 v_10414) v_10413 v_10414) (concat (mux (sgt v_10417 v_10418) v_10417 v_10418) (mux (sgt v_10421 v_10422) v_10421 v_10422))))))))))))))));
      pure ()
    pat_end;
    pattern fun (v_3501 : reg (bv 256)) (v_3502 : reg (bv 256)) (v_3503 : reg (bv 256)) => do
      v_10446 <- getRegister v_3502;
      v_10447 <- eval (extract v_10446 0 8);
      v_10448 <- getRegister v_3501;
      v_10449 <- eval (extract v_10448 0 8);
      v_10452 <- eval (extract v_10446 8 16);
      v_10453 <- eval (extract v_10448 8 16);
      v_10456 <- eval (extract v_10446 16 24);
      v_10457 <- eval (extract v_10448 16 24);
      v_10460 <- eval (extract v_10446 24 32);
      v_10461 <- eval (extract v_10448 24 32);
      v_10464 <- eval (extract v_10446 32 40);
      v_10465 <- eval (extract v_10448 32 40);
      v_10468 <- eval (extract v_10446 40 48);
      v_10469 <- eval (extract v_10448 40 48);
      v_10472 <- eval (extract v_10446 48 56);
      v_10473 <- eval (extract v_10448 48 56);
      v_10476 <- eval (extract v_10446 56 64);
      v_10477 <- eval (extract v_10448 56 64);
      v_10480 <- eval (extract v_10446 64 72);
      v_10481 <- eval (extract v_10448 64 72);
      v_10484 <- eval (extract v_10446 72 80);
      v_10485 <- eval (extract v_10448 72 80);
      v_10488 <- eval (extract v_10446 80 88);
      v_10489 <- eval (extract v_10448 80 88);
      v_10492 <- eval (extract v_10446 88 96);
      v_10493 <- eval (extract v_10448 88 96);
      v_10496 <- eval (extract v_10446 96 104);
      v_10497 <- eval (extract v_10448 96 104);
      v_10500 <- eval (extract v_10446 104 112);
      v_10501 <- eval (extract v_10448 104 112);
      v_10504 <- eval (extract v_10446 112 120);
      v_10505 <- eval (extract v_10448 112 120);
      v_10508 <- eval (extract v_10446 120 128);
      v_10509 <- eval (extract v_10448 120 128);
      v_10512 <- eval (extract v_10446 128 136);
      v_10513 <- eval (extract v_10448 128 136);
      v_10516 <- eval (extract v_10446 136 144);
      v_10517 <- eval (extract v_10448 136 144);
      v_10520 <- eval (extract v_10446 144 152);
      v_10521 <- eval (extract v_10448 144 152);
      v_10524 <- eval (extract v_10446 152 160);
      v_10525 <- eval (extract v_10448 152 160);
      v_10528 <- eval (extract v_10446 160 168);
      v_10529 <- eval (extract v_10448 160 168);
      v_10532 <- eval (extract v_10446 168 176);
      v_10533 <- eval (extract v_10448 168 176);
      v_10536 <- eval (extract v_10446 176 184);
      v_10537 <- eval (extract v_10448 176 184);
      v_10540 <- eval (extract v_10446 184 192);
      v_10541 <- eval (extract v_10448 184 192);
      v_10544 <- eval (extract v_10446 192 200);
      v_10545 <- eval (extract v_10448 192 200);
      v_10548 <- eval (extract v_10446 200 208);
      v_10549 <- eval (extract v_10448 200 208);
      v_10552 <- eval (extract v_10446 208 216);
      v_10553 <- eval (extract v_10448 208 216);
      v_10556 <- eval (extract v_10446 216 224);
      v_10557 <- eval (extract v_10448 216 224);
      v_10560 <- eval (extract v_10446 224 232);
      v_10561 <- eval (extract v_10448 224 232);
      v_10564 <- eval (extract v_10446 232 240);
      v_10565 <- eval (extract v_10448 232 240);
      v_10568 <- eval (extract v_10446 240 248);
      v_10569 <- eval (extract v_10448 240 248);
      v_10572 <- eval (extract v_10446 248 256);
      v_10573 <- eval (extract v_10448 248 256);
      setRegister (lhs.of_reg v_3503) (concat (mux (sgt v_10447 v_10449) v_10447 v_10449) (concat (mux (sgt v_10452 v_10453) v_10452 v_10453) (concat (mux (sgt v_10456 v_10457) v_10456 v_10457) (concat (mux (sgt v_10460 v_10461) v_10460 v_10461) (concat (mux (sgt v_10464 v_10465) v_10464 v_10465) (concat (mux (sgt v_10468 v_10469) v_10468 v_10469) (concat (mux (sgt v_10472 v_10473) v_10472 v_10473) (concat (mux (sgt v_10476 v_10477) v_10476 v_10477) (concat (mux (sgt v_10480 v_10481) v_10480 v_10481) (concat (mux (sgt v_10484 v_10485) v_10484 v_10485) (concat (mux (sgt v_10488 v_10489) v_10488 v_10489) (concat (mux (sgt v_10492 v_10493) v_10492 v_10493) (concat (mux (sgt v_10496 v_10497) v_10496 v_10497) (concat (mux (sgt v_10500 v_10501) v_10500 v_10501) (concat (mux (sgt v_10504 v_10505) v_10504 v_10505) (concat (mux (sgt v_10508 v_10509) v_10508 v_10509) (concat (mux (sgt v_10512 v_10513) v_10512 v_10513) (concat (mux (sgt v_10516 v_10517) v_10516 v_10517) (concat (mux (sgt v_10520 v_10521) v_10520 v_10521) (concat (mux (sgt v_10524 v_10525) v_10524 v_10525) (concat (mux (sgt v_10528 v_10529) v_10528 v_10529) (concat (mux (sgt v_10532 v_10533) v_10532 v_10533) (concat (mux (sgt v_10536 v_10537) v_10536 v_10537) (concat (mux (sgt v_10540 v_10541) v_10540 v_10541) (concat (mux (sgt v_10544 v_10545) v_10544 v_10545) (concat (mux (sgt v_10548 v_10549) v_10548 v_10549) (concat (mux (sgt v_10552 v_10553) v_10552 v_10553) (concat (mux (sgt v_10556 v_10557) v_10556 v_10557) (concat (mux (sgt v_10560 v_10561) v_10560 v_10561) (concat (mux (sgt v_10564 v_10565) v_10564 v_10565) (concat (mux (sgt v_10568 v_10569) v_10568 v_10569) (mux (sgt v_10572 v_10573) v_10572 v_10573))))))))))))))))))))))))))))))));
      pure ()
    pat_end;
    pattern fun (v_3485 : Mem) (v_3486 : reg (bv 128)) (v_3487 : reg (bv 128)) => do
      v_18740 <- getRegister v_3486;
      v_18741 <- eval (extract v_18740 0 8);
      v_18742 <- evaluateAddress v_3485;
      v_18743 <- load v_18742 16;
      v_18744 <- eval (extract v_18743 0 8);
      v_18747 <- eval (extract v_18740 8 16);
      v_18748 <- eval (extract v_18743 8 16);
      v_18751 <- eval (extract v_18740 16 24);
      v_18752 <- eval (extract v_18743 16 24);
      v_18755 <- eval (extract v_18740 24 32);
      v_18756 <- eval (extract v_18743 24 32);
      v_18759 <- eval (extract v_18740 32 40);
      v_18760 <- eval (extract v_18743 32 40);
      v_18763 <- eval (extract v_18740 40 48);
      v_18764 <- eval (extract v_18743 40 48);
      v_18767 <- eval (extract v_18740 48 56);
      v_18768 <- eval (extract v_18743 48 56);
      v_18771 <- eval (extract v_18740 56 64);
      v_18772 <- eval (extract v_18743 56 64);
      v_18775 <- eval (extract v_18740 64 72);
      v_18776 <- eval (extract v_18743 64 72);
      v_18779 <- eval (extract v_18740 72 80);
      v_18780 <- eval (extract v_18743 72 80);
      v_18783 <- eval (extract v_18740 80 88);
      v_18784 <- eval (extract v_18743 80 88);
      v_18787 <- eval (extract v_18740 88 96);
      v_18788 <- eval (extract v_18743 88 96);
      v_18791 <- eval (extract v_18740 96 104);
      v_18792 <- eval (extract v_18743 96 104);
      v_18795 <- eval (extract v_18740 104 112);
      v_18796 <- eval (extract v_18743 104 112);
      v_18799 <- eval (extract v_18740 112 120);
      v_18800 <- eval (extract v_18743 112 120);
      v_18803 <- eval (extract v_18740 120 128);
      v_18804 <- eval (extract v_18743 120 128);
      setRegister (lhs.of_reg v_3487) (concat (mux (sgt v_18741 v_18744) v_18741 v_18744) (concat (mux (sgt v_18747 v_18748) v_18747 v_18748) (concat (mux (sgt v_18751 v_18752) v_18751 v_18752) (concat (mux (sgt v_18755 v_18756) v_18755 v_18756) (concat (mux (sgt v_18759 v_18760) v_18759 v_18760) (concat (mux (sgt v_18763 v_18764) v_18763 v_18764) (concat (mux (sgt v_18767 v_18768) v_18767 v_18768) (concat (mux (sgt v_18771 v_18772) v_18771 v_18772) (concat (mux (sgt v_18775 v_18776) v_18775 v_18776) (concat (mux (sgt v_18779 v_18780) v_18779 v_18780) (concat (mux (sgt v_18783 v_18784) v_18783 v_18784) (concat (mux (sgt v_18787 v_18788) v_18787 v_18788) (concat (mux (sgt v_18791 v_18792) v_18791 v_18792) (concat (mux (sgt v_18795 v_18796) v_18795 v_18796) (concat (mux (sgt v_18799 v_18800) v_18799 v_18800) (mux (sgt v_18803 v_18804) v_18803 v_18804))))))))))))))));
      pure ()
    pat_end;
    pattern fun (v_3496 : Mem) (v_3497 : reg (bv 256)) (v_3498 : reg (bv 256)) => do
      v_18823 <- getRegister v_3497;
      v_18824 <- eval (extract v_18823 0 8);
      v_18825 <- evaluateAddress v_3496;
      v_18826 <- load v_18825 32;
      v_18827 <- eval (extract v_18826 0 8);
      v_18830 <- eval (extract v_18823 8 16);
      v_18831 <- eval (extract v_18826 8 16);
      v_18834 <- eval (extract v_18823 16 24);
      v_18835 <- eval (extract v_18826 16 24);
      v_18838 <- eval (extract v_18823 24 32);
      v_18839 <- eval (extract v_18826 24 32);
      v_18842 <- eval (extract v_18823 32 40);
      v_18843 <- eval (extract v_18826 32 40);
      v_18846 <- eval (extract v_18823 40 48);
      v_18847 <- eval (extract v_18826 40 48);
      v_18850 <- eval (extract v_18823 48 56);
      v_18851 <- eval (extract v_18826 48 56);
      v_18854 <- eval (extract v_18823 56 64);
      v_18855 <- eval (extract v_18826 56 64);
      v_18858 <- eval (extract v_18823 64 72);
      v_18859 <- eval (extract v_18826 64 72);
      v_18862 <- eval (extract v_18823 72 80);
      v_18863 <- eval (extract v_18826 72 80);
      v_18866 <- eval (extract v_18823 80 88);
      v_18867 <- eval (extract v_18826 80 88);
      v_18870 <- eval (extract v_18823 88 96);
      v_18871 <- eval (extract v_18826 88 96);
      v_18874 <- eval (extract v_18823 96 104);
      v_18875 <- eval (extract v_18826 96 104);
      v_18878 <- eval (extract v_18823 104 112);
      v_18879 <- eval (extract v_18826 104 112);
      v_18882 <- eval (extract v_18823 112 120);
      v_18883 <- eval (extract v_18826 112 120);
      v_18886 <- eval (extract v_18823 120 128);
      v_18887 <- eval (extract v_18826 120 128);
      v_18890 <- eval (extract v_18823 128 136);
      v_18891 <- eval (extract v_18826 128 136);
      v_18894 <- eval (extract v_18823 136 144);
      v_18895 <- eval (extract v_18826 136 144);
      v_18898 <- eval (extract v_18823 144 152);
      v_18899 <- eval (extract v_18826 144 152);
      v_18902 <- eval (extract v_18823 152 160);
      v_18903 <- eval (extract v_18826 152 160);
      v_18906 <- eval (extract v_18823 160 168);
      v_18907 <- eval (extract v_18826 160 168);
      v_18910 <- eval (extract v_18823 168 176);
      v_18911 <- eval (extract v_18826 168 176);
      v_18914 <- eval (extract v_18823 176 184);
      v_18915 <- eval (extract v_18826 176 184);
      v_18918 <- eval (extract v_18823 184 192);
      v_18919 <- eval (extract v_18826 184 192);
      v_18922 <- eval (extract v_18823 192 200);
      v_18923 <- eval (extract v_18826 192 200);
      v_18926 <- eval (extract v_18823 200 208);
      v_18927 <- eval (extract v_18826 200 208);
      v_18930 <- eval (extract v_18823 208 216);
      v_18931 <- eval (extract v_18826 208 216);
      v_18934 <- eval (extract v_18823 216 224);
      v_18935 <- eval (extract v_18826 216 224);
      v_18938 <- eval (extract v_18823 224 232);
      v_18939 <- eval (extract v_18826 224 232);
      v_18942 <- eval (extract v_18823 232 240);
      v_18943 <- eval (extract v_18826 232 240);
      v_18946 <- eval (extract v_18823 240 248);
      v_18947 <- eval (extract v_18826 240 248);
      v_18950 <- eval (extract v_18823 248 256);
      v_18951 <- eval (extract v_18826 248 256);
      setRegister (lhs.of_reg v_3498) (concat (mux (sgt v_18824 v_18827) v_18824 v_18827) (concat (mux (sgt v_18830 v_18831) v_18830 v_18831) (concat (mux (sgt v_18834 v_18835) v_18834 v_18835) (concat (mux (sgt v_18838 v_18839) v_18838 v_18839) (concat (mux (sgt v_18842 v_18843) v_18842 v_18843) (concat (mux (sgt v_18846 v_18847) v_18846 v_18847) (concat (mux (sgt v_18850 v_18851) v_18850 v_18851) (concat (mux (sgt v_18854 v_18855) v_18854 v_18855) (concat (mux (sgt v_18858 v_18859) v_18858 v_18859) (concat (mux (sgt v_18862 v_18863) v_18862 v_18863) (concat (mux (sgt v_18866 v_18867) v_18866 v_18867) (concat (mux (sgt v_18870 v_18871) v_18870 v_18871) (concat (mux (sgt v_18874 v_18875) v_18874 v_18875) (concat (mux (sgt v_18878 v_18879) v_18878 v_18879) (concat (mux (sgt v_18882 v_18883) v_18882 v_18883) (concat (mux (sgt v_18886 v_18887) v_18886 v_18887) (concat (mux (sgt v_18890 v_18891) v_18890 v_18891) (concat (mux (sgt v_18894 v_18895) v_18894 v_18895) (concat (mux (sgt v_18898 v_18899) v_18898 v_18899) (concat (mux (sgt v_18902 v_18903) v_18902 v_18903) (concat (mux (sgt v_18906 v_18907) v_18906 v_18907) (concat (mux (sgt v_18910 v_18911) v_18910 v_18911) (concat (mux (sgt v_18914 v_18915) v_18914 v_18915) (concat (mux (sgt v_18918 v_18919) v_18918 v_18919) (concat (mux (sgt v_18922 v_18923) v_18922 v_18923) (concat (mux (sgt v_18926 v_18927) v_18926 v_18927) (concat (mux (sgt v_18930 v_18931) v_18930 v_18931) (concat (mux (sgt v_18934 v_18935) v_18934 v_18935) (concat (mux (sgt v_18938 v_18939) v_18938 v_18939) (concat (mux (sgt v_18942 v_18943) v_18942 v_18943) (concat (mux (sgt v_18946 v_18947) v_18946 v_18947) (mux (sgt v_18950 v_18951) v_18950 v_18951))))))))))))))))))))))))))))))));
      pure ()
    pat_end
