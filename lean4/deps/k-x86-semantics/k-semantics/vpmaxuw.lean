def vpmaxuw1 : instruction :=
  definst "vpmaxuw" $ do
    pattern fun (v_2494 : reg (bv 128)) (v_2495 : reg (bv 128)) (v_2496 : reg (bv 128)) => do
      v_4299 <- getRegister v_2495;
      v_4300 <- eval (extract v_4299 0 16);
      v_4301 <- getRegister v_2494;
      v_4302 <- eval (extract v_4301 0 16);
      v_4305 <- eval (extract v_4299 16 32);
      v_4306 <- eval (extract v_4301 16 32);
      v_4309 <- eval (extract v_4299 32 48);
      v_4310 <- eval (extract v_4301 32 48);
      v_4313 <- eval (extract v_4299 48 64);
      v_4314 <- eval (extract v_4301 48 64);
      v_4317 <- eval (extract v_4299 64 80);
      v_4318 <- eval (extract v_4301 64 80);
      v_4321 <- eval (extract v_4299 80 96);
      v_4322 <- eval (extract v_4301 80 96);
      v_4325 <- eval (extract v_4299 96 112);
      v_4326 <- eval (extract v_4301 96 112);
      v_4329 <- eval (extract v_4299 112 128);
      v_4330 <- eval (extract v_4301 112 128);
      setRegister (lhs.of_reg v_2496) (concat (mux (ugt v_4300 v_4302) v_4300 v_4302) (concat (mux (ugt v_4305 v_4306) v_4305 v_4306) (concat (mux (ugt v_4309 v_4310) v_4309 v_4310) (concat (mux (ugt v_4313 v_4314) v_4313 v_4314) (concat (mux (ugt v_4317 v_4318) v_4317 v_4318) (concat (mux (ugt v_4321 v_4322) v_4321 v_4322) (concat (mux (ugt v_4325 v_4326) v_4325 v_4326) (mux (ugt v_4329 v_4330) v_4329 v_4330))))))));
      pure ()
    pat_end;
    pattern fun (v_2505 : reg (bv 256)) (v_2506 : reg (bv 256)) (v_2507 : reg (bv 256)) => do
      v_4346 <- getRegister v_2506;
      v_4347 <- eval (extract v_4346 0 16);
      v_4348 <- getRegister v_2505;
      v_4349 <- eval (extract v_4348 0 16);
      v_4352 <- eval (extract v_4346 16 32);
      v_4353 <- eval (extract v_4348 16 32);
      v_4356 <- eval (extract v_4346 32 48);
      v_4357 <- eval (extract v_4348 32 48);
      v_4360 <- eval (extract v_4346 48 64);
      v_4361 <- eval (extract v_4348 48 64);
      v_4364 <- eval (extract v_4346 64 80);
      v_4365 <- eval (extract v_4348 64 80);
      v_4368 <- eval (extract v_4346 80 96);
      v_4369 <- eval (extract v_4348 80 96);
      v_4372 <- eval (extract v_4346 96 112);
      v_4373 <- eval (extract v_4348 96 112);
      v_4376 <- eval (extract v_4346 112 128);
      v_4377 <- eval (extract v_4348 112 128);
      v_4380 <- eval (extract v_4346 128 144);
      v_4381 <- eval (extract v_4348 128 144);
      v_4384 <- eval (extract v_4346 144 160);
      v_4385 <- eval (extract v_4348 144 160);
      v_4388 <- eval (extract v_4346 160 176);
      v_4389 <- eval (extract v_4348 160 176);
      v_4392 <- eval (extract v_4346 176 192);
      v_4393 <- eval (extract v_4348 176 192);
      v_4396 <- eval (extract v_4346 192 208);
      v_4397 <- eval (extract v_4348 192 208);
      v_4400 <- eval (extract v_4346 208 224);
      v_4401 <- eval (extract v_4348 208 224);
      v_4404 <- eval (extract v_4346 224 240);
      v_4405 <- eval (extract v_4348 224 240);
      v_4408 <- eval (extract v_4346 240 256);
      v_4409 <- eval (extract v_4348 240 256);
      setRegister (lhs.of_reg v_2507) (concat (mux (ugt v_4347 v_4349) v_4347 v_4349) (concat (mux (ugt v_4352 v_4353) v_4352 v_4353) (concat (mux (ugt v_4356 v_4357) v_4356 v_4357) (concat (mux (ugt v_4360 v_4361) v_4360 v_4361) (concat (mux (ugt v_4364 v_4365) v_4364 v_4365) (concat (mux (ugt v_4368 v_4369) v_4368 v_4369) (concat (mux (ugt v_4372 v_4373) v_4372 v_4373) (concat (mux (ugt v_4376 v_4377) v_4376 v_4377) (concat (mux (ugt v_4380 v_4381) v_4380 v_4381) (concat (mux (ugt v_4384 v_4385) v_4384 v_4385) (concat (mux (ugt v_4388 v_4389) v_4388 v_4389) (concat (mux (ugt v_4392 v_4393) v_4392 v_4393) (concat (mux (ugt v_4396 v_4397) v_4396 v_4397) (concat (mux (ugt v_4400 v_4401) v_4400 v_4401) (concat (mux (ugt v_4404 v_4405) v_4404 v_4405) (mux (ugt v_4408 v_4409) v_4408 v_4409))))))))))))))));
      pure ()
    pat_end;
    pattern fun (v_2488 : Mem) (v_2489 : reg (bv 128)) (v_2490 : reg (bv 128)) => do
      v_10980 <- getRegister v_2489;
      v_10981 <- eval (extract v_10980 0 16);
      v_10982 <- evaluateAddress v_2488;
      v_10983 <- load v_10982 16;
      v_10984 <- eval (extract v_10983 0 16);
      v_10987 <- eval (extract v_10980 16 32);
      v_10988 <- eval (extract v_10983 16 32);
      v_10991 <- eval (extract v_10980 32 48);
      v_10992 <- eval (extract v_10983 32 48);
      v_10995 <- eval (extract v_10980 48 64);
      v_10996 <- eval (extract v_10983 48 64);
      v_10999 <- eval (extract v_10980 64 80);
      v_11000 <- eval (extract v_10983 64 80);
      v_11003 <- eval (extract v_10980 80 96);
      v_11004 <- eval (extract v_10983 80 96);
      v_11007 <- eval (extract v_10980 96 112);
      v_11008 <- eval (extract v_10983 96 112);
      v_11011 <- eval (extract v_10980 112 128);
      v_11012 <- eval (extract v_10983 112 128);
      setRegister (lhs.of_reg v_2490) (concat (mux (ugt v_10981 v_10984) v_10981 v_10984) (concat (mux (ugt v_10987 v_10988) v_10987 v_10988) (concat (mux (ugt v_10991 v_10992) v_10991 v_10992) (concat (mux (ugt v_10995 v_10996) v_10995 v_10996) (concat (mux (ugt v_10999 v_11000) v_10999 v_11000) (concat (mux (ugt v_11003 v_11004) v_11003 v_11004) (concat (mux (ugt v_11007 v_11008) v_11007 v_11008) (mux (ugt v_11011 v_11012) v_11011 v_11012))))))));
      pure ()
    pat_end;
    pattern fun (v_2499 : Mem) (v_2500 : reg (bv 256)) (v_2501 : reg (bv 256)) => do
      v_11023 <- getRegister v_2500;
      v_11024 <- eval (extract v_11023 0 16);
      v_11025 <- evaluateAddress v_2499;
      v_11026 <- load v_11025 32;
      v_11027 <- eval (extract v_11026 0 16);
      v_11030 <- eval (extract v_11023 16 32);
      v_11031 <- eval (extract v_11026 16 32);
      v_11034 <- eval (extract v_11023 32 48);
      v_11035 <- eval (extract v_11026 32 48);
      v_11038 <- eval (extract v_11023 48 64);
      v_11039 <- eval (extract v_11026 48 64);
      v_11042 <- eval (extract v_11023 64 80);
      v_11043 <- eval (extract v_11026 64 80);
      v_11046 <- eval (extract v_11023 80 96);
      v_11047 <- eval (extract v_11026 80 96);
      v_11050 <- eval (extract v_11023 96 112);
      v_11051 <- eval (extract v_11026 96 112);
      v_11054 <- eval (extract v_11023 112 128);
      v_11055 <- eval (extract v_11026 112 128);
      v_11058 <- eval (extract v_11023 128 144);
      v_11059 <- eval (extract v_11026 128 144);
      v_11062 <- eval (extract v_11023 144 160);
      v_11063 <- eval (extract v_11026 144 160);
      v_11066 <- eval (extract v_11023 160 176);
      v_11067 <- eval (extract v_11026 160 176);
      v_11070 <- eval (extract v_11023 176 192);
      v_11071 <- eval (extract v_11026 176 192);
      v_11074 <- eval (extract v_11023 192 208);
      v_11075 <- eval (extract v_11026 192 208);
      v_11078 <- eval (extract v_11023 208 224);
      v_11079 <- eval (extract v_11026 208 224);
      v_11082 <- eval (extract v_11023 224 240);
      v_11083 <- eval (extract v_11026 224 240);
      v_11086 <- eval (extract v_11023 240 256);
      v_11087 <- eval (extract v_11026 240 256);
      setRegister (lhs.of_reg v_2501) (concat (mux (ugt v_11024 v_11027) v_11024 v_11027) (concat (mux (ugt v_11030 v_11031) v_11030 v_11031) (concat (mux (ugt v_11034 v_11035) v_11034 v_11035) (concat (mux (ugt v_11038 v_11039) v_11038 v_11039) (concat (mux (ugt v_11042 v_11043) v_11042 v_11043) (concat (mux (ugt v_11046 v_11047) v_11046 v_11047) (concat (mux (ugt v_11050 v_11051) v_11050 v_11051) (concat (mux (ugt v_11054 v_11055) v_11054 v_11055) (concat (mux (ugt v_11058 v_11059) v_11058 v_11059) (concat (mux (ugt v_11062 v_11063) v_11062 v_11063) (concat (mux (ugt v_11066 v_11067) v_11066 v_11067) (concat (mux (ugt v_11070 v_11071) v_11070 v_11071) (concat (mux (ugt v_11074 v_11075) v_11074 v_11075) (concat (mux (ugt v_11078 v_11079) v_11078 v_11079) (concat (mux (ugt v_11082 v_11083) v_11082 v_11083) (mux (ugt v_11086 v_11087) v_11086 v_11087))))))))))))))));
      pure ()
    pat_end