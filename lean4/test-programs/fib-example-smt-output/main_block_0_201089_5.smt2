; main.block_0_201089.1 (0x201098). Checking jump register rip.
(set-logic ALL_SUPPORTED)
(set-option :produce-models true)
(define-fun even_parity ((v (_ BitVec 8))) Bool (= (bvxor ((_ extract 0 0) v) ((_ extract 1 1) v) ((_ extract 2 2) v) ((_ extract 3 3) v) ((_ extract 4 4) v) ((_ extract 5 5) v) ((_ extract 6 6) v) ((_ extract 7 7) v)) #b0))
(define-fun mem_readbv8 ((m (Array (_ BitVec 64) (_ BitVec 8))) (a (_ BitVec 64))) (_ BitVec 8) (select m a))
(define-fun mem_readbv16 ((m (Array (_ BitVec 64) (_ BitVec 8))) (a (_ BitVec 64))) (_ BitVec 16) (concat (select m (bvadd a (_ bv1 64))) (select m a)))
(define-fun mem_readbv32 ((m (Array (_ BitVec 64) (_ BitVec 8))) (a (_ BitVec 64))) (_ BitVec 32) (concat (select m (bvadd a (_ bv3 64))) (concat (select m (bvadd a (_ bv2 64))) (concat (select m (bvadd a (_ bv1 64))) (select m a)))))
(define-fun mem_readbv64 ((m (Array (_ BitVec 64) (_ BitVec 8))) (a (_ BitVec 64))) (_ BitVec 64) (concat (select m (bvadd a (_ bv7 64))) (concat (select m (bvadd a (_ bv6 64))) (concat (select m (bvadd a (_ bv5 64))) (concat (select m (bvadd a (_ bv4 64))) (concat (select m (bvadd a (_ bv3 64))) (concat (select m (bvadd a (_ bv2 64))) (concat (select m (bvadd a (_ bv1 64))) (select m a)))))))))
(define-fun mem_writebv8 ((m (Array (_ BitVec 64) (_ BitVec 8))) (a (_ BitVec 64)) (v (_ BitVec 8))) (Array (_ BitVec 64) (_ BitVec 8)) (store m a ((_ extract 7 0) v)))
(define-fun mem_writebv16 ((m (Array (_ BitVec 64) (_ BitVec 8))) (a (_ BitVec 64)) (v (_ BitVec 16))) (Array (_ BitVec 64) (_ BitVec 8)) (store (store m a ((_ extract 7 0) v)) (bvadd a (_ bv1 64)) ((_ extract 15 8) v)))
(define-fun mem_writebv32 ((m (Array (_ BitVec 64) (_ BitVec 8))) (a (_ BitVec 64)) (v (_ BitVec 32))) (Array (_ BitVec 64) (_ BitVec 8)) (store (store (store (store m a ((_ extract 7 0) v)) (bvadd a (_ bv1 64)) ((_ extract 15 8) v)) (bvadd a (_ bv2 64)) ((_ extract 23 16) v)) (bvadd a (_ bv3 64)) ((_ extract 31 24) v)))
(define-fun mem_writebv64 ((m (Array (_ BitVec 64) (_ BitVec 8))) (a (_ BitVec 64)) (v (_ BitVec 64))) (Array (_ BitVec 64) (_ BitVec 8)) (store (store (store (store (store (store (store (store m a ((_ extract 7 0) v)) (bvadd a (_ bv1 64)) ((_ extract 15 8) v)) (bvadd a (_ bv2 64)) ((_ extract 23 16) v)) (bvadd a (_ bv3 64)) ((_ extract 31 24) v)) (bvadd a (_ bv4 64)) ((_ extract 39 32) v)) (bvadd a (_ bv5 64)) ((_ extract 47 40) v)) (bvadd a (_ bv6 64)) ((_ extract 55 48) v)) (bvadd a (_ bv7 64)) ((_ extract 63 56) v)))
(declare-fun fnstart_rcx () (_ BitVec 64))
(declare-fun fnstart_rdx () (_ BitVec 64))
(declare-fun fnstart_rbx () (_ BitVec 64))
(declare-fun fnstart_rsp () (_ BitVec 64))
(declare-fun fnstart_rbp () (_ BitVec 64))
(declare-fun fnstart_rsi () (_ BitVec 64))
(declare-fun fnstart_rdi () (_ BitVec 64))
(declare-fun fnstart_r8 () (_ BitVec 64))
(declare-fun fnstart_r9 () (_ BitVec 64))
(declare-fun fnstart_r12 () (_ BitVec 64))
(declare-fun fnstart_r13 () (_ BitVec 64))
(declare-fun fnstart_r14 () (_ BitVec 64))
(declare-fun fnstart_r15 () (_ BitVec 64))
(declare-const stack_alloc_min (_ BitVec 64))
(assert (= (bvand stack_alloc_min #x0000000000000fff) (_ bv0 64)))
(assert (bvult (_ bv4096 64) stack_alloc_min))
(define-fun stack_guard_min () (_ BitVec 64) (bvsub stack_alloc_min (_ bv4096 64)))
(assert (bvult stack_guard_min stack_alloc_min))
(declare-const stack_max (_ BitVec 64))
(assert (= (bvand stack_max #x0000000000000fff) (_ bv0 64)))
(assert (bvult stack_alloc_min stack_max))
(assert (bvule stack_alloc_min fnstart_rsp))
(assert (bvule fnstart_rsp (bvsub stack_max (_ bv8 64))))
(define-fun on_stack ((a (_ BitVec 64)) (sz (_ BitVec 64))) Bool (let ((e (bvadd a sz))) (and (bvule stack_guard_min a) (bvule a e) (bvule e stack_max))))
(define-fun not_in_stack_range ((a (_ BitVec 64)) (sz (_ BitVec 64))) Bool (let ((e (bvadd a sz))) (and (bvule a e) (or (bvule e stack_alloc_min) (bvule stack_max a)))))
(assert (bvult fnstart_rsp (bvsub stack_max (_ bv8 64))))
(assert (= ((_ extract 3 0) fnstart_rsp) (_ bv8 4)))
(define-fun mc_only_stack_range ((a (_ BitVec 64)) (sz (_ BitVec 64))) Bool (let ((e (bvadd a sz))) (on_stack a sz)))
(define-fun a201089_rip () (_ BitVec 64) #x0000000000201089)
(declare-fun a201089_rax () (_ BitVec 64))
(declare-fun a201089_rcx () (_ BitVec 64))
(declare-fun a201089_rdx () (_ BitVec 64))
(declare-fun a201089_rbx () (_ BitVec 64))
(declare-fun a201089_rsp () (_ BitVec 64))
(declare-fun a201089_rbp () (_ BitVec 64))
(declare-fun a201089_rsi () (_ BitVec 64))
(declare-fun a201089_rdi () (_ BitVec 64))
(declare-fun a201089_r8 () (_ BitVec 64))
(declare-fun a201089_r9 () (_ BitVec 64))
(declare-fun a201089_r10 () (_ BitVec 64))
(declare-fun a201089_r11 () (_ BitVec 64))
(declare-fun a201089_r12 () (_ BitVec 64))
(declare-fun a201089_r13 () (_ BitVec 64))
(declare-fun a201089_r14 () (_ BitVec 64))
(declare-fun a201089_r15 () (_ BitVec 64))
(declare-fun a201089_cf () Bool)
(declare-fun a201089_pf () Bool)
(declare-fun a201089_af () Bool)
(declare-fun a201089_zf () Bool)
(declare-fun a201089_sf () Bool)
(declare-fun a201089_tf () Bool)
(declare-fun a201089_if () Bool)
(define-fun a201089_df () Bool false)
(declare-fun a201089_of () Bool)
(declare-fun a201089_ie () Bool)
(declare-fun a201089_de () Bool)
(declare-fun a201089_ze () Bool)
(declare-fun a201089_oe () Bool)
(declare-fun a201089_ue () Bool)
(declare-fun a201089_pe () Bool)
(declare-fun a201089_ef () Bool)
(declare-fun a201089_es () Bool)
(declare-fun a201089_c0 () Bool)
(declare-fun a201089_c1 () Bool)
(declare-fun a201089_c2 () Bool)
(declare-fun a201089_RESERVED_STATUS_11 () Bool)
(declare-fun a201089_RESERVED_STATUS_12 () Bool)
(declare-fun a201089_RESERVED_STATUS_13 () Bool)
(declare-fun a201089_c3 () Bool)
(declare-fun a201089_RESERVED_STATUS_15 () Bool)
(define-fun a201089_x87top () (_ BitVec 3) (_ bv7 3))
(declare-fun a201089_tag0 () (_ BitVec 2))
(declare-fun a201089_tag1 () (_ BitVec 2))
(declare-fun a201089_tag2 () (_ BitVec 2))
(declare-fun a201089_tag3 () (_ BitVec 2))
(declare-fun a201089_tag4 () (_ BitVec 2))
(declare-fun a201089_tag5 () (_ BitVec 2))
(declare-fun a201089_tag6 () (_ BitVec 2))
(declare-fun a201089_tag7 () (_ BitVec 2))
(declare-fun a201089_mm0 () (_ BitVec 80))
(declare-fun a201089_mm1 () (_ BitVec 80))
(declare-fun a201089_mm2 () (_ BitVec 80))
(declare-fun a201089_mm3 () (_ BitVec 80))
(declare-fun a201089_mm4 () (_ BitVec 80))
(declare-fun a201089_mm5 () (_ BitVec 80))
(declare-fun a201089_mm6 () (_ BitVec 80))
(declare-fun a201089_mm7 () (_ BitVec 80))
(declare-fun a201089_zmm0 () (_ BitVec 512))
(declare-fun a201089_zmm1 () (_ BitVec 512))
(declare-fun a201089_zmm2 () (_ BitVec 512))
(declare-fun a201089_zmm3 () (_ BitVec 512))
(declare-fun a201089_zmm4 () (_ BitVec 512))
(declare-fun a201089_zmm5 () (_ BitVec 512))
(declare-fun a201089_zmm6 () (_ BitVec 512))
(declare-fun a201089_zmm7 () (_ BitVec 512))
(declare-fun a201089_zmm8 () (_ BitVec 512))
(declare-fun a201089_zmm9 () (_ BitVec 512))
(declare-fun a201089_zmm10 () (_ BitVec 512))
(declare-fun a201089_zmm11 () (_ BitVec 512))
(declare-fun a201089_zmm12 () (_ BitVec 512))
(declare-fun a201089_zmm13 () (_ BitVec 512))
(declare-fun a201089_zmm14 () (_ BitVec 512))
(declare-fun a201089_zmm15 () (_ BitVec 512))
(declare-fun a201089_zmm16 () (_ BitVec 512))
(declare-fun a201089_zmm17 () (_ BitVec 512))
(declare-fun a201089_zmm18 () (_ BitVec 512))
(declare-fun a201089_zmm19 () (_ BitVec 512))
(declare-fun a201089_zmm20 () (_ BitVec 512))
(declare-fun a201089_zmm21 () (_ BitVec 512))
(declare-fun a201089_zmm22 () (_ BitVec 512))
(declare-fun a201089_zmm23 () (_ BitVec 512))
(declare-fun a201089_zmm24 () (_ BitVec 512))
(declare-fun a201089_zmm25 () (_ BitVec 512))
(declare-fun a201089_zmm26 () (_ BitVec 512))
(declare-fun a201089_zmm27 () (_ BitVec 512))
(declare-fun a201089_zmm28 () (_ BitVec 512))
(declare-fun a201089_zmm29 () (_ BitVec 512))
(declare-fun a201089_zmm30 () (_ BitVec 512))
(declare-fun a201089_zmm31 () (_ BitVec 512))
(declare-const x86mem_0 (Array (_ BitVec 64) (_ BitVec 8)))
(define-fun return_addr () (_ BitVec 64) (mem_readbv64 x86mem_0 fnstart_rsp))
(declare-const llvm_t1 (_ BitVec 64))
(assert (= llvm_t1 a201089_rax))
(assert (= a201089_rbx fnstart_rbx))
(assert (= a201089_rsp (bvsub fnstart_rsp (_ bv24 64))))
(assert (= a201089_rbp (bvsub fnstart_rsp (_ bv8 64))))
(assert (= a201089_r12 fnstart_r12))
(assert (= a201089_r13 fnstart_r13))
(assert (= a201089_r14 fnstart_r14))
(assert (= a201089_r15 fnstart_r15))
(assert (= (mem_readbv64 x86mem_0 (bvsub fnstart_rsp (_ bv8 64))) fnstart_rbp))
; LLVM: %t2 = call i64 (i64, ...) @printf(i64 2097618, i64 %t1)
(declare-fun x86local_0 () (_ BitVec 64))
(define-fun x86local_1 () (_ BitVec 64) (bvsub a201089_rsp (_ bv8 64)))
(assert (mc_only_stack_range x86local_1 (_ bv8 64)))
(define-fun x86mem_1 () (Array (_ BitVec 64) (_ BitVec 8)) (mem_writebv64 x86mem_0 x86local_1 #x000000000020109d))
(assert (= #x00000000002014aa #x00000000002014aa))
(assert (= (_ bv2097618 64) (_ bv2097618 64)))
(assert (= llvm_t1 a201089_rax))
(assert (= (mem_readbv64 x86mem_1 x86local_1) #x000000000020109d))
(define-fun a20109d_rip () (_ BitVec 64) #x000000000020109d)
(declare-fun a20109d_rax () (_ BitVec 64))
(declare-fun a20109d_rcx () (_ BitVec 64))
(declare-fun a20109d_rdx () (_ BitVec 64))
(define-fun a20109d_rbx () (_ BitVec 64) a201089_rbx)
(define-fun a20109d_rsp () (_ BitVec 64) (bvadd x86local_1 (_ bv8 64)))
(define-fun a20109d_rbp () (_ BitVec 64) a201089_rbp)
(declare-fun a20109d_rsi () (_ BitVec 64))
(declare-fun a20109d_rdi () (_ BitVec 64))
(declare-fun a20109d_r8 () (_ BitVec 64))
(declare-fun a20109d_r9 () (_ BitVec 64))
(declare-fun a20109d_r10 () (_ BitVec 64))
(declare-fun a20109d_r11 () (_ BitVec 64))
(define-fun a20109d_r12 () (_ BitVec 64) a201089_r12)
(define-fun a20109d_r13 () (_ BitVec 64) a201089_r13)
(define-fun a20109d_r14 () (_ BitVec 64) a201089_r14)
(define-fun a20109d_r15 () (_ BitVec 64) a201089_r15)
(declare-fun a20109d_cf () Bool)
(declare-fun a20109d_pf () Bool)
(declare-fun a20109d_af () Bool)
(declare-fun a20109d_zf () Bool)
(declare-fun a20109d_sf () Bool)
(declare-fun a20109d_tf () Bool)
(declare-fun a20109d_if () Bool)
(define-fun a20109d_df () Bool false)
(declare-fun a20109d_of () Bool)
(declare-fun a20109d_ie () Bool)
(declare-fun a20109d_de () Bool)
(declare-fun a20109d_ze () Bool)
(declare-fun a20109d_oe () Bool)
(declare-fun a20109d_ue () Bool)
(declare-fun a20109d_pe () Bool)
(declare-fun a20109d_ef () Bool)
(declare-fun a20109d_es () Bool)
(declare-fun a20109d_c0 () Bool)
(declare-fun a20109d_c1 () Bool)
(declare-fun a20109d_c2 () Bool)
(declare-fun a20109d_RESERVED_STATUS_11 () Bool)
(declare-fun a20109d_RESERVED_STATUS_12 () Bool)
(declare-fun a20109d_RESERVED_STATUS_13 () Bool)
(declare-fun a20109d_c3 () Bool)
(declare-fun a20109d_RESERVED_STATUS_15 () Bool)
(define-fun a20109d_x87top () (_ BitVec 3) (_ bv7 3))
(declare-fun a20109d_tag0 () (_ BitVec 2))
(declare-fun a20109d_tag1 () (_ BitVec 2))
(declare-fun a20109d_tag2 () (_ BitVec 2))
(declare-fun a20109d_tag3 () (_ BitVec 2))
(declare-fun a20109d_tag4 () (_ BitVec 2))
(declare-fun a20109d_tag5 () (_ BitVec 2))
(declare-fun a20109d_tag6 () (_ BitVec 2))
(declare-fun a20109d_tag7 () (_ BitVec 2))
(declare-fun a20109d_mm0 () (_ BitVec 80))
(declare-fun a20109d_mm1 () (_ BitVec 80))
(declare-fun a20109d_mm2 () (_ BitVec 80))
(declare-fun a20109d_mm3 () (_ BitVec 80))
(declare-fun a20109d_mm4 () (_ BitVec 80))
(declare-fun a20109d_mm5 () (_ BitVec 80))
(declare-fun a20109d_mm6 () (_ BitVec 80))
(declare-fun a20109d_mm7 () (_ BitVec 80))
(declare-fun a20109d_zmm0 () (_ BitVec 512))
(declare-fun a20109d_zmm1 () (_ BitVec 512))
(declare-fun a20109d_zmm2 () (_ BitVec 512))
(declare-fun a20109d_zmm3 () (_ BitVec 512))
(declare-fun a20109d_zmm4 () (_ BitVec 512))
(declare-fun a20109d_zmm5 () (_ BitVec 512))
(declare-fun a20109d_zmm6 () (_ BitVec 512))
(declare-fun a20109d_zmm7 () (_ BitVec 512))
(declare-fun a20109d_zmm8 () (_ BitVec 512))
(declare-fun a20109d_zmm9 () (_ BitVec 512))
(declare-fun a20109d_zmm10 () (_ BitVec 512))
(declare-fun a20109d_zmm11 () (_ BitVec 512))
(declare-fun a20109d_zmm12 () (_ BitVec 512))
(declare-fun a20109d_zmm13 () (_ BitVec 512))
(declare-fun a20109d_zmm14 () (_ BitVec 512))
(declare-fun a20109d_zmm15 () (_ BitVec 512))
(declare-fun a20109d_zmm16 () (_ BitVec 512))
(declare-fun a20109d_zmm17 () (_ BitVec 512))
(declare-fun a20109d_zmm18 () (_ BitVec 512))
(declare-fun a20109d_zmm19 () (_ BitVec 512))
(declare-fun a20109d_zmm20 () (_ BitVec 512))
(declare-fun a20109d_zmm21 () (_ BitVec 512))
(declare-fun a20109d_zmm22 () (_ BitVec 512))
(declare-fun a20109d_zmm23 () (_ BitVec 512))
(declare-fun a20109d_zmm24 () (_ BitVec 512))
(declare-fun a20109d_zmm25 () (_ BitVec 512))
(declare-fun a20109d_zmm26 () (_ BitVec 512))
(declare-fun a20109d_zmm27 () (_ BitVec 512))
(declare-fun a20109d_zmm28 () (_ BitVec 512))
(declare-fun a20109d_zmm29 () (_ BitVec 512))
(declare-fun a20109d_zmm30 () (_ BitVec 512))
(declare-fun a20109d_zmm31 () (_ BitVec 512))
(declare-const x86mem_2 (Array (_ BitVec 64) (_ BitVec 8)))
(assert (eqrange x86mem_2 x86mem_1 (bvadd x86local_1 (_ bv8 64)) (bvadd fnstart_rsp (_ bv7 64))))
(define-fun llvm_t2 () (_ BitVec 64) a20109d_rax)
; LLVM: br label %block_0_20109d
(check-sat-assuming ((not (= #x000000000020109d a20109d_rip))))
(exit)
