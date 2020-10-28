; main.block_0_201340.0 (0x201348). Machine code write at 0x201348 is in unreserved stack space.
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
(define-fun a201340_rip () (_ BitVec 64) #x0000000000201340)
(declare-fun a201340_rax () (_ BitVec 64))
(define-fun a201340_rcx () (_ BitVec 64) fnstart_rcx)
(define-fun a201340_rdx () (_ BitVec 64) fnstart_rdx)
(define-fun a201340_rbx () (_ BitVec 64) fnstart_rbx)
(define-fun a201340_rsp () (_ BitVec 64) fnstart_rsp)
(define-fun a201340_rbp () (_ BitVec 64) fnstart_rbp)
(define-fun a201340_rsi () (_ BitVec 64) fnstart_rsi)
(define-fun a201340_rdi () (_ BitVec 64) fnstart_rdi)
(define-fun a201340_r8 () (_ BitVec 64) fnstart_r8)
(define-fun a201340_r9 () (_ BitVec 64) fnstart_r9)
(declare-fun a201340_r10 () (_ BitVec 64))
(declare-fun a201340_r11 () (_ BitVec 64))
(define-fun a201340_r12 () (_ BitVec 64) fnstart_r12)
(define-fun a201340_r13 () (_ BitVec 64) fnstart_r13)
(define-fun a201340_r14 () (_ BitVec 64) fnstart_r14)
(define-fun a201340_r15 () (_ BitVec 64) fnstart_r15)
(declare-fun a201340_cf () Bool)
(declare-fun a201340_pf () Bool)
(declare-fun a201340_af () Bool)
(declare-fun a201340_zf () Bool)
(declare-fun a201340_sf () Bool)
(declare-fun a201340_tf () Bool)
(declare-fun a201340_if () Bool)
(define-fun a201340_df () Bool false)
(declare-fun a201340_of () Bool)
(declare-fun a201340_ie () Bool)
(declare-fun a201340_de () Bool)
(declare-fun a201340_ze () Bool)
(declare-fun a201340_oe () Bool)
(declare-fun a201340_ue () Bool)
(declare-fun a201340_pe () Bool)
(declare-fun a201340_ef () Bool)
(declare-fun a201340_es () Bool)
(declare-fun a201340_c0 () Bool)
(declare-fun a201340_c1 () Bool)
(declare-fun a201340_c2 () Bool)
(declare-fun a201340_RESERVED_STATUS_11 () Bool)
(declare-fun a201340_RESERVED_STATUS_12 () Bool)
(declare-fun a201340_RESERVED_STATUS_13 () Bool)
(declare-fun a201340_c3 () Bool)
(declare-fun a201340_RESERVED_STATUS_15 () Bool)
(define-fun a201340_x87top () (_ BitVec 3) (_ bv7 3))
(declare-fun a201340_tag0 () (_ BitVec 2))
(declare-fun a201340_tag1 () (_ BitVec 2))
(declare-fun a201340_tag2 () (_ BitVec 2))
(declare-fun a201340_tag3 () (_ BitVec 2))
(declare-fun a201340_tag4 () (_ BitVec 2))
(declare-fun a201340_tag5 () (_ BitVec 2))
(declare-fun a201340_tag6 () (_ BitVec 2))
(declare-fun a201340_tag7 () (_ BitVec 2))
(declare-fun a201340_mm0 () (_ BitVec 80))
(declare-fun a201340_mm1 () (_ BitVec 80))
(declare-fun a201340_mm2 () (_ BitVec 80))
(declare-fun a201340_mm3 () (_ BitVec 80))
(declare-fun a201340_mm4 () (_ BitVec 80))
(declare-fun a201340_mm5 () (_ BitVec 80))
(declare-fun a201340_mm6 () (_ BitVec 80))
(declare-fun a201340_mm7 () (_ BitVec 80))
(declare-fun a201340_zmm0 () (_ BitVec 512))
(declare-fun a201340_zmm1 () (_ BitVec 512))
(declare-fun a201340_zmm2 () (_ BitVec 512))
(declare-fun a201340_zmm3 () (_ BitVec 512))
(declare-fun a201340_zmm4 () (_ BitVec 512))
(declare-fun a201340_zmm5 () (_ BitVec 512))
(declare-fun a201340_zmm6 () (_ BitVec 512))
(declare-fun a201340_zmm7 () (_ BitVec 512))
(declare-fun a201340_zmm8 () (_ BitVec 512))
(declare-fun a201340_zmm9 () (_ BitVec 512))
(declare-fun a201340_zmm10 () (_ BitVec 512))
(declare-fun a201340_zmm11 () (_ BitVec 512))
(declare-fun a201340_zmm12 () (_ BitVec 512))
(declare-fun a201340_zmm13 () (_ BitVec 512))
(declare-fun a201340_zmm14 () (_ BitVec 512))
(declare-fun a201340_zmm15 () (_ BitVec 512))
(declare-fun a201340_zmm16 () (_ BitVec 512))
(declare-fun a201340_zmm17 () (_ BitVec 512))
(declare-fun a201340_zmm18 () (_ BitVec 512))
(declare-fun a201340_zmm19 () (_ BitVec 512))
(declare-fun a201340_zmm20 () (_ BitVec 512))
(declare-fun a201340_zmm21 () (_ BitVec 512))
(declare-fun a201340_zmm22 () (_ BitVec 512))
(declare-fun a201340_zmm23 () (_ BitVec 512))
(declare-fun a201340_zmm24 () (_ BitVec 512))
(declare-fun a201340_zmm25 () (_ BitVec 512))
(declare-fun a201340_zmm26 () (_ BitVec 512))
(declare-fun a201340_zmm27 () (_ BitVec 512))
(declare-fun a201340_zmm28 () (_ BitVec 512))
(declare-fun a201340_zmm29 () (_ BitVec 512))
(declare-fun a201340_zmm30 () (_ BitVec 512))
(declare-fun a201340_zmm31 () (_ BitVec 512))
(declare-const x86mem_0 (Array (_ BitVec 64) (_ BitVec 8)))
(define-fun return_addr () (_ BitVec 64) (mem_readbv64 x86mem_0 fnstart_rsp))
(assert (= a201340_rbx fnstart_rbx))
(assert (= a201340_rsp fnstart_rsp))
(assert (= a201340_rbp fnstart_rbp))
(assert (= a201340_r12 fnstart_r12))
(assert (= a201340_r13 fnstart_r13))
(assert (= a201340_r14 fnstart_r14))
(assert (= a201340_r15 fnstart_r15))
; LLVM: %t0 = call i64 (i64) @add(i64 42)
(define-fun x86local_0 () (_ BitVec 64) (bvsub a201340_rsp (_ bv8 64)))
(assert (mc_only_stack_range x86local_0 (_ bv8 64)))
(define-fun x86mem_1 () (Array (_ BitVec 64) (_ BitVec 8)) (mem_writebv64 x86mem_0 x86local_0 a201340_rbp))
(define-fun x86local_1 () Bool (distinct ((_ extract 64 64) (bvsub (bvsub ((_ sign_extend 1) x86local_0) (bvneg ((_ sign_extend 1) (_ bv16 64)))) (ite false (_ bv1 65) (_ bv0 65)))) ((_ extract 63 63) (bvsub (bvsub ((_ sign_extend 1) x86local_0) (bvneg ((_ sign_extend 1) (_ bv16 64)))) (ite false (_ bv1 65) (_ bv0 65))))))
(define-fun x86local_2 () Bool (bvult x86local_0 (_ bv16 64)))
(define-fun x86local_3 () (_ BitVec 64) (bvsub x86local_0 (_ bv16 64)))
(define-fun x86local_4 () Bool (bvslt x86local_3 (_ bv0 64)))
(define-fun x86local_5 () Bool (= x86local_3 (_ bv0 64)))
(define-fun x86local_6 () (_ BitVec 8) ((_ extract 7 0) x86local_3))
(define-fun x86local_7 () Bool (even_parity x86local_6))
(define-fun x86local_8 () (_ BitVec 64) (bvadd x86local_0 (_ bv18446744073709551612 64)))
(check-sat-assuming ((not (mc_only_stack_range x86local_8 (_ bv4 64)))))
(exit)