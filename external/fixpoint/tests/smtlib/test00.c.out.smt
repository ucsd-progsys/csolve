(benchmark unknown
:status unsat
:logic AUFLIA
:extrafuns ((x_main__SSA__phi_5 Int))

:extrafuns ((x_main__SSA__blk_4_1 Int))

:extrafuns ((x_main__SSA__blk_0_1 Int))
:extrafuns ((x_main Int))

:extrafuns ((a_main__SSA__blk_5_2 Int))

:extrafuns ((a_main__SSA__blk_5_1 Int))
:extrafuns ((a_main Int))

:extrafuns ((__cil_tmp6_main Int))
:extrafuns ((__cil_tmp5_main Int))

:extrafuns ((__cil_tmp4_main Int))
:extrafuns ((VV Int))

:extrafuns ((UNCHECKED Int Int))
:extrafuns ((SKOLEM Int Int))

:extrafuns ((EWRITE Int))
:extrafuns ((EREAD Int))

:extrafuns ((DEREF Int Int))
:extrafuns ((BLOCK_END Int Int))

:extrafuns ((BLOCK_BEGIN Int Int))
:extrapreds ((k_9 Int Int Int Int Int Int Int))

; cid = 4
:assumption
(implies ((and (= __cil_tmp4_main (ite (< x_main__SSA__blk_0_1 0) 1 0)) (and (= x_main__SSA__blk_4_1 (- 0 x_main__SSA__blk_0_1)) (and (not (= __cil_tmp4_main 0)) (and (= VV x_main__SSA__blk_4_1) true))))) (k_9 EREAD EWRITE VV a_main x_main x_main__SSA__blk_0_1 x_main__SSA__blk_4_1))



; cid = 3
:assumption
(implies ((and (= __cil_tmp4_main (ite (< x_main__SSA__blk_0_1 0) 1 0)) (and (not (not (= __cil_tmp4_main 0))) (and (= VV x_main__SSA__blk_0_1) true)))) (k_9 EREAD EWRITE VV a_main x_main x_main__SSA__blk_0_1 x_main__SSA__blk_0_1))



; cid = 2
:assumption
(implies ((and (not (not (= VV 0))) (and (k_9 EREAD EWRITE x_main__SSA__phi_5 a_main x_main x_main__SSA__blk_0_1 x_main__SSA__phi_5) (and (= __cil_tmp4_main (ite (< x_main__SSA__blk_0_1 0) 1 0)) (and (= __cil_tmp5_main (ite (>= x_main__SSA__phi_5 0) 1 0)) (and (= a_main__SSA__blk_5_1 (ite (>= x_main__SSA__phi_5 0) 1 0)) (and (= a_main__SSA__blk_5_2 999) (and (= VV __cil_tmp5_main) true)))))))) false)



; cid = 1
:assumption
(implies ((and (not (not (= VV 0))) (and (k_9 EREAD EWRITE x_main__SSA__phi_5 a_main x_main x_main__SSA__blk_0_1 x_main__SSA__phi_5) (and (= __cil_tmp4_main (ite (< x_main__SSA__blk_0_1 0) 1 0)) (and (= __cil_tmp5_main (ite (>= x_main__SSA__phi_5 0) 1 0)) (and (= __cil_tmp6_main (ite (>= x_main__SSA__phi_5 0) 1 0)) (and (= a_main__SSA__blk_5_1 (ite (>= x_main__SSA__phi_5 0) 1 0)) (and (= a_main__SSA__blk_5_2 999) (and (= VV __cil_tmp6_main) true))))))))) false)

)