(benchmark unknown
:status unsat
:logic AUFLIA
:extrafuns ((x  Int))
:extrafuns ((y  Int))
:extrafuns ((z  Int))
:extrapreds ((P0  Int Int))
:extrapreds ((P1  Int Int))
:assumption
(implies (= y (+ x 1)) (P0 x y))
:assumption
(implies (or (and (P0 x z) (P0 z y)) (= y (+ x 1))) (P1 x y))
:formula
(implies (and (P1 x y) (< y x)) false)
)
