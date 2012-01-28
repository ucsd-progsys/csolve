(benchmark unknown
:status unsat
:logic AUFLIA
:extrafuns ((x  Int))
:extrafuns ((y  Int))
:extrafuns ((z  Int))
:extrapreds ((P  Int Int))
:assumption
(implies (and (or (P x z) (= x z)) (= y (+ z 1))) (P x y))
:formula
(implies (and (P x y) (< y x)) false)
)
