let g1 = 3
let g2 = 4
let g3 = 5
let rec max x y = if x > y then x else y

let g4 = max 1 2;;
let g5 = max g1 g2;;

assert(g4 < g5)
