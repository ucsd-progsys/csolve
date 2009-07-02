let f0 (z: unit) = 0

let make_int n =
  if n = 0 then (f0 ()) else
    n

let check n = 
  assert (n = make_int n)
