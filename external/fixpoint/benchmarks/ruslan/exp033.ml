let rec sum n c = 
  if n <= 0 then 0, c
  else 
    let n', c' = sum (n - 1) (c + 1) in
    n + n', c'
