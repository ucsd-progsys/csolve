module E = EnvTBA

let mk_constr =
  let id = ref 0 in
  fun fid grd lhs rhs ->
    (envTBA, grd, lhs, rhs, (Some (incr id; !id)))

let env_of_cilenv cm =
