let annotate_instr sto 

let annotate_block sh globalslocs anna j =

let annot_iter cfg sh globalslocs anna =
  let do_block j (_, ans) =
    match cfg.Ssa.blocks.(j).Ssa.bstmt.skind with
    | Instr is -> annotate_block sto globalslocs ctm theta j anna.(j) is
    | _        -> ans in
  Misc.array_fold_lefti do_block [] sol

let annotate_cfg cfg sh globalsloc =
  
