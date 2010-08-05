module S  = Sloc
module CT = Ctypes
module P  = Pretty
module RA = Refanno
module C  = Cil
module M  = Misc

open Misc.Ops

module IndexSet = Set.Make (struct
  type t = CT.index

  let compare = compare
end)

module IndexSetPrinter = P.MakeSetPrinter (IndexSet)

module IntraprocNonFinalFields = struct
  type context = {
    cfg   : Ssa.cfgInfo;
    ctem  : CT.ctemap;
    conca : (RA.cncm * RA.cncm) array;
    ctab  : RA.ctab;
    annot : RA.block_annotation array;
  }

  let nonfinal_fields s nf =
    try S.SlocMap.find s nf with Not_found -> IndexSet.empty

  let process_annot fs = function
    | RA.Gen (s, _) | RA.WGen (s, _) -> S.SlocSet.remove s fs
    | RA.NewC (_, _, s)              -> S.SlocSet.add s fs
    | RA.Ins _ | RA.New _            -> fs

  let process_set ctx fs nf = function
    | C.Mem (C.Lval (C.Var vi, _) as e)
    | C.Mem (C.CastE (_, C.Lval (C.Var vi, _)) as e) ->
        let c = vi |> RA.cloc_of_varinfo ctx.ctab |> M.maybe in
          begin match CT.ExpMap.find e ctx.ctem with
            | CT.CTInt _      -> assert false
            | CT.CTRef (a, i) ->
                if S.SlocSet.mem c fs then
                  nf
                else
                  S.SlocMap.add a (IndexSet.add i (nonfinal_fields a nf)) nf
          end
    | _ -> nf

  let process_instr ctx fs nf = function
    | C.Set ((l, C.NoOffset), _, _) -> process_set ctx fs nf l
    | C.Call _                      -> nf
    | C.Set _ | C.Asm _             -> assert false

  let process_instr ctx (nf, fs) annot instr =
    let fs = List.fold_left process_annot fs annot in
    let nf = process_instr ctx fs nf instr in
      (nf, fs)

  let merge_preds ctx f j =
    if j = 0 then
      S.SlocSet.empty
    else
         ctx.cfg.Ssa.predecessors.(j)
      |> List.map (fun i -> f.(i))
      |> M.list_reduce S.SlocSet.inter
      |> S.SlocMap.fold (fun s _ fs -> S.SlocSet.remove s fs) (snd ctx.conca.(j))

  let process_block ctx f j nf b =
    let fs = merge_preds ctx f j in
      match b.Ssa.bstmt.C.skind with
        | C.Instr is ->
            let nf, fs = List.fold_left2 (process_instr ctx) (nf, fs) ctx.annot.(j) is in
            let _      = f.(j) <- fs in
              nf
        | _ -> f.(j) <- fs; nf

  let fixed nf nf' f f' =
    nf = nf' && M.array_fold_lefti (fun i b ss -> b && S.SlocSet.equal ss f'.(i)) true f

  let nonfinal_iter ctx f nf =
    let f'  = Array.copy f in
    let nf' = Misc.array_fold_lefti (process_block ctx f) nf ctx.cfg.Ssa.blocks in
      (nf', not (fixed nf nf' f f'))

  let all_clocs annot =
    Array.fold_left begin fun ss annots ->
      List.fold_left begin fun ss -> function
        | RA.Ins (_, s) | RA.NewC (_, _, s) -> S.SlocSet.add s ss
        | _                                 -> ss
      end ss (List.concat annots)
    end S.SlocSet.empty annot

  let dump_nonfinal nf =
    Pretty.printf "%a\n\n" (S.d_slocmap (IndexSetPrinter.d_set ", " CT.d_index)) nf

  let dump_fresh f =
    Array.iteri begin fun i fs ->
      Pretty.printf "%d |-> %a\n\n" i S.d_slocset fs |> ignore
    end f

  let find_nonfinal_fields cfg ctem conca ctab annot =
    let f   = Array.create (Array.length annot) (all_clocs annot) in
    let ctx = {cfg = cfg; ctem = ctem; conca = conca; ctab = ctab; annot = annot} in
    let res = S.SlocMap.empty |> Misc.fixpoint (nonfinal_iter ctx f) |> fst in
    let _ = dump_nonfinal res in
    let _ = dump_fresh f in
      res
end

let infer_final_fields cfg ctem conca ctab annot =
  let _ = IntraprocNonFinalFields.find_nonfinal_fields cfg ctem conca ctab annot in
    ()
