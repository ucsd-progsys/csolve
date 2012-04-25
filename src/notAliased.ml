(* Must-Not-Alias Locations
   ------------------------

   For each instruction in the CFG, compute a set

     {(c1, a2), ..., (cn, an)}

   of pairs of locations such that ci and ai must not alias,
   i.e., no pointer to an ai can alias ci.
*)
module Misc = FixMisc
module S  = Sloc
module P  = Pretty
module M  = Misc
module CT = Ctypes.I
module RA = Refanno
module C  = Cil

open M.Ops

module NotAliased = struct
  type t = (* Concrete *) S.t * (* Abstract *) S.t

  let compare = compare

  let equal (cl1, al1) (cl2, al2) =
    S.eq cl1 cl2 && S.eq al1 al2

  let make cl al =
    begin
      assert ((S.is_abstract al || not (S.is_abstract cl)) ||
              (not (S.is_any al) && not (S.is_any cl)));
      (cl, al)
    end

  let d_not_aliased () (cl, al) =
    P.dprintf "%a / %a" S.d_sloc cl S.d_sloc al
end

module NASet        = Set.Make (NotAliased)
module NASetPrinter = P.MakeSetPrinter (NASet)

let d_naset () na =
  NASetPrinter.d_set ", " NotAliased.d_not_aliased () na

type block_annotation = NASet.t list

let dump_not_aliased nasa =
  if !Constants.verbose_level >= Constants.ol_finals then
    Array.iteri begin fun i (nas, na) ->
      let _ = P.printf "Block %d:\n" i in
      let _ = M.fold_lefti (fun i _ na -> P.printf "  %d: %a\n" i d_naset na |> ignore) () nas in
      let _ = P.printf "  F: %a\n\n" d_naset na in
        ()
    end nasa

type context = {
  cfg   : Ssa.cfgInfo;
  ctem  : CT.ctemap;
  conca : (RA.cncm * RA.cncm) array;
  annot : RA.block_annotation array;
}

(* abakst come back to this *)
let process_annot na = function
  | RA.Gen (cl, al) | RA.WGen (cl, al) -> NASet.remove (NotAliased.make cl al) na
  | RA.NewC (_, al, cl)                -> NASet.add (NotAliased.make cl al) na
  | RA.New _ | RA.Ins _ | RA.HInst _ | RA.TNew _ | RA.TInst _  -> na

let process_set ctx na = function
  | C.Mem _, e when not (C.isConstant e) ->
    begin match CT.ExpMap.find e ctx.ctem |> CT.CType.sloc with
      | Some s -> NASet.filter (fun (_, s2) -> not (S.eq s s2)) na
      | None   -> na
    end
  | _ -> na

let process_instr ctx (nas, na) annot instr =
  let na = List.fold_left process_annot na annot in
    match instr with
      | C.Set ((l, C.NoOffset), e, _) ->
          let na = process_set ctx na (l, e) in
            (na :: nas, na)
      | C.Call _          -> (na :: nas, na)
      | C.Set _ | C.Asm _ -> assert false

let merge_preds ctx nasa j =
  match ctx.cfg.Ssa.predecessors.(j) with
    | []    -> NASet.empty
    | preds ->
          preds
      |> List.map (fun i -> nasa.(i) |> snd)
      |> List.fold_left NASet.inter begin
           let _, conc = ctx.conca.(j) in
             S.SlocMap.fold begin fun al _ na ->
                  al
               |> RA.clocs_of_aloc conc
               |> List.fold_left (fun na cl -> NASet.add (NotAliased.make cl al) na) na
             end conc NASet.empty
         end

let process_block ctx nasa j b =
  let na = merge_preds ctx nasa j in
    match b.Ssa.bstmt.C.skind with
      | C.Instr is -> List.fold_left2 (process_instr ctx) ([], na) ctx.annot.(j) is |> M.app_fst List.rev
      | _          -> ([], na)

let fixed nasa nasa' =
  M.array_fold_lefti begin fun i b (nas, na) ->
    b &&
      let nas', na' = nasa'.(i) in
        NASet.equal na na' && List.for_all2 (NASet.equal) nas nas'
  end true nasa

let non_aliased_iter ctx nasa =
  let nasa' = Array.copy nasa in
  let _     = Array.iteri (fun i b -> nasa.(i) <- process_block ctx nasa i b) ctx.cfg.Ssa.blocks in
    (nasa, not (fixed nasa nasa'))

let all_disjoint annot =
  Array.fold_left begin fun na annotss ->
    List.fold_left begin fun na -> function
      | RA.NewC (_, al, cl) -> NASet.add (NotAliased.make cl al) na
      | _                   -> na
    end na (List.concat annotss)
  end NASet.empty annot

let initial_nasa ctx =
  let annot = ctx.annot in
  let ad    = all_disjoint annot in
    Array.init (Array.length annot) (fun i -> (List.map (fun _ -> ad) annot.(i), ad))

let non_aliased_locations cfg ctem conca annot =
  let ctx = {cfg = cfg; ctem = ctem; conca = conca; annot = annot} in
       ctx
    |> initial_nasa
    |> Misc.fixpoint (non_aliased_iter ctx)
    >> (fst <+> dump_not_aliased)
    |> fst
    |> Array.map fst
