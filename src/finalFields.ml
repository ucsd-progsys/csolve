(* TODO
   1. Test with globals
   4. Checking all present functions against spec annotation
        Because genspec won't generate finality specs,
        we have to check the inferred spec is a *refinement*
        of the given spec.
   5. Do-nothing test against regrtest
        Don't use the final field info, just make sure no regression
        test broke with all the refactoring.

   QUIRKS/BUGS
   1. genspec can't produce final field declarations
        genspec is totally type-driven and does no analysis
        whatsoever.
   2. Functions like read (void * ) are hard to specify:
        read () takes a pointer to a location A containing anything,
        i.e., "A |-> ".  The fields in this location should be
        non-final on account of the read, but we can't specify
        that yet.
*)

module S  = Sloc
module CT = Ctypes
module P  = Pretty
module RA = Refanno
module C  = Cil
module M  = Misc
module SM = M.StringMap
module SI = ShapeInfra

open M.Ops

module IndexSet = Set.Make (struct
  type t = CT.index

  let compare = compare
end)

module IndexSetPrinter = P.MakeSetPrinter (IndexSet)

let nonfinal_fields s nfm =
  try S.SlocMap.find s nfm with Not_found -> IndexSet.empty

let nonfinal_equal nfm1 nfm2 =
  S.SlocMap.equal IndexSet.equal nfm1 nfm2

let dump_nonfinal nfm =
  Pretty.printf "%a\n\n" (S.d_slocmap (IndexSetPrinter.d_set ", " CT.d_index)) nfm

module IntraprocNonFinalFields = struct
  type context = {
    cfg   : Ssa.cfgInfo;
    ctem  : CT.ctemap;
    conca : (RA.cncm * RA.cncm) array;
    ctab  : RA.ctab;
    annot : RA.block_annotation array;
  }

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
    match ctx.cfg.Ssa.predecessors.(j) with
      | []    -> S.SlocSet.empty
      | preds ->
           preds
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
    nonfinal_equal nf nf' && M.array_fold_lefti (fun i b ss -> b && S.SlocSet.equal ss f'.(i)) true f

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

  let dump_fresh f =
    Array.iteri (fun i fs -> Pretty.printf "%d |-> %a\n\n" i S.d_slocset fs |> ignore)

  let find_nonfinal_fields cfg shp =
    let f   = Array.create (Array.length shp.SI.anna) (all_clocs shp.SI.anna) in
    let ctx = {cfg = cfg; ctem = shp.SI.etypm; conca = shp.SI.conca; ctab = shp.SI.theta; annot = shp.SI.anna} in
      S.SlocMap.empty |> Misc.fixpoint (nonfinal_iter ctx f) |> fst
end

module InterprocNonFinalFields = struct
  let proc_nonfinal_fields shpm fname (_, {Ssa_transform.cfg = cfg}) fnfm =
    SM.add fname (SM.find fname shpm |> fst |> IntraprocNonFinalFields.find_nonfinal_fields cfg) fnfm

  let process_call callee_nfm nfm annots =
    List.fold_left begin fun nfm -> function
      | RA.New (sfrom, sto) | RA.NewC (sfrom, sto, _) ->
          S.SlocMap.add sto (IndexSet.union (nonfinal_fields sfrom callee_nfm) (nonfinal_fields sto nfm)) nfm
      | _ -> nfm
    end nfm annots

  let process_instr fnfm nfm annots = function
    | C.Call (_, C.Lval (C.Var vi, C.NoOffset), _, _) -> process_call (SM.find (vi.C.vname) fnfm) nfm annots
    | C.Call _                                        -> assert false
    | _                                               -> nfm

  let process_block fnfm nfm annotss b =
    match b.Ssa.bstmt.C.skind with
      | C.Instr is -> List.fold_left2 (process_instr fnfm) nfm annotss is
      | _          -> nfm

  let process_fun shpm fname (_, {Ssa_transform.cfg = cfg}) fnfm =
    let anna = (SM.find fname shpm |> fst).SI.anna in
    let nfm  = M.array_fold_lefti (fun i nfm b -> process_block fnfm nfm anna.(i) b) (SM.find fname fnfm) cfg.Ssa.blocks in
      SM.add fname nfm fnfm

  let funspec_nonfinal_fields cf =
    S.SlocMap.fold begin fun s ld nfm ->
      if S.is_abstract s then
        let nfs = nonfinal_fields s nfm in
        let po  = CT.LDesc.get_period ld in
             ld
          |> CT.LDesc.nonfinal_fields
          |> List.fold_left begin fun nfs (pl, _) ->
               match pl, po with
                 | CT.PLAt _, _       -> IndexSet.add (CT.index_of_ploc pl 0) nfs
                 | CT.PLSeq _, Some p -> IndexSet.add (CT.index_of_ploc pl p) nfs
                 | _                  -> assert false
             end nfs
          |> fun nfs -> S.SlocMap.add s nfs nfm
      else
        nfm
    end cf.CT.sto_out S.SlocMap.empty

  let iter_calls shpm fm fnfm =
    let fnfm' = SM.fold (process_fun shpm) fm fnfm in
      (fnfm', not (SM.equal nonfinal_equal fnfm fnfm'))

  let infer_nonfinal_fields fspec fm shpm =
       SM.empty
    |> SM.fold (fun fname (cf, _) fnfm -> SM.add fname (funspec_nonfinal_fields cf) fnfm) fspec
    |> SM.fold (proc_nonfinal_fields shpm) fm
    |> Misc.fixpoint (iter_calls shpm fm)
    |> fst
end

let dump_proc_nonfinals fnfm =
    SM.iter begin fun fname nfm ->
      P.printf "Function %s:\n" fname |> ignore;
      dump_nonfinal nfm |> ignore;
      P.printf "\n\n" |> ignore
    end fnfm

let infer_nonfinal_fields fspec fm shpm =
     InterprocNonFinalFields.infer_nonfinal_fields fspec fm shpm
  >> fun fnfm ->
       if Constants.ck_olev Constants.ol_solve then begin
         P.printf "Interproc nonfinal fields:\n\n" |> ignore;
         dump_proc_nonfinals fnfm
     end
