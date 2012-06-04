(* QUIRKS/BUGS
   Functions like read (void * ) are hard to specify:
   read () takes a pointer to a location A containing anything,
   i.e., "A |-> ".  The fields in this location should be
   non-final on account of the read, but we can't specify
   that yet.
*)

module Misc = FixMisc
module S  = Sloc
module CT = Ctypes.I
module LD = CT.LDesc
module F  = CT.Field
module Ix = Index
module IS = Ix.IndexSet
module P  = Pretty
module RA = Refanno
module C  = Cil
module M  = Misc
module SM = M.StringMap
module Sh = Shape
module LM = S.SlocMap
module NA = NotAliased

open M.Ops

module type Context = sig
  val cfg   : Ssa.cfgInfo
  val shp   : Sh.t
  val ffmm  : (IS.t LM.t * Sh.final_fields_annot array) SM.t
  val glocs : S.t list
end

let d_final_fields () ffmsa =
  P.docArray ~sep:P.line begin fun i (ffm, ffms) ->
    P.dprintf "Block %d:\n  %a\n%a"
      i
      (S.d_slocmap Ix.d_indexset) ffm
      (P.docList ~sep:P.line (fun ffm -> P.dprintf "  %a" (S.d_slocmap Ix.d_indexset) ffm)) ffms
  end () ffmsa

module Intraproc (X: Context) = struct
  (******************************************************************************)
  (****************************** Type Manipulation *****************************)
  (******************************************************************************)

  let find_stored_indices al i =
    al |> CT.Store.find_or_empty X.shp.Sh.store |> CT.LDesc.find i |> List.map fst

  let locs_of_lval = function
    | (C.Mem e, _) ->
        let vi = CilMisc.referenced_var_of_exp e in
        let cl = vi |> RA.cloc_of_varinfo X.shp.Sh.theta |> M.maybe in
        let al = Sloc.canonical cl in
          begin match CT.ExpMap.find e X.shp.Sh.etypm with
            | Ctypes.Ref (_, i) -> Some (cl, al, i)
            | _                 -> assert false
          end
    | (C.Var _, C.NoOffset) -> None
    | _                     -> assert false

  (******************************************************************************)
  (**************************** Output Sanity Checks ****************************)
  (******************************************************************************)

  let check_final_inclusion ffm1 ffm2 =
    LM.iter begin fun l ffs ->
      if S.is_abstract l then assert (IS.subset ffs (LM.find l ffm2))
    end ffm1;
    ffm2

  let check_pred_inclusion ffmsa ffm i =
    match ffmsa.(i) with
      | ffm2, [] -> check_final_inclusion ffm2 ffm |> ignore
      | _, ffms  -> check_final_inclusion (ffms |> List.rev |> List.hd) ffm |> ignore

  let check_lval_set ffm na lval =
    match locs_of_lval lval with
      | None             -> ()
      | Some (_, al, _) when al = Sloc.sloc_of_any -> () 
      | Some (cl, al, i) ->
           find_stored_indices al i
        |> List.iter begin fun pl ->
             assert (not (LM.mem cl ffm) || not (IS.mem pl (LM.find cl ffm)));
             assert (NA.NASet.mem (cl, al) na || (not (IS.mem pl (LM.find al ffm))))
           end

  let check_block_set ffm ffm' na i =
    begin match i with
      | C.Set (lval, _, _)          -> check_lval_set ffm na lval
      | C.Call (Some lval, _, _, _) -> check_lval_set ffm na lval
      | _                           -> ()
    end;
    ffm'

  let check_block_sets b ffm ffms nas =
    match b.Ssa.bstmt.C.skind with
      | C.Instr is -> M.fold_left3 check_block_set ffm ffms nas is |> ignore
      | _          -> ()

  let check_call_annots fname ffm annots =
    let ffmcallee = SM.find fname X.ffmm |> fst in
      List.iter begin function
        | RA.New (scallee, scaller) ->
            assert (IS.subset (LM.find scaller ffm) (LM.find scallee ffmcallee))
        | _ -> ()
      end annots

  let check_block_call ffm ffm' annots i =
    begin match i with
      | C.Call (_, C.Lval (C.Var vi, C.NoOffset), _, _) -> check_call_annots vi.C.vname ffm annots
      | C.Call _                                        -> assert false
      | _                                               -> ()
    end;
    ffm'

  let check_block_calls b ffm ffms annotss is =
    match b.Ssa.bstmt.C.skind with
      | C.Instr _ -> M.fold_left3 check_block_call ffm ffms annotss is |> ignore
      | _         -> ()

  let sanity_check_finals (ffmsa, _) =
    Array.iteri begin fun i b ->
      let ffm, ffms = ffmsa.(i) in
      let _         = List.fold_left check_final_inclusion LM.empty ffms in
      let _         = List.iter (check_pred_inclusion ffmsa ffm) (X.cfg.Ssa.predecessors.(i)) in
      let _         = check_block_sets b ffm ffms (X.shp.Sh.nasa.(i)) in
      let _         = check_block_calls b ffm ffms (X.shp.Sh.anna.(i)) in
        ()
    end X.cfg.Ssa.blocks

  (******************************************************************************)
  (******************************* Analysis Proper ******************************)
  (******************************************************************************)

  let kill_field_index l al i ffm =
    (* DEBUG *)
    (* let _   = P.printf "Trying to kill %a:%a in\n" S.d_sloc l Ix.d_index i in *)
    (* let _   = P.printf "%a\n\n" (S.d_slocmap d_plocset) ffm in *)
    LM.add l (List.fold_left (fun ffs pl -> IS.remove pl ffs) (LM.find l ffm) (find_stored_indices al i)) ffm

  let process_set_lval na lval ffm =
    match locs_of_lval lval with
      | Some (cl, al, i) ->
          let ffm = kill_field_index cl al i ffm in
            if NA.NASet.mem (cl, al) na then
              ffm
            else
              kill_field_index al al i ffm
      | None -> ffm

  let process_call na annots fname lvo ffm =
       annots
    |> List.fold_left begin fun ffm -> function
	 | RA.New (scallee, scaller) ->
             if CT.Store.mem X.shp.Sh.store scaller && not (List.mem scaller X.glocs) then
	       let callee_ffm = SM.find fname X.ffmm |> fst in
               if LM.mem scallee callee_ffm then
                 LM.add scaller (IS.inter (LM.find scaller ffm) (LM.find scallee callee_ffm)) ffm
               else ffm
             else ffm
	 | _ -> ffm
       end ffm
    |> fun ffm ->
	 match lvo with
	   | Some lval -> process_set_lval na lval ffm
	   | None      -> ffm

  let instr_update_finals na annots i ffm =
    match i with
      | C.Set (lval, _, _)	  	                  -> process_set_lval na lval ffm
      | C.Call (lvo, C.Lval (C.Var vi, C.NoOffset), _, _) -> process_call na annots vi.C.vname lvo ffm
      | C.Call (_, C.Lval (C.Mem _, C.NoOffset), _, _)    -> LM.map (fun _ -> IS.empty) ffm
      | C.Asm _ | C.Call _			          -> assert false

  let process_gen_inst annots ffm =
    List.fold_left begin fun ffm -> function
      | RA.Gen (cl, al) | RA.WGen (cl, al)      -> LM.add cl (LM.find al ffm) ffm
      | RA.Ins (_, _, cl) | RA. NewC (_, _, cl) -> LM.remove cl ffm
      | RA.New _ | RA.HInst _ | RA.TNew _ | RA.TInst _ -> ffm
    end ffm annots

  let process_instr na i annots (ffm, ffms) =
       ffm
    |> instr_update_finals na annots i
    |> process_gen_inst annots
    |> fun ffm' -> (ffm', ffm :: ffms)

  let meet_finals ffm1 ffm2 =
    LM.fold begin fun l ps ffm ->
      try
        LM.add l (IS.inter ps (LM.find l ffm2)) ffm
      with Not_found ->
        ffm
    end ffm1 LM.empty

  let add_succ_generalized_clocs conc_out succ_conc_in ffm =
    LM.fold begin fun al _ ffm ->
         al
      |> RA.clocs_of_aloc conc_out
      |> List.fold_left begin fun ffm cl ->
           if LM.mem al succ_conc_in && LM.mem cl (LM.find al succ_conc_in) then
             ffm
           else
             LM.add cl (LM.find al ffm) ffm
         end ffm
    end conc_out ffm

  let merge_succs init_ffm ffmsa i =
    let conc_out = snd X.shp.Sh.conca.(i) in
      match X.cfg.Ssa.successors.(i) with
        | []    -> add_succ_generalized_clocs conc_out LM.empty init_ffm
        | succs ->
             succs
          |> List.map (fun j -> ffmsa.(j) |> fst)
          |> M.list_reduce "merge_succs" meet_finals
          |> List.fold_right (fun j ffm -> add_succ_generalized_clocs conc_out (fst X.shp.Sh.conca.(j)) ffm) succs

  let process_block init_ffm ffmsa i =
    let ffm = merge_succs init_ffm ffmsa i in
      match X.cfg.Ssa.blocks.(i).Ssa.bstmt.C.skind with
        | C.Instr is -> M.fold_right3 process_instr (X.shp.Sh.nasa.(i)) is X.shp.Sh.anna.(i) (ffm, [])
        | _          -> (ffm, [])

  let fixed ffmsa ffmsa' =
    M.array_fold_lefti begin fun i b (ffm, ffms) ->
      b &&
        let (ffm', ffms') = ffmsa'.(i) in
          LM.equal IS.equal ffm ffm' &&
            M.same_length ffms ffms' &&
            List.for_all2 (LM.equal IS.equal) ffms ffms'
    end true ffmsa

  let iter_finals init_ffm ffmsa =
    let ffmsa' = Array.copy ffmsa in
    let _      = M.array_rev_iteri (fun i _ -> ffmsa.(i) <- process_block init_ffm ffmsa i) X.cfg.Ssa.blocks in
      (ffmsa, not (fixed ffmsa ffmsa'))

  let with_all_fields_final sto ffm =
    CT.Store.fold_locs begin fun l ld ffm ->
      LM.add l (LD.fold (fun pls pl _ -> IS.add pl pls) IS.empty ld) ffm
    end ffm sto

  let init_abstract_finals () =
       LM.empty
    |> List.fold_right (fun l lm -> LM.add l IS.empty lm) X.glocs
    |> with_all_fields_final X.shp.Sh.store

  let init_concrete_finals annots ffm =
    Array.fold_left begin fun ffm annotss ->
      List.fold_left begin fun ffm -> function
        | RA.Ins (_, al, cl) | RA.NewC (_, al, cl) ->
            begin try
              LM.add cl (LM.find al ffm) ffm
            with Not_found ->
              (* If we can't find the aloc, it's never read or written in this function. *)
              ffm |> LM.add cl IS.empty |> LM.add al IS.empty
            end
        | _ -> ffm
      end ffm (List.concat annotss)
    end ffm annots

  let final_fields () =
    let init_ffm = () |> init_abstract_finals |> init_concrete_finals X.shp.Sh.anna in
         Array.make (Array.length X.shp.Sh.nasa) (init_ffm, [])
      |> M.fixpoint (iter_finals init_ffm)
      >> sanity_check_finals
      |> fst
      |> fun ffmsa -> (ffmsa, ffmsa.(0) |> fst)
end

module Interproc = struct
  let iter_final_fields scis shpm globlocs ffmm =
    SM.fold begin fun fname (ffm, _) (ffmm', reiter) ->
      if SM.mem fname shpm then
        let module X = struct
	  let shp   = SM.find fname shpm
	  let cfg   = (scis |> SM.find fname |> snd3).Ssa_transform.cfg
	  let ffmm  = ffmm
          let glocs = globlocs
        end in
        let module IP   = Intraproc (X) in
        let ffmsa, ffm' = IP.final_fields () in
	  (SM.add fname (ffm', ffmsa) ffmm', reiter || not (LM.equal IS.equal ffm ffm'))
      else (ffmm', reiter)
    end ffmm (ffmm, false)

  let spec_final_fields (cf, _) =
    CT.Store.fold_locs begin fun l ld ffm ->
      LM.add l (LD.fold (fun ffs pl fld -> if F.is_final fld then IS.add pl ffs else ffs) IS.empty ld) ffm
    end LM.empty cf.Ctypes.sto_out

  let shape_init_final_fields shp =
    CT.Store.fold_locs begin fun l ld ffm ->
      LM.add l (LD.fold (fun ffs pl fld -> IS.add pl ffs) IS.empty ld) ffm
    end LM.empty shp.Sh.store

  let init_final_fields fspecm shpm =
    let empty_annot = Array.create 0 (LM.empty, []) in
         SM.empty
      |> SM.fold (fun fname spec ffmm -> SM.add fname (spec_final_fields spec, empty_annot) ffmm) fspecm
      |> SM.fold (fun fname shp ffmm -> SM.add fname (shape_init_final_fields shp, empty_annot) ffmm) shpm

  let set_nonfinal_fields shpm ffmm =
    SM.mapi begin fun fname shp ->
      {shp with
         Sh.ffmsa = SM.find fname ffmm |> snd;
         Sh.store =
          CT.Store.fold_locs begin fun l ld sto ->
            let ffs = SM.find fname ffmm |> fst |> LM.find l in
              CT.Store.add sto l begin
                LD.mapn begin fun _ i fld ->
                  if IS.mem i ffs then
                    F.set_finality fld Ctypes.Final
                  else
                    F.set_finality fld Ctypes.Nonfinal 
                end ld
              end
          end shp.Sh.store shp.Sh.store}
    end shpm

  let final_fields spec scis shpm =
       init_final_fields (Ctypes.I.Spec.funspec spec) shpm
    |> Misc.fixpoint (iter_final_fields scis shpm (spec |> Ctypes.I.Spec.store |> Ctypes.I.Store.domain))
    |> fst
    |> set_nonfinal_fields shpm
end

let check_location_finality fname l spec_store ld =
  if CT.Store.mem spec_store l then
    let spec_ld = CT.Store.find spec_store l in
      LD.iter begin fun i fld ->
           spec_ld
        |> LD.find i
        |> List.iter begin fun (_, spec_fld) ->
             match F.get_finality spec_fld, F.get_finality fld with
               | Ctypes.Final, Ctypes.Nonfinal ->
                   let _ = P.printf "Error: Field %a -> %a of function %s specified final, inferred nonfinal\n" S.d_sloc l Ix.d_index i fname in
                     assert false
               | _ -> ()
           end
      end ld

let check_finality_specs fspecm shpm =
  SM.iter begin fun fname shp ->
    let cf = SM.find fname fspecm |> fst in
      CT.Store.fold_locs begin fun l ld _ ->
        check_location_finality fname l cf.Ctypes.sto_in ld;
        check_location_finality fname l cf.Ctypes.sto_out ld
      end () shp.Sh.store
  end shpm

let infer_final_fields tgr spec scis shpm =
     shpm
  |> Interproc.final_fields spec scis
  >> check_finality_specs (Ctypes.I.Spec.funspec spec)
