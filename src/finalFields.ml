(* TODO
   1. Test with globals

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
module LD = CT.LDesc
module F  = CT.Field
module P  = Pretty
module RA = Refanno
module C  = Cil
module M  = Misc
module SM = M.StringMap
module Sh = Shape
module LM = S.SlocMap
module NA = NotAliased

open M.Ops

module Ploc = struct
  type t = CT.ploc

  let compare = compare
end

module PlocSet        = Set.Make (Ploc)
module PlocSetPrinter = P.MakeSetPrinter (PlocSet)

let d_plocset () ps =
  PlocSetPrinter.d_set ", " CT.d_ploc () ps

module type Context = sig
  val cfg   : Ssa.cfgInfo
  val shp   : Sh.t
  val ffmm  : PlocSet.t LM.t SM.t
  val sspec : CT.store
end

module IntraprocFinalFields (X: Context) = struct
  let kill_field_index l al i ffm =
    (* DEBUG *)
(*     let _   = P.printf "Trying to kill %a in\n" S.d_sloc l in *)
(*     let _   = P.printf "%a\n\n" (S.d_slocmap d_plocset) ffm in *)
    let ffs = LM.find l ffm in
    let ld  = CT.PreStore.find al X.shp.Sh.store in
    let ffs = ld |> CT.LDesc.find_index i |> List.fold_left (fun ffs (pl, _) -> PlocSet.remove pl ffs) ffs in
      LM.add l ffs ffm

  let process_set_lval na lval ffm =
    match lval with
      | (C.Mem (C.Lval (C.Var vi, _) as e), _)
      | (C.Mem (C.CastE (_, (C.Lval (C.Var vi, _) as e))), _) ->
          let cl = vi |> RA.cloc_of_varinfo X.shp.Sh.theta |> M.maybe in
          let al = cl |> RA.aloc_of_cloc X.shp.Sh.theta |> M.maybe in
            begin match CT.ExpMap.find e X.shp.Sh.etypm with
              | CT.CTRef (_, i) ->
                  let ffm = kill_field_index cl al i ffm in
                    if NA.NASet.mem (cl, al) na then
                      ffm
                    else
                      kill_field_index al al i ffm
              | _ -> assert false
            end
      | (C.Var _, C.NoOffset) -> ffm
      | _                     -> assert false

  let process_call na annots fname lvo ffm =
       annots
    |> List.fold_left begin fun ffm -> function
	 | RA.New (scallee, scaller) ->
             let _ = P.printf "Processing call to %s\n\n" fname in
	     let callee_ffm = SM.find fname X.ffmm in
	       LM.add scaller (PlocSet.inter (LM.find scaller ffm) (LM.find scallee callee_ffm)) ffm
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
      | C.Asm _ | C.Call _			          -> assert false

  let process_gen_inst annots ffm =
    List.fold_left begin fun ffm -> function
      | RA.Gen (cl, al) | RA.WGen (cl, al)      -> LM.add cl (LM.find al ffm) ffm
      | RA.Ins (_, _, cl) | RA. NewC (_, _, cl) -> LM.remove cl ffm
      | RA.New _                                -> ffm
    end ffm annots

  let process_instr na i annots (ffms, ffm) =
       ffm
    |> instr_update_finals na annots i
    |> process_gen_inst annots
    |> fun ffm -> (ffm :: ffms, ffm)

  let meet_finals ffm1 ffm2 =
    LM.fold begin fun l ps ffm ->
      try
        LM.add l (PlocSet.inter ps (LM.find l ffm2)) ffm
      with Not_found ->
        ffm
    end ffm1 LM.empty

  let add_succ_generalized_clocs conc_out succ_conc_in ffm =
    LM.fold begin fun al (cl, _) ffm ->
      if LM.mem al succ_conc_in && S.eq cl (LM.find al succ_conc_in |> fst) then
        ffm
      else
        LM.add cl (LM.find al ffm) ffm
    end conc_out ffm

  let merge_succs init_ffm ffmsa i =
    let conc_out = snd X.shp.Sh.conca.(i) in
      match X.cfg.Ssa.successors.(i) with
        | []    -> add_succ_generalized_clocs conc_out LM.empty init_ffm
        | succs ->
             succs
          |> List.map (fun j -> ffmsa.(j) |> snd)
          |> M.list_reduce meet_finals
          |> List.fold_right (fun j ffm -> add_succ_generalized_clocs conc_out (fst X.shp.Sh.conca.(j)) ffm) succs

  let process_block init_ffm ffmsa i =
    let ffm = merge_succs init_ffm ffmsa i in
      match X.cfg.Ssa.blocks.(i).Ssa.bstmt.C.skind with
        | C.Instr is -> M.fold_right3 process_instr (X.shp.Sh.nasa.(i)) is X.shp.Sh.anna.(i) ([], ffm)
        | _          -> ([], ffm)

  let fixed ffmsa ffmsa' =
    M.array_fold_lefti begin fun i b (ffms, ffm) ->
      b &&
        let (ffms', ffm') = ffmsa'.(i) in
          LM.equal PlocSet.equal ffm ffm' &&
            M.same_length ffms ffms' &&
            List.for_all2 (LM.equal PlocSet.equal) ffms ffms'
    end true ffmsa

  let dump_final_fields ffmsa =
    Array.iteri begin fun i (ffms, ffm) ->
      let _ = P.printf "Block %d:\n" i in
      let _ = P.printf "  I: %a\n" (S.d_slocmap d_plocset) ffm in
      let _ = M.fold_lefti (fun i _ ffm -> P.printf "  %i: %a\n" i (S.d_slocmap d_plocset) ffm |> ignore) () ffms in
      let _ = P.printf "\n" in
        ()
    end ffmsa

  let iter_finals init_ffm ffmsa =
    let ffmsa' = Array.copy ffmsa in
    let _      = M.array_rev_iteri (fun i _ -> ffmsa.(i) <- process_block init_ffm ffmsa i(* ; dump_final_fields ffmsa *)) X.cfg.Ssa.blocks in
      (ffmsa, not (fixed ffmsa ffmsa'))

  let with_all_fields_final sto ffm =
    LM.fold begin fun l ld ffm ->
      LM.add l (LD.fold (fun pls pl _ -> PlocSet.add pl pls) PlocSet.empty ld) ffm
    end sto ffm

  let init_abstract_finals () =
       LM.empty
    |> with_all_fields_final X.shp.Sh.store
    |> with_all_fields_final X.sspec

  let init_concrete_finals annots ffm =
    Array.fold_left begin fun ffm annotss ->
      List.fold_left begin fun ffm -> function
        | RA.Ins (_, al, cl) | RA.NewC (_, al, cl) ->
            begin try
              LM.add cl (LM.find al ffm) ffm
            with Not_found ->
              (* If we can't find the aloc, it's never read or written in this function. *)
              ffm |> LM.add cl PlocSet.empty |> LM.add al PlocSet.empty
            end
        | _                                        -> ffm
      end ffm (List.concat annotss)
    end ffm annots

  let final_fields () =
    let init_ffm = () |> init_abstract_finals |> init_concrete_finals X.shp.Sh.anna in
         Array.make (Array.length X.shp.Sh.nasa) ([], init_ffm)
      |> M.fixpoint (iter_finals init_ffm)
      |> fst
      >> dump_final_fields
      |> fun ffmsa -> ffmsa.(0) |> snd
end

module InterprocFinalFields = struct
  let iter_final_fields scis shpm storespec ffmm =
    SM.fold begin fun fname ffm (ffmm', reiter) ->
      if SM.mem fname shpm then
        let module X = struct
	  let shp   = SM.find fname shpm
	  let cfg   = (SM.find fname scis |> snd).Ssa_transform.cfg
          let sspec = storespec
	  let ffmm  = ffmm
        end in
        let module FF = IntraprocFinalFields (X) in
        let ffm'      = FF.final_fields () in
	  (SM.add fname ffm' ffmm', reiter || not (LM.equal PlocSet.equal ffm ffm'))
      else (ffmm', reiter)
    end ffmm (ffmm, false)

  let spec_final_fields (cf, _) =
    LM.map begin fun ld ->
      LD.fold (fun ffs pl fld -> if F.is_final fld then PlocSet.add pl ffs else ffs) PlocSet.empty ld
    end cf.CT.sto_out

  let shape_init_final_fields shp =
    LM.map (fun ld -> LD.fold (fun ffs pl fld -> PlocSet.add pl ffs) PlocSet.empty ld) shp.Sh.store

  let init_final_fields fspecm shpm =
       SM.empty
    |> SM.fold (fun fname spec ffmm -> SM.add fname (spec_final_fields spec) ffmm) fspecm
    |> SM.fold (fun fname shp ffmm -> SM.add fname (shape_init_final_fields shp) ffmm) shpm

  let set_nonfinal_fields shpm ffmm =
    SM.mapi begin fun fname shp ->
      {shp with
         Sh.store =
          LM.mapi begin fun l ld ->
            let ffs = SM.find fname ffmm |> LM.find l in
              LD.mapn begin fun _ pl fld ->
                if PlocSet.mem pl ffs then
                  F.set_finality F.Final fld
                else
                  F.set_finality F.Nonfinal fld
              end ld
          end shp.Sh.store}
    end shpm

  let final_fields fspecm storespec scis shpm =
       init_final_fields fspecm shpm
    |> Misc.fixpoint (iter_final_fields scis shpm storespec)
    |> fst
    |> set_nonfinal_fields shpm
end

let infer_final_fields fspecm storespec scis shpm =
     shpm
  |> InterprocFinalFields.final_fields fspecm storespec scis
(*   >> check_finality_specs fspec *)
