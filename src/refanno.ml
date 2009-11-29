(*
 * Copyright © 1990-2009 The Regents of the University of California. All rights reserved. 
 *
 * Permission is hereby granted, without written agreement and without 
 * license or royalty fees, to use, copy, modify, and distribute this 
 * software and its documentation for any purpose, provided that the 
 * above copyright notice and the following two paragraphs appear in 
 * all copies of this software. 
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY 
 * FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES 
 * ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN 
 * IF THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY 
 * OF SUCH DAMAGE. 
 * 
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES, 
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY 
 * AND FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE PROVIDED HEREUNDER IS 
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION 
 * TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 *
 *)

(* This file is part of the liquidC Project.*)

module IIM = Misc.IntIntMap
module LM  = Sloc.SlocMap
module SM  = Misc.StringMap
module S   = Sloc

open Cil
open Misc.Ops

(** Gen: Generalize a concrete location into an abstract location
 *  Ins: Instantiate an abstract location with a concrete location
 *  New: Call-site instantiation of abs-loc-var with abs-loc-param
 *  NewC: Call-site instantiation of conc-loc-var NewC = (New;Ins) 
 NewC is an artifact of the fact that Sinfer only creates abstract locations *)
type annotation = 
  | Gen  of Sloc.t * Sloc.t             (* CLoc, ALoc *)
  | Ins  of Sloc.t * Sloc.t             (* ALoc, CLoc *)
  | New  of Sloc.t * Sloc.t             (* Xloc, Yloc *) 
  | NewC of Sloc.t * Sloc.t * Sloc.t    (* XLoc, Aloc, CLoc *) 

type block_annotation = annotation list list
type ctab = (string, Sloc.t) Hashtbl.t
type soln = (Sloc.t Sloc.SlocMap.t option * block_annotation) array

(*****************************************************************************)
(********************** Substitution *****************************************)
(*****************************************************************************)

let annotation_subs (sub: S.Subst.t) (a: annotation): annotation =
  let app = S.Subst.apply sub in
    match a with
      | Gen (s1, s2)      -> Gen (app s1, app s2)
      | Ins (s1, s2)      -> Ins (app s1, app s2)
      | New (s1, s2)      -> New (app s1, app s2)
      | NewC (s1, s2, s3) -> NewC (app s1, app s2, app s3)

(* API *)
let subs (sub: S.Subst.t): block_annotation -> block_annotation =
  List.map (List.map (annotation_subs sub))

(*****************************************************************************)
(********************** Pretty Printing **************************************)
(*****************************************************************************)

let d_annotation () = function
  | Gen (cl, al) -> 
      Pretty.dprintf "Generalize(%a->%a) " Sloc.d_sloc cl Sloc.d_sloc al 
  | Ins (al, cl) -> 
      Pretty.dprintf "Instantiate(%a->%a) " Sloc.d_sloc al Sloc.d_sloc cl 
  | New (al, cl) -> 
      Pretty.dprintf "New(%a->%a) " Sloc.d_sloc al Sloc.d_sloc cl 
  | NewC (cl, al, cl') -> 
      Pretty.dprintf "NewC(%a->%a->%a) " Sloc.d_sloc cl Sloc.d_sloc al Sloc.d_sloc cl'

let d_annotations () anns = 
  Pretty.seq (Pretty.text ", ") 
    (fun ann -> Pretty.dprintf "%a" d_annotation ann) 
    anns

let d_block_annotation () annss =
  Misc.numbered_list annss
  |> Pretty.d_list "\n" (fun () (i,x) -> Pretty.dprintf "%i: %a" i d_annotations x) ()

(* API *)
let d_block_annotation_array =
  Pretty.docArray 
    ~sep:(Pretty.text "\n")
    (fun i x -> Pretty.dprintf "block %i: @[%a@]" i d_block_annotation x) 

(* API *)
let d_ctab () t = 
  let vcls = Misc.hashtbl_to_list t in
  Pretty.seq (Pretty.text "\n") 
    (fun (vn, cl) -> Pretty.dprintf "Theta(%s) = %a \n" vn Sloc.d_sloc cl) 
    vcls

(* API *)
let d_edgem () em =
  let eanns = IIM.fold (fun k v acc -> (k,v) :: acc) em [] in
  Pretty.seq (Pretty.text "\n")
    (fun ((i,j), anns) -> Pretty.dprintf "(%d -> %d) = %a \n" i j d_annotations anns)
    eanns

(******************************************************************************)
(*********************** Operations on Solutions ******************************)
(******************************************************************************)

let conc_size conc = 
  LM.fold (fun _ _ n -> n + 1) conc 0 

let conc_join conc1 conc2 = 
  LM.fold begin fun al cl conc ->
    if not (LM.mem al conc2) then conc else
      if not (Sloc.eq cl (LM.find al conc2)) then conc else
        LM.add al cl conc
  end conc1 LM.empty
 
let conc_eq conc1 conc2 = 
  (conc_size conc1) = (conc_size (conc_join conc1 conc2))

(* API *)
let conc_of_preds cfg sol = function
  | [] -> Some LM.empty
  | is -> is |> Misc.map_partial (Array.get sol <+> fst)    
             |> (function [] -> None | concs -> Some (Misc.list_reduce conc_join concs))

(* API *)
let soln_diff (sol1, sol2) = 
  let soln_size = Array.to_list <+> List.map fst <+> List.map (Misc.map_opt conc_size) in
  (sol1, sol2) |> Misc.map_pair soln_size |> Misc.uncurry (<>)

(* API *)
let soln_init cfg anna = 
  Array.init (Array.length cfg.Ssa.blocks) (fun i -> (None, anna.(i)))

(***************************************************************************************)

let fresh_cloc () =
  Sloc.fresh Sloc.Concrete

let cloc_of_v theta v =
  Misc.do_memo theta fresh_cloc () v.vname

let loc_of_var_expr theta =
  let rec loc_rec = function
    | Lval (Var v, _) when isPointerType v.vtype ->
        Some (cloc_of_v theta v)
    | BinOp (_, e1, e2, _) ->
        let l1 = loc_rec e1 in
        let l2 = loc_rec e2 in
        (match (l1, l2) with
          | (None, l) | (l, None) -> l
          | (Some l1, Some l2) when Sloc.eq l1 l2 -> Some l1
          | _ -> None)
    | CastE (_, e) -> loc_rec e
    | _ -> None in
  loc_rec

let sloc_of_expr ctm e =
  match Ctypes.ExpMap.find e ctm with
  | Ctypes.CTInt _      -> None
  | Ctypes.CTRef (s, _) -> Some s

let cloc_of_aloc conc al =
  try LM.find al conc with Not_found -> al

let instantiate f conc al cl =
  let cl' = cloc_of_aloc conc al in
  if Sloc.eq cl' al then 
    (* al has no conc representative *) 
    (LM.add al cl conc, [f (al, cl)])
  else if Sloc.eq cl' cl then
    (* al conc representative is cl *)
    (conc, [])
  else 
    (* al conc representative is some other cl' *)
    (LM.add al cl conc, [Gen (cl', al); f (al, cl)])

(******************************************************************************)
(******************************** Visitors ************************************)
(******************************************************************************)

let annotate_set ctm theta conc = function
  (* v1 := *v2 *)
  | (Var v1, _), Lval (Mem (Lval (Var v2, _) as e), _) 
  | (Var v1, _), Lval (Mem (CastE (_, Lval (Var v2, _)) as e), _) ->
      let al = sloc_of_expr ctm e |> Misc.maybe in
      let cl = cloc_of_v theta v2 in
      instantiate (fun (x,y) -> Ins (x,y)) conc al cl 
  
  (* v := e *)
  | (Var v, _), e ->
      let _ = CilMisc.check_pure_expr e in
      loc_of_var_expr theta e
      |> Misc.maybe_iter (Hashtbl.replace theta v.vname) 
      |> fun _ -> (conc, [])

  (* *v := _ *)
  | (Mem (Lval (Var v, _) as e), _), _ 
  | (Mem (CastE (_, Lval (Var v, _)) as e), _), _ ->
      let al = sloc_of_expr ctm e |> Misc.maybe in
      let cl = cloc_of_v theta v in
      instantiate (fun (x,y) -> Ins (x,y)) conc al cl   
  
  (* Uh, oh! *)
  | lv, e -> 
      Errormsg.error "annotate_set: lv = %a, e = %a" Cil.d_lval lv Cil.d_exp e;
      assertf "annotate_set: unknown set"

let concretize_new conc = function
  | New (x,y) ->
      let _  = asserts (Sloc.is_abstract y) "concretize_new" in
      if Sloc.is_abstract x then 
        let y' = cloc_of_aloc conc y in
        if Sloc.eq y y' then 
          (* y has no conc instance   *)  
          (conc, [New (x,y)])
        else
          (* y has a conc instance y' *)
          (LM.remove y conc, [Gen (y', y); New (x, y)]) 
      else
        (* let _  = asserts (Sloc.is_abstract y) "concretize_new" in *)
        let cl = Sloc.fresh Sloc.Concrete in
        instantiate (fun (y,cl) -> NewC (x,y,cl)) conc y cl
  | _ -> assertf "concretize_new 2"

let generalize_global conc al =
  match cloc_of_aloc conc al with
  | al' when Sloc.eq al al' -> (conc, [])
  | al'                     -> (LM.remove al conc, [Gen (al', al)])

let rec new_cloc_of_aloc al = function
  | NewC (_,al',cl) :: ns when Sloc.eq al al' -> Some cl
  | _ :: ns -> new_cloc_of_aloc al ns
  | []  -> None

let sloc_of_ret ctm theta (conc, anns) = function 
  | (Var v, NoOffset) as lv ->
      sloc_of_expr ctm (Lval lv) 
      |>> fun al -> new_cloc_of_aloc al anns
      |>> fun cl -> Hashtbl.replace theta v.vname cl; None
      |>> fun _  -> None 
  | lv ->
      Errormsg.error "sloc_of_ret: %a" Cil.d_lval lv;
      assertf "sloc_of_ret"

(* ns : New (al,_) list, with distinct al *)
let annotate_instr globalslocs ctm theta conc = function
  | ns, Cil.Call (_, Lval ((Var fv), NoOffset), _,_) 
    when Constants.is_pure_function fv.vname ->
      conc, ns 

  | ns, Cil.Call (lvo,_,_,_) ->
      let conc, anns  = Misc.mapfold concretize_new conc ns in
      let conc, anns' = Misc.mapfold generalize_global conc globalslocs in
      let conc_anns   = (conc, Misc.flatten (anns ++ anns')) in
      let _           = lvo |>> sloc_of_ret ctm theta conc_anns in
      conc_anns

  | _, Cil.Set (lv, e, _) -> 
      annotate_set ctm theta conc (lv, e)

  | _, instr ->
      Errormsg.error "annotate_instr: %a" Cil.d_instr instr;
      assertf "TBD: annotate_instr"

(*****************************************************************************)
(******************************** Fixpoint ***********************************)
(*****************************************************************************)

let annotate_block globalslocs ctm theta anns instrs conc0 =
  let _ = asserts (List.length anns = 1 + List.length instrs) "annotate_block" in
  let ainstrs = List.combine (Misc.chop_last anns) instrs in
  let conc', anns' =
    List.fold_left begin fun (conc, anns') ainstr -> 
      let conc', ann = annotate_instr globalslocs ctm theta conc ainstr in
      (conc', ann::anns')
    end (conc0, []) ainstrs in
  (Some conc', List.rev anns')

let annot_iter cfg globalslocs ctm theta anna (sol : soln) : soln * bool = 
  sol |> Array.mapi begin fun i x ->
           let sk  = cfg.Ssa.blocks.(i).Ssa.bstmt.skind in
           let cin = conc_of_preds cfg sol cfg.Ssa.predecessors.(i) in
           match cin, sk  with
           | Some conc, Instr is -> annotate_block globalslocs ctm theta anna.(i) is conc
           | _                   -> x 
         end
      |> (fun sol' -> (sol', (soln_diff (sol, sol'))))

(*****************************************************************************)
(***************************** Edge Annotations ******************************)
(*****************************************************************************)

let annotate_edge = function
  | None, _ -> 
      assertf "Refanno.annotate_edge: Undefined CONC (src)"
  | _, None -> 
      assertf "Refanno.annotate_edge: Undefined CONC (dst)"
  | Some iconc, Some jconc ->
      LM.fold begin fun al cl anns -> 
        if LM.mem al jconc then anns else (Gen (cl, al) :: anns)
      end iconc []

let mk_edgem cfg sol =
  Misc.array_fold_lefti begin fun j em is ->
    let jconco = conc_of_preds cfg sol is in
    List.fold_left begin fun em i ->
      let iconco = fst (sol.(i)) in
      IIM.add (i,j) (annotate_edge (iconco, jconco)) em
    end em is 
  end IIM.empty cfg.Ssa.predecessors 
  
(* {{{ FOLD ALL conc-locs 

let annotate_end conc =
  LM.fold (fun al cl anns -> (Gen (cl, al)) :: anns) conc []

Misc.array_fold_lefti begin fun i em js ->
   let anns = sol.(i) |> fst |> Misc.get_option LM.empty |> annotate_end in 
   let js   = cfg.Ssa.successors.(i) in
   List.fold_left (fun em j -> IIM.add (i,j) anns em) em js 
 end IIM.empty cfg.Ssa.successors
 
 }}} *)


(*****************************************************************************)
(********************************** API **************************************)
(*****************************************************************************)

let apply_annot conc = function
  | Gen (cl, al) -> LM.remove al conc
  | Ins (al, cl) -> LM.add al cl conc
  | _            -> conc

let apply_annots conc anns =
  List.fold_left apply_annot conc anns

let reconstruct_conca cfg annota egenm = failwith "TBD"


(** See refanno.mli for details on INVARIANTS *)
let check_annots cfg annota egenm = 
  let conca, conca' = reconstruct_conca cfg annota egenm in
  Array.iteri begin fun i iconc ->
    let iconc' =  List.flatten annota.(i) 
               |> List.fold_left apply_annot iconc in
    asserts (conc_eq iconc' conca'.(i)) "Refanno: INVARIANT 1 Fails on %d \n" i
  end conca;
  IIM.iter begin fun (i,j) anns ->
    let ijconc = List.fold_left apply_annot (conca'.(i)) anns in
    asserts (conc_eq ijconc conca.(i)) "Refanno: INVARIANT 2 Fails on (%d,%d) \n" i j
  end egenm


(* API *)
let annotate_cfg cfg globalslocs ctm anna =
  let theta  = Hashtbl.create 17 in
  let sol    = soln_init cfg anna 
               |> Misc.fixpoint (annot_iter cfg globalslocs ctm theta anna)
               |> fst in
  let annota = Array.map snd sol in
  let egenm  = mk_edgem cfg sol in  
  let _      = check_annots cfg annota egenm in
  (annota, egenm, theta)

(* API *)
let cloc_of_varinfo theta v =
  try
    let l = Hashtbl.find theta v.vname in
    let _ = asserts (not (Sloc.is_abstract l)) "cloc_of_varinfo: absloc! (%s)" v.vname in
    Some l
  with Not_found -> 
    (* let _ = Errormsg.log "cloc_of_varinfo: unknown %s" v.vname in *)
    None

(***************************************************************************)
(************************** Reconstruction *********************************)
(***************************************************************************)
(*
let lesser_pred i =
  try i |> preds |> List.filter ((>) i) |> List.hd
  with _ -> assertf "lesser_pred" 

let reconstruct A B = 
   for i in 1..n :
     Ci(i) := if i = 0 then emp else 
                let j = lesser_pred i in
                UPD(Co(j), A(j,i))
     Co(i) := UPD(Ci(i), B(i))
   return Ci, Co
*)
