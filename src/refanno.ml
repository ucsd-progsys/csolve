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

let mydebug = true 

(************************************************************************************)
(**************************** Location Tags******************************************)
(************************************************************************************)

type op  = Read | Write 
type tag = int * op

let tag_fresh = let x = ref 0 in fun o -> let _ = incr x in (!x, o)
let tag_eq    = (=)
let tag_dirty = function (_,Write) -> true | _ -> false 
let d_tag     = fun () t -> Pretty.dprintf "(%d, %b) " (fst t) (tag_dirty t)

(************************************************************************************)
(**************************** Type Definitions **************************************)
(************************************************************************************)

(** Gen: Generalize a concrete location into an abstract location
 *  WGen: Generalize a concrete location into an abstract location, but eschew subtyping
 *  Ins: Instantiate an abstract location with a concrete location
 *  New: Call-site instantiation of abs-loc-var with abs-loc-param
 *  NewC: Call-site instantiation of conc-loc-var NewC = (New;Ins) 
 NewC is an artifact of the fact that Sinfer only creates abstract locations *)
type annotation = 
  | Gen  of Sloc.t * Sloc.t             (* CLoc, ALoc *)
  | WGen of Sloc.t * Sloc.t             (* CLoc, ALoc *)
  | Ins  of Sloc.t * Sloc.t             (* ALoc, CLoc *)
  | New  of Sloc.t * Sloc.t             (* Xloc, Yloc *) 
  | NewC of Sloc.t * Sloc.t * Sloc.t    (* XLoc, Aloc, CLoc *) 

type block_annotation = annotation list list
type ctab = (string, Sloc.t) Hashtbl.t
type cncm = (Sloc.t * tag) Sloc.SlocMap.t
type soln = (cncm option * block_annotation) array

(************************************************************************************)
(****************** (DAG) Orderered Iterating Over CFGs *****************************)
(************************************************************************************)

let cfg_check cfg = 
  Array.iteri begin fun j is -> 
    asserts (j = 0 || List.exists ((>) j) is) "refanno: cfg_check fails"
  end cfg.Ssa.predecessors

(** val cfg_predmap: 
    cfg:Ssa.cfgInfo -> 
    (j:int -> is: ({v < j} * 'a) list -> rva:{'a option array | 0 <= idx < j => v <> None} -> 'a) -> 
    'a array *)
(* TBD: Range-Context-Types *)
let cfg_predmap cfg f = 
  let rva = Array.make cfg.Ssa.size None in 
  Misc.array_fold_lefti begin fun j rva is ->
    let rvj = is |> List.filter ((>) j) 
                 >> (fun x -> asserts (j=0 || x <> []) "refanno: cfg_predmap") 
                 |> List.map (Misc.pad_snd (Array.get rva <+> Misc.maybe)) 
                 |> f j in
    Array.set rva j (Some rvj); rva 
  end rva cfg.Ssa.predecessors  
  |> Array.map Misc.maybe

(*****************************************************************************)
(********************** Substitution *****************************************)
(*****************************************************************************)

let annotation_subs (sub: S.Subst.t) (a: annotation): annotation =
  let app = S.Subst.apply sub in
    match a with
      | Gen  (s1, s2)     -> Gen  (app s1, app s2)
      | WGen (s1, s2)     -> WGen (app s1, app s2)
      | Ins  (s1, s2)     -> Ins  (app s1, app s2)
      | New  (s1, s2)     -> New  (app s1, app s2)
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
  | WGen (cl, al) -> 
      Pretty.dprintf "WeakGeneralize(%a->%a) " Sloc.d_sloc cl Sloc.d_sloc al 
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

let d_conc () (conc:cncm) =
  let binds = LM.fold (fun k v acc -> (k, v) :: acc) conc [] in
  Pretty.seq (Pretty.text ";")
    (fun (al, (cl, t)) -> Pretty.dprintf "(%a |-> %a, %a) " Sloc.d_sloc al Sloc.d_sloc cl d_tag t)
    binds 

(* API *)
let d_conca = 
  Pretty.docArray 
    ~sep:(Pretty.text "\n")
    (fun i (cncm, cncm') -> Pretty.dprintf "block %i: @[CONC-In: %a@] @[CONC-Out: %a@]" i d_conc cncm d_conc cncm') 


(******************************************************************************)
(*********************** Operations on CONC-Maps ******************************)
(******************************************************************************)

let conc_size = Sloc.slm_bindings <+> List.length

let tag_join = function
  | t1, t2 when t1 = t2   -> t1
  | (_, Read), (_, Read)  -> tag_fresh Read
  | _                     -> tag_fresh Write


let conc_join (conc1:cncm) (conc2:cncm) : cncm = 
  LM.fold begin fun al (cl1, t1) conc ->
    if not (LM.mem al conc2) then conc else
      let cl2, t2 = LM.find al conc2 in
      if not (Sloc.eq cl1 cl2) then conc else
        LM.add al (cl1, tag_join (t1, t2)) conc
  end conc1 LM.empty
 
let conc_eq conc1 conc2 =
  let n1 = conc_size conc1 in
  let n2 = conc_size (conc_join conc1 conc2) in 
  n1 = n2

let conc_of_predecessors = function
  | [] -> LM.empty 
  | is -> Misc.list_reduce conc_join is

(******************************************************************************)
(*************************** Operations on Solns ******************************)
(******************************************************************************)

(* API *)
let soln_diff (sol1, sol2) = 
  let soln_size = Array.to_list <+> List.map fst <+> List.map (Misc.map_opt conc_size) in
  (sol1, sol2) |> Misc.map_pair soln_size |> Misc.uncurry (<>)

(* API *)
let soln_init cfg anna = 
  Array.init (Array.length cfg.Ssa.blocks) (fun i -> (None, anna.(i)))

  (*
let annots_of_edge iconc jconc =
  LM.fold begin fun al cl anns -> 
    if LM.mem al jconc then anns else (Gen (cl, al) :: anns)
  end iconc []

(* API *)
let soln_edgem cfg sol =
  Misc.array_fold_lefti begin fun j em is ->
    let jconc = is |> List.map (Array.get sol <+> fst) |> conc_of_predecessors in
    List.fold_left begin fun em i ->
      let iconc = fst (sol.(i)) in
      IIM.add (i,j) (annots_of_edge iconc jconc) em
    end em is 
  end IIM.empty cfg.Ssa.predecessors 
*)

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
  try Some (LM.find al conc) with Not_found -> None 

let instantiate f conc al cl rw =
  match rw, cloc_of_aloc conc al with
  | _, None -> 
    (LM.add al (cl, tag_fresh rw) conc, [f (al, cl)])
  | Write, Some (cl', t) when Sloc.eq cl' cl -> 
    (LM.add al (cl, tag_fresh Write) conc, [])
  | Read, Some (cl', t) when Sloc.eq cl' cl ->
    (conc, [])
  | _, Some (cl', (_, Write)) -> 
    (LM.add al (cl, tag_fresh rw) conc, [Gen (cl', al); f (al, cl)])
  | _, Some (cl', (_, Read)) -> 
    (LM.add al (cl, tag_fresh rw) conc, [WGen (cl', al); f (al, cl)])

(******************************************************************************)
(******************************** Visitors ************************************)
(******************************************************************************)

let annotate_set ctm theta conc = function
  (* v1 := *v2 *)
  | (Var v1, _), Lval (Mem (Lval (Var v2, _) as e), _) 
  | (Var v1, _), Lval (Mem (CastE (_, Lval (Var v2, _)) as e), _) ->
      let al = sloc_of_expr ctm e |> Misc.maybe in
      let cl = cloc_of_v theta v2 in
      instantiate (fun (x,y) -> Ins (x,y)) conc al cl Read

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
      instantiate (fun (x,y) -> Ins (x,y)) conc al cl Write
  
  (* Uh, oh! *)
  | lv, e -> 
      Errormsg.error "annotate_set: lv = %a, e = %a" Cil.d_lval lv Cil.d_exp e;
      assertf "annotate_set: unknown set"

let concretize_new conc = function
  | New (x,y) ->
      let _  = asserts (Sloc.is_abstract y) "concretize_new" in
      if Sloc.is_abstract x then 
        match cloc_of_aloc conc y with
        | None    -> 
            (* y has no conc instance   *)  
            (conc, [New (x,y)])
        | Some (y', (_, Write)) -> 
            (* y has a conc instance y' *)
            (LM.remove y conc, [Gen (y', y); New (x, y)])
        | Some (y', (_, Read)) -> 
            (* y has a conc instance y' *)
            (LM.remove y conc, [WGen (y', y); New (x, y)])
      else
        let cl = Sloc.fresh Sloc.Concrete in
        instantiate (fun (y,cl) -> NewC (x,y,cl)) conc y cl Read
  | _ -> assertf "concretize_new 2"

let generalize_global conc al =
  match cloc_of_aloc conc al with
  | None                 -> (conc, [])
  | Some (cl',(_,Write)) -> (LM.remove al conc, [Gen  (cl', al)])
  | Some (cl',(_,Read )) -> (LM.remove al conc, [WGen (cl', al)])

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
let annotate_instr globalslocs ctm theta (conc:cncm) = function
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

let d_instrucs () instrs = 
  Pretty.seq (Pretty.text "; ") (fun i -> Pretty.dprintf "%a" d_instr i) instrs 

let annotate_block globalslocs ctm theta j anns instrs (conc0 : cncm) =
  let _ = if mydebug then Printf.printf "annotate_block  %d \n" j in
  let _ = asserts (List.length anns = 1 + List.length instrs) "annotate_block: %d" j in
  List.combine (Misc.chop_last anns) instrs
  |> Misc.mapfold (annotate_instr globalslocs ctm theta) conc0 

  (* {{{ 
  List.fold_left begin fun (conc, anns') ainstr ->
       let conc', ann = annotate_instr globalslocs ctm theta conc ainstr in
       (conc', ann::anns')
     end (conc0, []) 

  let rv = (conc', List.rev anns') in
  let _  = if mydebug then
             ignore <| Pretty.printf 
             "\n annotate_block (%d): conc0 = %a, block= \n %a \n yields: \n %a \n conc' = %a \n" j
                         d_conc conc0 d_instrucs instrs 
                         d_block_annotation (snd rv)
                         d_conc (fst rv)
  in
  rv
}}} *)

let annot_iter cfg globalslocs ctm theta anna (sol : soln) : soln * bool = 
  let sol' = Array.copy sol in 
  let sol' = Misc.array_fold_lefti begin fun j sol' (_, ans) ->
      let conc = cfg.Ssa.predecessors.(j) 
                   |> Misc.map_partial (Array.get sol' <+> fst)
                   >> (fun cs -> asserts (j = 0 || cs <> []) "annot_iter %d" j)
                   |> conc_of_predecessors in
      let conc', ans' = match cfg.Ssa.blocks.(j).Ssa.bstmt.skind with
                   | Instr is -> annotate_block globalslocs ctm theta j anna.(j) is conc
                   | _        -> conc, ans in
      let _    = Array.set sol' j (Some conc', ans') in
      sol'
     end sol' sol in
  (sol', (soln_diff (sol, sol')))

let d_sol = 
  Pretty.docArray 
    ~sep:(Pretty.text "\n")
    (fun i (_,x) -> Pretty.dprintf "block %i: @[%a@]" i d_block_annotation x) 

(* {{{ 
let annot_iter cfg globalslocs ctm theta anna (sol : soln) : soln * bool = 
  let _  = Pretty.printf "CALLING annot_iter \n" in
  let rv = annot_iter cfg globalslocs ctm theta anna sol in
  let _  = Pretty.printf "AFTER annot_iter %b \n %a \n" (snd rv) d_sol (fst rv) in
  rv
}}} *)

(*****************************************************************************)
(********************************** API **************************************)
(*****************************************************************************)

let check_sol cfg globalslocs ctm theta anna sol (annota, conca, conca') = 
  failwith "TBD"

let annots_of_sol cfg sol = 
  let conca' = sol |> Array.map (fst <+> Misc.maybe) in
  let annota = sol |> Array.map snd in
  let conca  = Array.map (List.map (Array.get conca') <+> conc_of_predecessors) cfg.Ssa.predecessors in
  (annota, (Misc.array_combine conca conca'))

(* API *)
let annotate_cfg cfg globalslocs ctm anna =
  let theta = Hashtbl.create 17 in
  soln_init cfg anna 
  |> Misc.fixpoint (annot_iter cfg globalslocs ctm theta anna)
  |> fst
  |> annots_of_sol cfg
(* TBD >> check_sol cfg globalslocs ctm theta anna *)
  |> fun (annota, conca) -> (annota, conca, theta)

(* API *)
let cloc_of_varinfo theta v =
  try
    let l = Hashtbl.find theta v.vname in
    let _ = asserts (not (Sloc.is_abstract l)) "cloc_of_varinfo: absloc! (%s)" v.vname in
    Some l
  with Not_found -> 
    (* let _ = Errormsg.log "cloc_of_varinfo: unknown %s" v.vname in *)
    None
