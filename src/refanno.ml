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

module Misc = FixMisc
module M = Misc
module IIM = Misc.IntIntMap
module LM  = Sloc.SlocMap
module SM  = Misc.StringMap
module S   = Sloc
  

open Cil
open Misc.Ops

let mydebug = false

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
  | Ins  of string * Sloc.t * Sloc.t    (* ptr var, ALoc, CLoc *)
  | New  of Sloc.t * Sloc.t             (* Xloc, Yloc *) 
  | NewC of Sloc.t * Sloc.t * Sloc.t    (* XLoc, Aloc, CLoc *) 
  | HInst of Ctypes.StoreSubst.t
  | TNew of Ctypes.tvar * Ctypes.tvar
  | TInst of Ctypes.IndexTypes.TVarInst.t

type block_annotation = annotation list list
type ctab = (string, Sloc.t) Hashtbl.t

type tagm = tag Sloc.SlocMap.t
type cncm = tagm Sloc.SlocMap.t
type soln = (cncm option * block_annotation) array

module TagMPrinter = Pretty.MakeMapPrinter (Sloc.SlocMap)

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
      | Ins  (v, s1, s2)  -> Ins  (v, app s1, app s2)
      | New  (s1, s2)     -> New  (app s1, app s2)
      | NewC (s1, s2, s3) -> NewC (app s1, app s2, app s3)
      | HInst s -> HInst (Ctypes.StoreSubst.subs sub s)
      | TInst inst -> TInst (inst |>: (M.app_snd (Ctypes.I.CType.subs sub)))
      | TNew  (f,t) -> TNew (f,t)
      | _ -> a

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
  | Ins (v, al, cl) ->
      Pretty.dprintf "Instantiate(*%s:%a->%a) " v Sloc.d_sloc al Sloc.d_sloc cl
  | New (al, cl) -> 
      Pretty.dprintf "New(%a->%a) " Sloc.d_sloc al Sloc.d_sloc cl 
  | NewC (cl, al, cl') -> 
      Pretty.dprintf "NewC(%a->%a->%a) " Sloc.d_sloc cl Sloc.d_sloc al Sloc.d_sloc cl'
  | HInst ss -> 
      Pretty.dprintf "HInst(%a)" Ctypes.StoreSubst.d_subst ss
  | TNew (f, t) ->
      Pretty.dprintf "TNew(%a->%a)" Ctypes.d_tvar f Ctypes.d_tvar t
  | TInst i ->
      Pretty.dprintf "TInst (%a)" Ctypes.IndexTypes.TVarInst.d_inst i
       

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

let d_tagm =
  TagMPrinter.d_map
    ~dmaplet:(fun s t -> Pretty.dprintf "(%t, %t)" (fun () -> s) (fun () -> t))
    ", "
    Sloc.d_sloc
    d_tag

let d_conc () (conc:cncm) =
  let binds = LM.fold (fun k v acc -> (k, v) :: acc) conc [] in
  Pretty.seq (Pretty.text ";")
    (fun (al, tm) -> Pretty.dprintf "(%a |-> %a) " Sloc.d_sloc al d_tagm tm)
    binds 

(* API *)
let d_conca = 
  Pretty.docArray 
    ~sep:(Pretty.text "\n")
    (fun i (cncm, cncm') -> Pretty.dprintf "block %i: @[CONC-In: %a@] @[CONC-Out: %a@]" i d_conc cncm d_conc cncm') 

let d_theta () th = 
  th |> Misc.hashtbl_to_list 
     |> Pretty.docList ~sep:(Pretty.text ",") (fun (vn,cl) -> Pretty.dprintf "(%s := %a)" vn Sloc.d_sloc cl) ()

let d_cncms = Pretty.docList ~sep:(Pretty.text " --- ") (d_conc ())

let d_instrucs () instrs = 
  Pretty.seq (Pretty.text "; ") (fun i -> Pretty.dprintf "%a" d_instr i) instrs 

let d_sol = 
  Pretty.docArray 
    ~sep:(Pretty.text "\n")
    (fun i (_,x) -> Pretty.dprintf "block %i: @[%a@]" i d_block_annotation x) 

(******************************************************************************)
(*********************** Operations on CONC-Maps ******************************)
(******************************************************************************)

let conc_size conc =
       conc
   |>  Sloc.slm_bindings
   |>: snd
   |>: Sloc.slm_bindings
   |>  List.concat
   |>  List.length

let tag_join = function
  | t1, t2 when t1 = t2   -> t1
  | (_, Read), (_, Read)  -> tag_fresh Read
  | _                     -> tag_fresh Write

let tagm_join tagm1 tagm2 =
  LM.fold begin fun cl t1 tagm ->
    try LM.add cl (tag_join (t1, LM.find cl tagm2)) tagm with Not_found -> tagm
  end tagm1 LM.empty

let conc_join (conc1:cncm) (conc2:cncm) : cncm = 
  LM.fold begin fun al tagm1 conc ->
    if not (LM.mem al conc2) then conc else
      let tagm = tagm_join tagm1 (LM.find al conc2) in
        if tagm = LM.empty then conc else
          LM.add al tagm conc
  end conc1 LM.empty

let conc_of_predecessors = function
  | [] -> LM.empty 
  | is -> Misc.list_reduce "conc_of_predecessors" conc_join is

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

(***************************************************************************************)

let cloc_of_v theta al v =
(*  let _  = Pretty.printf "cloc_of_v theta = %a \n" d_theta theta in *)
  Misc.do_memo theta Sloc.copy_concrete al v.vname 
(*  >> Pretty.printf "cloc_of_v: v = %s cl = %a \n" v.vname Sloc.d_sloc *)


let cloc_of_position theta al (j,k,i) = 
  Printf.sprintf "#%d#%d#%d" j k i
  |> Misc.do_memo theta Sloc.copy_concrete al
(*  >> Pretty.printf "cloc_of_position: position = (%d,%d,%d) cl = %a \n" j k i Sloc.d_sloc
*)

let sloc_of_expr ctm e =
  try
  match Ctypes.I.ExpMap.find e ctm with
  | Ctypes.Ref (s, _) -> Some s
  | _                 -> None
  with Not_found -> Pretty.printf "Could not find %a\n\n" Cil.d_exp e; assert false

let loc_of_var_expr ctm theta =
  let rec loc_rec = function
    | Lval (Var v, _) as e when isPointerType v.vtype ->
        (match sloc_of_expr ctm e with 
         | None    -> None
         | Some al -> Some (cloc_of_v theta al v))
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

let tagm_of_aloc conc al =
  try LM.find al conc with Not_found -> LM.empty

let clocs_of_aloc conc al =
  al |> tagm_of_aloc conc |> Sloc.slm_bindings |>: fst

let set_rw conc al cl rw =
  LM.add al (LM.add cl (tag_fresh rw) (tagm_of_aloc conc al)) conc

let generalize conc al =
  (LM.remove al conc,
   LM.fold begin fun cl (_, op) gens ->
     match op with
       | Write -> Gen (cl, al) :: gens
       | Read  -> WGen (cl, al) :: gens
   end (tagm_of_aloc conc al) [])

let instantiate f conc al cl rw =
  match rw, clocs_of_aloc conc al with
    | _, []                           -> (set_rw conc al cl rw, [f (al, cl)])
    | Write, cls when List.mem cl cls -> (set_rw conc al cl rw, [])
    | Read, cls when List.mem cl cls  -> (conc, [])
    | _                               ->
        let conc, gens = generalize conc al in
          (set_rw conc al cl rw, gens @ [f (al, cl)])

(******************************************************************************)
(******************************** Visitors ************************************)
(******************************************************************************)

let annotate_set ctm theta conc = function
  (* v1 := *v2 *)
  | (Var v1, _), Lval (Mem e, _) ->
      let v2 = CilMisc.referenced_var_of_exp e in
      let al = sloc_of_expr ctm e |> Misc.maybe in
      let cl = cloc_of_v theta al v2 in
      instantiate (fun (x,y) -> Ins (v2.vname,x,y)) conc al cl Read


  (* v := e *)
  | (Var v, _), e ->
      e >> (CilMisc.is_pure_expr CilMisc.StringsArePure <+> (fun b -> asserts b "impure expr")) 
        |> loc_of_var_expr ctm theta
        |> Misc.maybe_iter (Hashtbl.replace theta v.vname) 
        |> fun _ -> (conc, [])

  (* *v := _ *)
  | (Mem e, _), _ ->
      let v = CilMisc.referenced_var_of_exp e in
      let al = sloc_of_expr ctm e |> Misc.maybe in
      let cl = cloc_of_v theta al v in
      instantiate (fun (x,y) -> Ins (v.vname,x,y)) conc al cl Write
  
  (* Uh, oh! *)
  | lv, e -> 
      Errormsg.error "annotate_set: lv = %a, e = %a" Cil.d_lval lv Cil.d_exp e;
      assertf "annotate_set: unknown set"

let concretize_new theta j k conc = function
  | i, New (x,y) ->
      let _  = asserts (Sloc.is_abstract y) "concretize_new" in
      if Sloc.is_abstract x then
        match clocs_of_aloc conc y with
          | [] -> (conc, [New (x, y)])
          | _  ->
              let conc, gens = generalize conc y in
                (conc, gens @ [New (x, y)])
      else  (* x is is_concrete *)
        let cl = cloc_of_position theta y (j,k,i) in
        instantiate (fun (y, cl) -> NewC (x,y,cl)) conc y cl Write
  | i, HInst s -> (conc, [HInst s])
  | i, TInst x -> (conc, [TInst x])
  | i, TNew (t, f) -> (conc, [TNew (t,f)])
  | _, _ -> assertf "concretize_new 2"

let concretize_malloc theta j k conc x y =
  let _  = asserts (Sloc.is_abstract y) "concretize_malloc" in
  let _  = asserts (x |> Sloc.is_abstract |> not) "concretize_malloc" in
  let cl = cloc_of_position theta y (j,k,0) in
  (set_rw conc y cl Write, [NewC (x,y,cl)])

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

(* kns : (k, New (al,_) list), with distinct al *)
let annotate_instr globalslocs ctm theta j (conc:cncm) = function
  | (_, ns), Cil.Call (_, Lval ((Var fv), NoOffset), _,_) 
    when CilMisc.is_pure_function fv.vname ->
      conc, ns 

  | _, Cil.Call (_, Lval ((Var fv), _), _, _) when fv.vname = "csolve_fold_all" ->
    conc |> LM.domain |> Misc.mapfold generalize conc |> Misc.app_snd List.concat

  | (k, [New (x, y)]), Cil.Call (lvo, Lval ((Var fv), NoOffset), _, _)
    when fv.vname = "malloc" ->
      let conc, anns  = concretize_malloc theta j k conc x y in
      let conc_anns   = conc, anns in
      let _           = lvo |>> sloc_of_ret ctm theta conc_anns in
      conc_anns

  | (k, ns), Cil.Call (lvo,_,_,_) ->
      let ins         = Misc.numbered_list ns in
      let conc, anns  = Misc.mapfold (concretize_new theta j k) conc ins in
      let conc, anns' = Misc.mapfold generalize conc globalslocs in
      let conc_anns   = conc, Misc.flatten (anns ++ anns') in
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

let annotate_block globalslocs ctm theta j anns instrs (conc0 : cncm) =
  let _ = if mydebug then ignore <| Pretty.printf "annotate_block  %d : CONC = %a \n" j d_conc conc0 in
  let _ = asserts (List.length anns = 1 + List.length instrs) "annotate_block: %d" j in
  let ianns = Misc.numbered_list (Misc.chop_last anns) in
  List.combine ianns instrs
  |> Misc.mapfold (annotate_instr globalslocs ctm theta j) conc0 

let annot_iter cfg globalslocs ctm theta anna (sol : soln) : soln * bool = 
(*let _    = Pretty.printf "annot_iter: PRE theta = %a \n" d_theta theta in*)
  let sol' = Array.copy sol in 
  let sol' = Misc.array_fold_lefti begin fun j sol' (_, ans) ->
      let conc = cfg.Ssa.predecessors.(j) 
                   |> Misc.map_partial (Array.get sol' <+> fst)
                   |> conc_of_predecessors in
      let conc', ans' = match cfg.Ssa.blocks.(j).Ssa.bstmt.skind with
                   | Instr is -> annotate_block globalslocs ctm theta j anna.(j) is conc
                   | _        -> conc, ans in
      let _    = Array.set sol' j (Some conc', ans') in
      sol'
     end sol' sol in
  (sol', (soln_diff (sol, sol')))

(*****************************************************************************)
(********************************** API **************************************)
(*****************************************************************************)

let d_conca' () (cfg, conca') =
  let conca  = Array.map (List.map (Array.get conca') <+> conc_of_predecessors) cfg.Ssa.predecessors in
  d_conca () (Misc.array_combine conca conca')

let check_sol cfg globalslocs ctm theta anna (sol:soln) =
  let _ = (cfg, Array.map (fst <+> Misc.maybe) sol) 
          |> Pretty.printf "check_sol PRE:\n %a \n" d_conca' in
  let sol', b = annot_iter cfg globalslocs ctm theta anna sol in
  let _ = (cfg, Array.map (fst <+> Misc.maybe) sol') 
          |> Pretty.printf "check_sol POST:\n %b : %a \n" b d_conca' in
  let sol'', b = annot_iter cfg globalslocs ctm theta anna sol' in
  let _ = (cfg, Array.map (fst <+> Misc.maybe) sol'') 
          |> Pretty.printf "check_sol POSTPOST:\n %b : %a \n" b d_conca' in
  ()

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
(*>> check_sol cfg globalslocs ctm theta anna *)
  |> annots_of_sol cfg
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
