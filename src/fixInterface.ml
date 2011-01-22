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

(************** Interface between LiquidC and Fixpoint *************)

module IM = Misc.IntMap
module F  = Format
module ST = Ssa_transform
module  C = FixConstraint

module  A = Ast
module  P = A.Predicate
module  E = A.Expression
module Sy = A.Symbol
module Su = A.Subst
module So = A.Sort
module  Q = A.Qualifier

module CI = CilInterface
module Ct = Ctypes
module It = Ct.I
module YM = Sy.SMap
module SM = Misc.StringMap
module Co = Constants
module LM = Sloc.SlocMap
module CM = CilMisc
module VM = CM.VarMap
module Ix = Ct.Index


open Misc.Ops
open Cil

let mydebug = false

(******************************************************************************)
(***************************** Tags and Locations *****************************)
(******************************************************************************)

let (fresh_tag, _ (* loc_of_tag *) ) =
  let tbl     = Hashtbl.create 17 in
  let fint, _ = Misc.mk_int_factory () in
    ((fun loc ->
        let t = fint () in
          Hashtbl.add tbl t loc;
          t),
     (fun t -> Hashtbl.find tbl t))


(*******************************************************************)
(********************* CLOC-to-ALOC Maps Names *********************)
(*******************************************************************)

type alocmap = Sloc.t -> Sloc.t option 

(* 
module AlocMap = struct
  type t = Sloc.t LM.t 
 
  (* API *)
  let id   = LM.empty
  let mem  = LM.mem
  let find = LM.find

  (* API *)
  let add cl al cf = 
    asserts (not (Sloc.is_abstract cl)) "ERROR: canonicize src abstract ptr";
    asserts (Sloc.is_abstract al) "ERROR: canonicize dst concrete ptr";
    LM.add cl al cf
end
*)
(*******************************************************************)
(******************************** Names ****************************)
(*******************************************************************)

type name = Sy.t

let string_of_name = Sy.to_string 
let name_of_varinfo = fun v -> Sy.of_string v.vname
let name_of_string  = fun s -> Sy.of_string s

let name_fresh =
  let t, _ = Misc.mk_int_factory () in
  (fun _ -> t () |> string_of_int |> (^) "lqn#" |> name_of_string)

let name_of_sloc_index l i = 
  name_of_string <| Sloc.to_string l ^ "#" ^ Ix.repr i

let is_temp_name n =
  let s = string_of_name n in
  List.exists (Misc.is_substring s) [Ix.repr_prefix; "lqn#"] 

let base_of_name n = 
  match ST.deconstruct_ssa_name (string_of_name n) with
  | None        -> None 
  | Some (b, _) -> Some (name_of_string b)
 
(*******************************************************************)
(******************(Basic) Builtin Types and Sorts *****************)
(*******************************************************************)

(* API *)
let string_of_sloc, sloc_of_string = 
  let t = Hashtbl.create 37 in
  begin fun l -> 
    l |> Sloc.to_string 
      >> (fun s -> if not (Hashtbl.mem t s) then Hashtbl.add t s l)
  end,
  begin fun s -> 
    try Hashtbl.find t s with Not_found -> 
      assertf "ERROR: unknown sloc-string"
  end

let so_ref = fun l -> So.t_ptr (So.Loc (string_of_sloc l))
let so_int = So.t_int
let so_skl = So.t_func 0 [so_int; so_int]
let so_bls = So.t_func 1 [So.t_generic 0; So.t_generic 0] 
let so_pun = So.t_func 1 [So.t_generic 0; so_int]
let so_drf = So.t_func 1 [So.t_generic 0; So.t_generic 1]

let vv_int = Sy.value_variable so_int 
let vv_bls = Sy.value_variable so_bls
let vv_skl = Sy.value_variable so_skl
let vv_pun = Sy.value_variable so_pun
let vv_drf = Sy.value_variable so_drf

let uf_bbegin  = name_of_string "BLOCK_BEGIN"
let uf_bend    = name_of_string "BLOCK_END"
let uf_skolem  = name_of_string "SKOLEM" 
let uf_uncheck = name_of_string "UNCHECKED"
let uf_deref   = name_of_string "DEREF"

(* API *)
let eApp_bbegin  = fun x -> A.eApp (uf_bbegin,  [x])
let eApp_bend    = fun x -> A.eApp (uf_bend,    [x])
let eApp_uncheck = fun x -> A.eApp (uf_uncheck, [x])
let eApp_deref   = fun x so -> A.eCst (A.eApp (uf_deref, [x]), so)
let eApp_skolem  = fun x -> A.eApp (uf_skolem, [x])

(*
let mk_pure_cfun args ret = 
  mk_refcfun [] args refstore_empty ret refstore_empty
*)

(* API *)
(* List.fold_left (fun env (n, r) -> YM.add n r env) YM.empty *)

let reft_of_top = C.make_reft (Sy.value_variable So.t_obj) So.t_obj [] 

(* API *)
let axioms      = [A.pEqual (A.zero, eApp_bbegin A.zero)]
let sorts       = [] 
let builtinm    = [(uf_bbegin,  C.make_reft vv_bls so_bls [])
                  ;(uf_bend,    C.make_reft vv_bls so_bls [])
                  ;(uf_skolem,  C.make_reft vv_skl so_skl [])
                  ;(uf_uncheck, C.make_reft vv_pun so_pun [])
                  ;(uf_deref,   C.make_reft vv_drf so_drf [])]
                  |> Sy.sm_of_list


(*******************************************************************)
(********************* Refined Types and Stores ********************)
(*******************************************************************)

let d_index_reft () (i,r) = 
  let di = Ix.d_index () i in
  let dc = Pretty.text " , " in
  let dr = Misc.fsprintf (C.print_reft None) r |> Pretty.text in
  Pretty.concat (Pretty.concat di dc) dr

module Reft = struct
  type t = Ix.t * C.reft
  let d_refinement = d_index_reft
  let lub          = fun ir1 ir2 -> assert false
  let is_subref    = fun ir1 ir2 -> assert false
  let of_const     = fun c -> assert false
  let top          = Ix.top, reft_of_top 
end

module RefCTypes = Ct.Make (Reft)
module RCt       = RefCTypes

type refctype = RCt.CType.t
type refcfun  = RCt.CFun.t
type reffield = RCt.Field.t
type refldesc = RCt.LDesc.t
type refstore = RCt.Store.t
type refspec  = RCt.Spec.t
type cilenv   = refcfun SM.t * refctype YM.t * name YM.t

let d_refstore = RCt.Store.d_store
let d_refctype = RCt.CType.d_ctype
let d_refcfun  = RCt.CFun.d_cfun

let reft_of_reft r t' =
  let vv   = C.vv_of_reft r in
  let t    = C.sort_of_reft r in
  let _    = asserti (So.ptr_of_t t  <> None) "reft_of_reft (src)" in
  let _    = asserti (So.ptr_of_t t' <> None) "reft_of_reft (dst)" in
  let vv'  = Sy.value_variable t' in
  let evv' = A.eVar vv' in
  r |> C.ras_of_reft 
    |> List.map (function C.Conc p      -> C.Conc (P.subst p vv evv') 
                        | C.Kvar (s, k) -> C.Kvar (Su.extend s (vv, evv'), k))
    |> C.make_reft vv' t'
(*    >> F.printf "reft_of_reft: r = %a, t' = %a, r' = %a \n" (C.print_reft None) r So.print t' (C.print_reft None) 
*)

let reft_of_refctype = function
  | Ct.Int (_,(_,r)) 
  | Ct.Ref (_,(_,r)) 
  | Ct.Top (_,r)     -> r

let refctype_of_reft_ctype r = function
  | Ct.Int (w,k) -> Ct.Int (w, (k, r)) 
  | Ct.Ref (l,o) -> Ct.Ref (l, (o, reft_of_reft r (so_ref l)))
  | Ct.Top (o)   -> Ct.Top (o, r) 



let ctype_of_refctype = function
  | Ct.Int (x, (y, _)) -> Ct.Int (x, y) 
  | Ct.Ref (x, (y, _)) -> Ct.Ref (x, y)
  | Ct.Top (x,_)       -> Ct.Top (x) 

let refctype_of_ctype f = function
  | Ct.Int (i, x) as t ->
      let r = C.make_reft vv_int So.t_int (f t) in
      Ct.Int (i, (x, r)) 
  | Ct.Ref (l, x) as t ->
      let so = so_ref l in
      let vv = Sy.value_variable so in
      let r  = C.make_reft vv so (f t) in
      Ct.Ref (l, (x, r)) 
  | Ct.Top (x) -> 
      Ct.Top (x, reft_of_top)

let is_base = function
  | TInt _ -> true
  | _      -> false


(* API *)
let cfun_of_refcfun   = It.CFun.map ctype_of_refctype 
(* let refcfun_of_cfun   = It.CFun.map (refctype_of_reft_ctype (Sy.value_variable So.t_int, So.t_int, [])) *)
let refcfun_of_cfun   = It.CFun.map (refctype_of_ctype (fun _ -> []))


let cspec_of_refspec  = It.Spec.map (fun (i,_) -> i)
let store_of_refstore = It.Store.map_ct ctype_of_refctype
let qlocs_of_refcfun  = fun ft -> ft.Ct.qlocs
let args_of_refcfun   = fun ft -> ft.Ct.args
let ret_of_refcfun    = fun ft -> ft.Ct.ret
let stores_of_refcfun = fun ft -> (ft.Ct.sto_in, ft.Ct.sto_out)
let mk_refcfun qslocs args ist ret ost = 
  { Ct.qlocs   = qslocs; 
    Ct.args    = args;
    Ct.ret     = ret;
    Ct.sto_in  = ist;
    Ct.sto_out = ost; }




(* API *)
let pred_of_refctype s v cr = 
  let n        = name_of_varinfo v in
  let vv,_,ras = cr |> reft_of_refctype |> C.apply_solution s in
  let su       = Su.of_list [(vv, A.eVar n)] in
  ras |> Misc.flap (function C.Conc (A.And ps, _) -> ps | _ -> []) 
      |> A.pAnd
      |> Misc.flip A.substs_pred su

(*******************************************************************)
(******************** Operations on Refined Stores *****************)
(*******************************************************************)

let refstore_fold      = LM.fold
let refstore_partition = fun f -> RCt.Store.partition (fun l _ -> f l) 
let refstore_empty     = LM.empty
let refstore_mem       = fun l sto -> LM.mem l sto
let refstore_remove    = fun l sto -> LM.remove l sto

let refstore_set sto l rd =
  try LM.add l rd sto with Not_found -> 
    assertf "refstore_set"

let refstore_get sto l =
  try LM.find l sto with Not_found ->
    (Errormsg.error "Cannot find location %a in store\n" Sloc.d_sloc l;   
     asserti false "refstore_get"; assert false)

let sloc_binds_of_refldesc l rd =
  RCt.LDesc.foldn begin fun _ binds i rfld ->
    ((name_of_sloc_index l i, RCt.Field.type_of rfld), i)::binds
  end [] rd
  |> List.rev

let binds_of_refldesc l rd = 
  sloc_binds_of_refldesc l rd 
  |> List.filter (fun (_, i) -> not (Ix.is_periodic i))
  |> List.map fst

let refldesc_subs rd f =
  RCt.LDesc.mapn (fun i pl fld -> RCt.Field.map_type (f i pl) fld) rd

let refdesc_find i rd = 
  match RCt.LDesc.find i rd with
  | [(i', rfld)] -> (RCt.Field.type_of rfld, Ix.is_periodic i')
  | _            -> assertf "refdesc_find"

let addr_of_refctype loc = function
  | Ct.Ref (cl, (i,_)) when not (Sloc.is_abstract cl) ->
      (cl, i)
  | cr ->
      let s = cr  |> d_refctype () |> Pretty.sprint ~width:80 in
      let l = loc |> d_loc () |> Pretty.sprint ~width:80 in
      let _ = asserti false "addr_of_refctype: bad arg %s at %s \n" s l in
      assert false

let ac_refstore_read loc sto cr = 
  let (l, ix) = addr_of_refctype loc cr in 
  LM.find l sto 
  |> refdesc_find ix

(* API *)
let refstore_read loc sto cr = 
  ac_refstore_read loc sto cr |> fst

(* API *)
let is_poly_cloc st cl =
  let _ = asserts (not (Sloc.is_abstract cl)) "is_poly_cloc" in
  refstore_get st cl 
  |> binds_of_refldesc cl 
  |> (=) []

(* API *)
let is_soft_ptr loc sto cr = 
  ac_refstore_read loc sto cr |> snd

let refstore_write loc sto rct rct' = 
  let (cl, ix) = addr_of_refctype loc rct in
  let _  = assert (not (Sloc.is_abstract cl)) in
  let ld = LM.find cl sto in
  let ld = RCt.LDesc.remove ix ld in
  let ld = RCt.LDesc.add loc ix (RCt.Field.create Ct.Nonfinal rct') ld in
  LM.add cl ld sto

(*******************************************************************)
(****************** Tag/Annotation Generation **********************)
(*******************************************************************)

type binding = TVar of string * refctype
             | TFun of string * refcfun
             | TSto of string * refstore

let report_bad_binding = function 
  | TVar (x, cr) ->
      Errormsg.warn "\nBad TVar for %s :: \n\n@[%a@]" x d_refctype cr
  | TFun (fn, cf) ->
      Errormsg.warn "\nBad TFun for %s ::\n\n@[%a@]" fn d_refcfun cf
  | TSto (fn, st) -> 
      Errormsg.error "\nBad TSto for %s ::\n\n@[%a@]" fn d_refstore st 

let tags_of_binds s binds = 
  let s_typ = RCt.CType.map (Misc.app_snd (C.apply_solution s)) in
  let s_fun = RCt.CFun.map s_typ in
  let s_sto = RCt.Store.map_ct s_typ in
  let nl    = Constants.annotsep_name in
  List.fold_left begin fun (d, kts) bind -> 
    try
      match bind with 
      | TVar (x, cr) ->
          let k,t  = x, ("variable "^x) in
          let d'   = Pretty.dprintf "%s ::\n\n@[%a@] %s" t d_refctype (s_typ cr) nl in
          (Pretty.concat d d', (k,t)::kts)
      | TFun (f, cf) -> 
          let k,t  = f, ("function "^f) in
          let d'   = Pretty.dprintf "%s ::\n\n@[%a@] %s" t d_refcfun (s_fun cf) nl in
          (Pretty.concat d d', (k,t)::kts)
      | TSto (f, st) -> 
        let kts' =  RCt.Store.domain st 
                 |> List.map (Pretty.sprint ~width:80 <.> Sloc.d_sloc ())
                 |> List.map (fun s -> (s, s^" |->")) in
        let d'   = Pretty.dprintf "funstore %s ::\n\n@[%a@] %s" f d_refstore (s_sto st) nl in
        (Pretty.concat d d', kts' ++ kts)
    with
      C.UnmappedKvar _ -> (if mydebug then report_bad_binding bind); (d, kts)
  end (Pretty.nil, []) binds

let generate_annots d = 
  let fn = !Co.liquidc_file_prefix ^ ".annot" in
  let oc = open_out fn in
  let _  = Pretty.fprint ~width:80 oc d in
  let _  = close_out oc in
  ()

let generate_tags kts =
  let fn = !Co.liquidc_file_prefix ^ ".tags" in
  let oc = open_out fn in
  let _  = kts |> List.sort (fun (k1,_) (k2,_) -> compare k1 k2) 
               |> List.iter (fun (k,t) -> ignore <| Pretty.fprintf oc "%s\t%s.annot\t/%s/\n" k !Co.liquidc_file_prefix t) in
  let _  = close_out oc in
  ()

let annotr    = ref [] 

(* API *)
let annot_var   = fun x cr  -> annotr := TVar (string_of_name x, cr) :: !annotr
let annot_fun   = fun f cf  -> annotr := TFun (f, cf) :: !annotr
let annot_sto   = fun f st  -> annotr := TSto (f, st) :: !annotr
let annot_clear = fun _     -> annotr := []
let annot_dump  = fun s     -> !annotr
                               |> tags_of_binds s 
                               >> (fst <+> generate_annots)
                               >> (snd <+> generate_tags) 
                               |> ignore

(*******************************************************************)
(************************** Environments ***************************)
(*******************************************************************)


let ce_rem   = fun n cenv       -> Misc.app_snd3 (YM.remove n) cenv
let ce_mem   = fun n (_, vnv,_) -> YM.mem n vnv

let ce_find n (_, vnv, _) =
  try YM.find n vnv with Not_found -> 
    let _  = asserti false "Unknown name! %s" (Sy.to_string n) in
    assertf "Unknown name! %s" (Sy.to_string n)

let ce_find_fn s (fnv, _,_) =
  try SM.find s fnv with Not_found ->
    assertf "FixInterface.ce_find: Unknown function! %s" s

let ce_adds cenv ncrs =
  let _ = if mydebug then (List.iter (fun (n, cr) -> Errormsg.log "ce_adds: n = %s cr = %a \n"
  (Sy.to_string n) d_refctype cr) ncrs) in
  let _ = List.iter (fun (n, cr) -> annot_var n cr) ncrs in
  List.fold_left begin fun (fnv, env, livem) (n, cr) ->
    let env'   = YM.add n cr env in
    let livem' = match base_of_name n with 
                 | None -> livem 
                 | Some bn -> YM.add bn n livem in
    fnv, env', livem'
  end cenv ncrs

let ce_adds_fn (fnv, vnv, livem) sfrs = 
  let _ = List.iter (Misc.uncurry annot_fun) sfrs in
  (List.fold_left (fun fnv (s, fr) -> SM.add s fr fnv) fnv sfrs, vnv, livem)

let ce_mem_fn = fun s (fnv, _, _) -> SM.mem s fnv

let ce_empty = (SM.empty, YM.empty, YM.empty) 

let d_cilenv () (fnv,_,_) = failwith "TBD: d_cilenv"


let print_rctype so ppf rct =
  rct |> reft_of_refctype |> C.print_reft so ppf 

let print_binding so ppf (n, rct) = 
  F.fprintf ppf "%a : %a" Sy.print n (print_rctype so) rct

let print_ce so ppf (_, vnv) =
  YM.iter begin fun n cr -> 
    F.fprintf ppf "@[%a@]@\n" (print_binding so) (n, cr) 
  end vnv

(****************************************************************)
(************************** Refinements *************************)
(****************************************************************)

let sort_of_prectype = function
  | Ct.Ref (l,_) -> so_ref l
  | _            -> so_int

let ra_fresh        = fun _ -> [C.Kvar (Su.empty, C.fresh_kvar ())] 
let ra_true         = fun _ -> []

let ra_zero ct = 
  let vv = ct |> sort_of_prectype |> Sy.value_variable in
  [C.Conc (A.pAtom (A.eVar vv, A.Eq, A.zero))]

let ra_equal v ct =
  let vv = ct |> sort_of_prectype |> Sy.value_variable in
  [C.Conc (A.pAtom (A.eVar vv, A.Eq, A.eVar v))]

let ra_deref ct base offset =
  let so  = sort_of_prectype ct in
  let vv  = so |> Sy.value_variable in
  let ptr = A.eBin (base, A.Plus, A.eCon (A.Constant.Int offset)) in
    [C.Conc (A.pAtom (A.eVar vv, A.Eq, eApp_deref ptr so))]

let ra_skolem, get_skolems =
  let xr = ref 0 in
  (fun ct ->
    let vv = ct |> sort_of_prectype |> Sy.value_variable in
    [C.Conc (A.pEqual (A.eVar vv, eApp_skolem (A.eInt (xr =+ 1))))]),
  (fun _ -> Misc.range 0 !xr |>: A.eInt) 

let ra_bbegin ct =
  ct 
  |> sort_of_prectype 
  |> Sy.value_variable 
  |> A.eVar 
  |> (fun vv -> [C.Conc (A.pEqual (vv, eApp_bbegin vv))])

let t_fresh         = fun ct -> refctype_of_ctype ra_fresh ct 
let t_true          = fun ct -> refctype_of_ctype ra_true ct
let t_equal         = fun ct v -> refctype_of_ctype (ra_equal v) ct
let t_skolem        = fun ct -> refctype_of_ctype ra_skolem ct 

let t_conv_refctype = fun f rct -> rct |> ctype_of_refctype |> refctype_of_ctype f
let t_true_refctype = t_conv_refctype ra_true
let t_zero_refctype = t_conv_refctype ra_zero


(* convert {v : ct | p } into refctype *)
let t_pred ct v p = 
  let so = sort_of_prectype ct in
  let vv = Sy.value_variable so in
  let p  = P.subst p v (A.eVar vv) in
  let r  = C.make_reft vv so [C.Conc p] in
  refctype_of_reft_ctype r ct


let t_size_ptr ct size =
  let so  = sort_of_prectype ct in
  let vv  = Sy.value_variable so in
  let evv = A.eVar vv in
  t_pred ct vv
    (A.pAnd [A.pAtom (evv, A.Gt, A.zero);
             A.pAtom (eApp_bbegin evv, A.Eq, evv);
             A.pAtom (eApp_bend evv, A.Eq, A.eBin (evv, A.Plus, A.eCon (A.Constant.Int size)))])

let t_valid_ptr ct =
  let so  = sort_of_prectype ct in
  let vv  = Sy.value_variable so in
  let evv = A.eVar vv in
  t_pred ct vv (A.pOr [A.pAtom (eApp_uncheck evv, A.Eq, A.one);
                       A.pAnd [A.pAtom (evv, A.Ne, A.zero);
                               A.pAtom (eApp_bbegin evv, A.Le, evv);
                               A.pAtom (evv, A.Lt, eApp_bend evv)]])

let is_reference cenv x =
  if YM.mem x builtinm (* List.mem_assoc x builtins *) then (* TBD: REMOVE GROSS HACK *)
    false
  else if not (ce_mem x cenv) then
    false
  else match ce_find x cenv with 
    | Ct.Ref (_,(_,_)) -> true
    | _                -> false

let mk_eq_uf = fun f x y -> A.pAtom (f x, A.Eq, f y)

let t_exp_ptr cenv e ct vv so p = (* TBD: REMOVE UNSOUND AND SHADY HACK *)
  let refs = P.support p |> List.filter (is_reference cenv) in
  match ct, refs with
  | (Ct.Ref (_,_)), [x]  ->
      let x         = A.eVar x  in
      let vv        = A.eVar vv in
      let unchecked =
        if e |> typeOf |> CM.is_unchecked_ptr_type then
          C.Conc (A.pAtom (eApp_uncheck vv, A.Eq, A.one))
        else
          C.Conc (mk_eq_uf eApp_uncheck vv x)
      in [C.Conc (mk_eq_uf eApp_bbegin  vv x);
          C.Conc (mk_eq_uf eApp_bend    vv x);
          unchecked]
  | _ ->
      []


let t_exp cenv ct e =
  let so = sort_of_prectype ct in
  let vv = Sy.value_variable so in
  let p  = CI.reft_of_cilexp vv e in
(* let _  = Errormsg.log "\n reft_of_cilexp [e: %a] [p: %s] \n" Cil.d_exp e (P.to_string p) in *)
  let rs = [C.Conc p] ++ (t_exp_ptr cenv e ct vv so p) in
  let r  = C.make_reft vv so rs in
  refctype_of_reft_ctype r ct

let ptrs_of_exp e = 
  let xm = ref VM.empty in
  let _  = CM.iterExprVars e (fun v -> xm := VM.add v () !xm) in
  !xm |> CM.vm_to_list |>: fst |> (List.filter (fun v -> Cil.isPointerType v.Cil.vtype))

let t_exp_scalar_ptr vv e = (* TODO: REMOVE UNSOUND AND SHADY HACK *)
  e |> ptrs_of_exp 
    |> (function [v] -> [C.Conc (mk_eq_uf eApp_bbegin (A.eVar vv) (A.eVar (name_of_varinfo v)))] | _ -> [])

let t_exp_scalar v e =
  let ct = Ct.scalar_ctype in
  let so = sort_of_prectype ct in
  let vv = Sy.value_variable so in
  let p  = CI.reft_of_cilexp vv e in
  let rs = [C.Conc p] in
  let rs = if Cil.isPointerType v.Cil.vtype then (rs ++ t_exp_scalar_ptr vv e) else rs in
  let r  = C.make_reft vv so rs in
  refctype_of_reft_ctype r ct


let pred_of_index = function
  | Ix.IBot                     -> vv_int, A.pFalse
  | Ix.IInt n                   -> vv_int, A.pEqual (A.eVar vv_int, A.eInt n)
  | Ix.ICClass {Ix.lb = Some n} -> vv_int, A.pAtom (A.eVar vv_int, A.Ge, A.eInt n)
  | _                           -> vv_int, A.pTrue 

let t_name (_,vnv,_) n = 
  let _  = asserti (YM.mem n vnv) "t_name: reading unbound var %s" (string_of_name n) in
  let rct = YM.find n vnv in
  let so = rct |> reft_of_refctype |> C.sort_of_reft in
  let vv = Sy.value_variable so in
  let r  = C.make_reft vv so [C.Conc (A.pAtom (A.eVar vv, A.Eq, A.eVar n))] in
  rct |> ctype_of_refctype |> refctype_of_reft_ctype r

(* API *)
let map_fn = RCt.CFun.map 

(* let t_fresh_fn = It.CFun.map t_fresh  *)

let t_ctype_refctype ct rct =
  rct 
  |> reft_of_refctype
  |> Misc.flip refctype_of_reft_ctype ct 

let strengthen_refctype mkreft rct =
  let reft = reft_of_refctype rct in
  let vv   = C.vv_of_reft reft in
  let so   = C.sort_of_reft reft in
  let ras  = C.ras_of_reft reft in
  refctype_of_reft_ctype (C.make_reft vv so (mkreft rct @ ras)) (ctype_of_refctype rct)

let refctype_subs f nzs = 
  nzs |> Misc.map (Misc.app_snd f) 
      |> Su.of_list
      |> C.theta
      |> Misc.app_snd
      |> RCt.CType.map

(* API *)
let t_subs_exps    = refctype_subs (CI.expr_of_cilexp (* skolem *))
let t_subs_names   = refctype_subs A.eVar
let refstore_fresh = fun f st -> st |> RCt.Store.map_ct t_fresh >> annot_sto f 
let refstore_subs  = fun f subs st -> RCt.Store.map_ct (f subs) st

let t_scalar_index = pred_of_index <+> Misc.uncurry (t_pred Ct.scalar_ctype)

let t_scalar_zero  = refctype_of_ctype ra_bbegin Ct.scalar_ctype

let t_scalar = function
  | Ct.Ref (_,Ix.IInt 0) -> t_scalar_zero 
  | Ct.Int (_,ix)        -> t_scalar_index ix 
  | _                    -> t_true Ct.scalar_ctype

let deconstruct_refctype rct = 
  let r = reft_of_refctype rct in
  (ctype_of_refctype rct, C.vv_of_reft r, C.sort_of_reft r, C.ras_of_reft r)

let meet_refctype rct1 rct2 = 
  let (ct1, vv1, so1, ra1) = deconstruct_refctype rct1 in
  let (ct2, vv2, so2, ra2) = deconstruct_refctype rct2 in
  if not (ct1 = ct2 && vv1 = vv2 && so1 = so2) then begin
    Pretty.printf "ct1 = %a, ct2 = %a" Ctypes.d_ctype ct1 Ctypes.d_ctype ct2;
    Pretty.printf "vv1 = %s, vv2 = %s" (Sy.to_string vv1) (Sy.to_string vv2);
    assertf "meet_refctype"  
  end;
  refctype_of_reft_ctype (C.make_reft vv1 so1 (ra1 ++ ra2)) ct1 

let vv_rename so' r =
  let vv'    = Sy.value_variable so' in
  let vv, so = r |> (C.vv_of_reft <*> C.sort_of_reft) in
  let ras'   = r |> C.theta (Su.of_list [vv, A.eVar vv']) |> C.ras_of_reft in
  C.make_reft vv' so' ras'

let t_scalar_refctype_raw rct = 
  let so'  = Ct.scalar_ctype |> sort_of_prectype in 
  let r'   = rct |> reft_of_refctype |> vv_rename so' in 
  refctype_of_reft_ctype r' Ct.scalar_ctype 

(* API *)
let t_scalar_refctype rct =
  rct 
  |> (t_scalar_refctype_raw <*> (t_scalar <.> ctype_of_refctype)) 
  |> Misc.uncurry meet_refctype

(* WRAPPER *)
let t_scalar_refctype x =
  x |> t_scalar_refctype
    >> (fun y -> ignore <| Pretty.printf "t_scalar_refctype: [in=%a] [out=%a] \n" d_refctype x d_refctype y)


(* API *)
let t_subs_locs lsubs rct =
  rct |> ctype_of_refctype
      |> It.CType.subs lsubs
      |> refctype_of_reft_ctype (reft_of_refctype rct)

let subs_of_lsubs lsubs sto = 
  Misc.tr_rev_flap begin fun (l, l') -> 
    if not (refstore_mem l sto) then [] else
      let is  = l |> refstore_get sto |> RCt.LDesc.indices in
      let ns  = List.map (name_of_sloc_index l)  is in
      let ns' = List.map (name_of_sloc_index l') is in
      List.combine ns ns'
  end lsubs

let refstore_subs_locs lsubs sto =
  let subs = subs_of_lsubs lsubs sto in
  RCt.Store.map_ct ((t_subs_locs lsubs) <+> (t_subs_names subs)) sto

  
(*
let refstore_subs_locs lsubs sto = 
  List.fold_left begin fun sto (l, l') -> 
    let rv = 
    if not (refstore_mem l sto) then sto else
      let plocs = l |> refstore_get sto |> plocs_of_refldesc in
      let ns    = List.map (name_of_sloc_ploc l) plocs in
      let ns'   = List.map (name_of_sloc_ploc l') plocs in
      let subs  = List.combine ns ns' in
      Ct.prestore_map_ct (t_subs_names subs) sto
    in
    (* let _ = Pretty.printf "refstore_subs_locs: l = %a, l' = %a \n sto = %a \n sto' = %a \n"
            Sloc.d_sloc l Sloc.d_sloc l' d_refstore sto d_refstore rv in *)
    rv
  end sto lsubs

let refstore_subs_locs lsubs st = 
  st |> Ct.prestore_subs lsubs
     |> Ct.prestore_map_ct (t_subs_locs lsubs) 

*)

exception ContainsDeref

let check_expr_is_deref e =
  match A.Expression.unwrap e with
    | A.App (f, _) when f = uf_deref -> raise ContainsDeref
    | _                              -> ()

let may_contain_deref rct =
  match rct |> reft_of_refctype |> C.preds_kvars_of_reft with
    | _, _ :: _ -> true
    | ps, _     ->
        try
          List.iter (P.iter (fun _ -> ()) check_expr_is_deref) ps;
          false
        with ContainsDeref ->
          true

(**************************************************************)
(*******************Constraint Simplification *****************)
(**************************************************************)

let is_var_def = function
  | C.Conc (A.Atom ((A.Var x, _), A.Eq, (A.Var y, _)), _) 
    when Sy.is_value_variable x -> Some y
  | C.Conc (A.Atom ((A.Var x, _), A.Eq, (A.Var y, _)), _) 
    when Sy.is_value_variable y -> Some x
  | _                           -> None

let str_reft env r = 
  Misc.expand begin fun (_, t, ras) ->
    ras |> Misc.map_partial is_var_def
        |> List.filter (fun x -> YM.mem x env)
        |> List.map (fun x -> YM.find x env)
        |> (fun rs -> rs, ras)
 end [r] []

let is_temp_equality ra = 
  match is_var_def ra with 
  | Some x -> is_temp_name x
  | _      -> false 

let strengthen_reft env ((v, t, ras) as r) =
  r |> str_reft env 
    |> List.filter (not <.> is_temp_equality) 
(*  |> List.filter (fun ra -> match is_var_def ra with None -> true | _ -> false) *)
    |> Misc.sort_and_compact
    |> (fun ras' -> v, t, ras')

(*******************************************************************)
(********************** Pointer Canonicization *********************)
(*******************************************************************)

let canon_loc cf l = 
  match cf l with 
  | Some al -> al 
  | None    -> l
  (* if AlocMap.mem l cf then AlocMap.find l cf else l *)

let canon_sort cf t = 
  (match So.ptr_of_t t with
   | Some (So.Loc s) -> s |> sloc_of_string |> canon_loc cf |> so_ref 
   | _               -> t)
(*  >> (fun t' -> Format.printf "canon_sort: t = %a, t' = %a \n" So.print t So.print t') *)

let canon_reft cf r = 
  let t  = C.sort_of_reft r in
  let t' = canon_sort cf t in
  if t = t' then r else reft_of_reft r t'

let canon_env cf env = 
  YM.map (canon_reft cf) env

(******************************************************************************)
(********************** WF For Dereferencing Expressions **********************)
(******************************************************************************)

let find_unfolded_loc cf l sto =
  try
    LM.find l sto
  with Not_found ->
    LM.find (canon_loc cf l) sto

let points_to_final cf cenv sto p o =
  match ce_find p cenv with
    | Ct.Ref (l, (Ix.IInt n, _)) ->
           sto
        |> find_unfolded_loc cf l
        |> RCt.LDesc.find (Ix.IInt (n + o))
        |> List.for_all (fun (_, fld) -> RCt.Field.is_final fld)
    | _ -> false

let expr_derefs_wf cf cenv sto e =
  match E.unwrap e with
    | A.App (f, [e]) when f = uf_deref ->
        begin match E.unwrap e with
          | A.Var p                -> points_to_final cf cenv sto p 0
          | A.Bin (e1, A.Plus, e2) ->
              begin match E.unwrap e1, E.unwrap e2 with
                | A.Var p, A.Con (A.Constant.Int n) -> points_to_final cf cenv sto p n
                | _                                 -> assert false
              end
          | A.Bin (e1, A.Minus, e2) ->
              begin match E.unwrap e1, E.unwrap e2 with
                | A.Var p, A.Con (A.Constant.Int n) -> points_to_final cf cenv sto p (-n)
                | _                                 -> assert false
              end
          | _ -> assert false
        end
    | _ -> true

let filter_store_derefs cf cenv sto rct q =
  let cenv = ce_adds cenv [(Q.vv_of_t q, rct)] in
  let wf   = ref true in
       q
    |> Q.pred_of_t
    |> P.iter (fun _ -> ()) (fun e -> wf := !wf && expr_derefs_wf cf cenv sto e);
    !wf

(****************************************************************)
(********************** Constraints *****************************)
(****************************************************************)

let is_live_name livem n =
  match base_of_name n with
  | None    -> true
  | Some bn -> if YM.mem bn livem then n = YM.find bn livem else true

let env_of_cilenv cf (_, vnv, _) = 
  builtinm
  |> YM.fold (fun n rct env -> YM.add n (reft_of_refctype rct) env) vnv 
  |> canon_env cf

let make_wfs cf ((_,_,livem) as cenv) sto rct _ =
  let r   = rct |> reft_of_refctype |> canon_reft cf in
  let env = cenv 
            |> env_of_cilenv cf 
            |> Sy.sm_filter (fun n _ -> n |> Sy.to_string |> Co.is_cil_tempvar |> not)
            |> (if !Co.prune_live then Sy.sm_filter (fun n _ -> is_live_name livem n) else id)
  in [C.make_filtered_wf env r None (filter_store_derefs cf cenv sto rct)]
(* >> F.printf "\n make_wfs: \n @[%a@]" (Misc.pprint_many true "\n" (C.print_wf None)) 
*)

let make_wfs_refstore cf env full_sto sto tag =
  LM.fold begin fun l rd ws ->
    let ncrs = sloc_binds_of_refldesc l rd in
    let env' = ncrs |> List.filter (fun (_,i) -> not (Ix.is_periodic i)) 
                    |> List.map fst
                    |> ce_adds env in 
    let ws'  = Misc.flap (fun ((_,cr),_) -> make_wfs cf env' full_sto cr tag) ncrs in
    ws' ++ ws
  end sto []
(* >> F.printf "\n make_wfs_refstore: \n @[%a@]" (Misc.pprint_many true "\n" (C.print_wf None))  *)


let make_wfs_fn cf cenv rft tag =
  let args  = List.map (Misc.app_fst Sy.of_string) rft.Ct.args in
  let env'  = ce_adds cenv args in
  let retws = make_wfs cf env' rft.Ct.sto_out rft.Ct.ret tag in
  let argws = Misc.flap (fun (_, rct) -> make_wfs cf env' rft.Ct.sto_in rct tag) args in
  let inws  = make_wfs_refstore cf env' rft.Ct.sto_in rft.Ct.sto_in tag in
  let outws = make_wfs_refstore cf env' rft.Ct.sto_out rft.Ct.sto_out tag in
  Misc.tr_rev_flatten [retws ; argws ; inws ; outws]

let make_dep pol xo yo =
  (xo, yo) |> Misc.map_pair (Misc.maybe_map CilTag.tag_of_t)
           |> Misc.uncurry (C.make_dep pol)

(*
let add_deps tago tag = 
  match tago with 
  | Some t -> [make_dep true (Some t) (Some tag)] 
  | _      -> [] 
*)

let make_cs cf cenv p rct1 rct2 tago tag =
  let env    = cenv |> env_of_cilenv cf in
  let r1, r2 = Misc.map_pair (reft_of_refctype <+> canon_reft cf) (rct1, rct2) in
  let r1     = if !Co.simplify_t then strengthen_reft env r1 else r1 in
  let cs     = [C.make_t env p r1 r2 None (CilTag.tag_of_t tag)] in
  let ds     = [] (* add_deps tago tag *) in
  cs, ds

let make_cs_validptr cf cenv p rct tago tag =
  let rvp = rct |> ctype_of_refctype |> t_valid_ptr in
  make_cs cf cenv p rct rvp tago tag


let make_cs_refldesc cf env p (sloc1, rd1) (sloc2, rd2) tago tag =
  let ncrs1  = sloc_binds_of_refldesc sloc1 rd1 in
  let ncrs2  = sloc_binds_of_refldesc sloc2 rd2 in
  let ncrs12 = Misc.join snd ncrs1 ncrs2 |> List.map (fun ((x,_), (y,_)) -> (x,y)) in  
(*  let _      = asserts ((* TBD: HACK for malloc polymorphism *) ncrs1 = [] 
                       || List.length ncrs12 = List.length ncrs2) "make_cs_refldesc" in *)
  let env'   = List.map fst ncrs1 |> ce_adds env in
  let subs   = List.map (fun ((n1,_), (n2,_)) -> (n2, n1)) ncrs12 in
  Misc.map begin fun ((n1, _), (_, cr2)) -> 
      let lhs = t_name env' n1 in
      let rhs = t_subs_names subs cr2 in
      make_cs cf env' p lhs rhs tago tag 
  end ncrs12
  |> Misc.splitflatten

let make_cs_refstore cf env p st1 st2 polarity tago tag loc =
(*  let _  = Pretty.printf "make_cs_refstore: pol = %b, st1 = %a, st2 = %a, loc = %a \n"
           polarity Ct.d_prestore_addrs st1 Ct.d_prestore_addrs st2 Cil.d_loc loc in
  let _  = Pretty.printf "st1 = %a \n" d_refstore st1 in
  let _  = Pretty.printf "st2 = %a \n" d_refstore st2 in  
*)
  (if polarity then st2 else st1)
  |> RCt.Store.domain
  |> Misc.map begin fun sloc ->
       let lhs = (sloc, refstore_get st1 sloc) in
       let rhs = (sloc, refstore_get st2 sloc) in
       make_cs_refldesc cf env p lhs rhs tago tag 
     end 
  |> Misc.splitflatten 
(* >> (fun (cs,_) -> F.printf "make_cs_refstore: %a" (Misc.pprint_many true "\n" (C.print_t None)) cs) 
*)

(* API *)
let make_cs_refstore cf env p st1 st2 polarity tago tag loc =
  try make_cs_refstore cf env p st1 st2 polarity tago tag loc with ex ->
    let _ = Cil.errorLoc loc "make_cs_refstore fails with: %s" (Printexc.to_string ex) in
    let _ = asserti false "make_cs_refstore" in 
    assert false

(* API *)
let make_cs cf cenv p rct1 rct2 tago tag loc =
(*  let _ = Pretty.printf "make_cs: rct1 = %a, rct2 = %a \n" d_refctype rct1 d_refctype rct2 in
 *) try make_cs cf cenv p rct1 rct2 tago tag with ex ->
    let _ = Cil.errorLoc loc "make_cs fails with: %s" (Printexc.to_string ex) in
    let _ = asserti false "make_cs" in 
    assert false

(* API *)
let make_cs_validptr cf cenv p rct tago tag loc =
  try make_cs_validptr cf cenv p rct tago tag with ex ->
    let _ = Cil.errorLoc loc "make_cs_validptr fails with: %s" (Printexc.to_string ex) in
    let _ = asserti false "make_cs_validptr" in
    assert false

(* API *)
let make_cs_refldesc cf env p (sloc1, rd1) (sloc2, rd2) tago tag loc =
  try make_cs_refldesc cf env p (sloc1, rd1) (sloc2, rd2) tago tag with ex ->
    let _ = Cil.errorLoc loc "make_cs_refldesc fails with: %s" (Printexc.to_string ex) in 
    let _ = asserti false "make_cs_refldesc" in 
    assert false

let new_block_reftype = t_zero_refctype (* t_true_refctype *)


(* API: TBD: UGLY *)
let extend_world cf ssto sloc cloc newloc strengthen loc tag (env, sto, tago) =
  let ld    = sloc |> refstore_get ssto |> strengthen in
  let binds = binds_of_refldesc sloc ld 
              |> (Misc.choose newloc (List.map (Misc.app_snd new_block_reftype)) id) in 
  let subs  = List.map (fun (n,_) -> (n, name_fresh ())) binds in
  let env'  = Misc.map2 (fun (_, cr) (_, n') -> (n', cr)) binds subs
              |> Misc.map (Misc.app_snd (t_subs_names subs))
              |> ce_adds env in
  let _, im = Misc.fold_lefti (fun i im (_,n') -> IM.add i n' im) IM.empty subs in
  let ld'   = RCt.LDesc.mapn begin fun i ix rfld ->
                let fnl = RCt.Field.get_finality rfld in
                  if IM.mem i im then IM.find i im |> t_name env' |> RCt.Field.create fnl else
                    match ix with
                      | Ix.IInt _ -> assertf "missing binding!"
                      | _         -> RCt.Field.map_type (t_subs_names subs) rfld
              end ld in
  let cs    = if not newloc then [] else
                RCt.LDesc.foldn begin fun i cs ix rfld ->
                  match ix with
                  | Ix.ICClass _ ->
                      let rct = RCt.Field.type_of rfld in
                      let lhs = new_block_reftype rct in
                      let rhs = t_subs_names subs rct in
                      let cs' = fst <| make_cs cf env' A.pTrue lhs rhs None tag loc in
                      cs' ++ cs
                  | _ -> cs
                end [] ld in
  let sto'  = refstore_set sto cloc ld' in
  (env', sto', tago), cs

let strengthen_final_field ffs ptrname i fld =
  let ptr_base = ptrname |> Sy.of_string |> A.eVar |> eApp_bbegin in
    match i with
      | Ix.ICClass _ | Ix.IBot -> fld
      | Ix.IInt n              ->
          if Ct.IndexSet.mem i ffs then
                fld
            |> RCt.Field.map_type (strengthen_refctype (fun ct -> ra_deref ct ptr_base n))
            |> RCt.Field.set_finality Ct.Final
          else
            fld

let finalized_name = "FINAL" |> Misc.mk_string_factory |> fst

let refstore_strengthen_addr loc env sto ffm ptrname addr =
  let cl, i = addr_of_refctype loc addr in
  let _     = assert (not (Sloc.is_abstract cl)) in
  let ffs   = LM.find cl ffm in
    if Ct.IndexSet.mem i ffs then
      let ld  = LM.find cl sto in
      let fld = ld |> RCt.LDesc.find i |> List.hd |> snd in
      let ld  = RCt.LDesc.remove i ld in
      let sct = fld |> strengthen_final_field ffs ptrname i |> RCt.Field.type_of in
      let fn  = () |> finalized_name |> Sy.of_string in
      let env = ce_adds env [(fn, sct)] in
      let fld = t_equal (ctype_of_refctype sct) fn |> RCt.Field.create Ct.Final in
      let ld  = RCt.LDesc.add loc i fld ld in
        (env, LM.add cl ld sto)
    else
      (env, sto)

(* API *)
let quals_of_file fname =
  try
    let _ = Errorline.startFile fname in
      fname
      |> open_in 
      |> Lexing.from_channel
      |> FixParse.defs FixLex.token
      |> Misc.map_partial (function C.Qul p -> Some p | _ -> None) 
      >> Co.bprintf mydebug "Read Qualifiers: \n%a" (Misc.pprint_many true "" Q.print) 
  with Sys_error s ->
    Errormsg.warn "Error reading qualifiers: %s@!@!Continuing without qualifiers...@!@!" s; []

(* API: shady hack -- remove when "Solve.force" properly implemented 
let annot_binds () = 
  let cf = fun _ -> None in
  !annotr
  |> Misc.map_partial begin function 
       | TVar (n, env, cr) -> Some (name_of_string n, (env_of_cilenv cf env, reft_of_refctype cr))
       | _                 -> None
     end 
  |> Ast.Symbol.sm_of_list

  *)


