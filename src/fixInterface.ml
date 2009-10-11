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



module F   = Format
module ST  = Ssa_transform
module  A  = Ast
module  P  = Ast.Predicate
module  C  = FixConstraint
module Sy  = Ast.Symbol
module So  = Ast.Sort
module CI  = CilInterface

module YM  = Sy.SMap
module SM  = Misc.StringMap
module Co  = Constants
module SLM = Sloc.SlocMap

open Misc.Ops
open Cil

(*******************************************************************)
(************** Interface between LiquidC and Fixpoint *************)
(*******************************************************************)

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

let name_of_sloc_ploc l p = 
  let ls    = Sloc.to_string l in
  let pt,pi = match p with 
              | Ctypes.PLAt i -> "PLAt",i 
              | Ctypes.PLSeq i -> "PLSeq", i 
              | Ctypes.PLEverywhere -> "PLEverywhere", 0 in
  Printf.sprintf "%s#%s#%d" ls pt pi 
  |> name_of_string

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
(********************* Refined Types and Stores ********************)
(*******************************************************************)

type refctype  = (Ctypes.index * C.reft) Ctypes.prectype
type refcfun   = (Ctypes.index * C.reft) Ctypes.precfun
type refldesc  = (Ctypes.index * C.reft) Ctypes.LDesc.t
type refstore  = (Ctypes.index * C.reft) Ctypes.prestore


let d_index_reft () (i,r) = 
  let di = Ctypes.d_index () i in
  let dc = Pretty.text " , " in
  let dr = Misc.fsprintf (C.print_reft None) r |> Pretty.text in
  Pretty.concat (Pretty.concat di dc) dr

let d_refstore = Ctypes.d_precstore d_index_reft 
let d_refctype = Ctypes.d_prectype d_index_reft
let d_refcfun  = Ctypes.d_precfun d_index_reft

let reft_of_refctype = function
  | Ctypes.CTInt (_,(_,r)) 
  | Ctypes.CTRef (_,(_,r)) -> r

let refctype_of_reft_ctype r = function
  | Ctypes.CTInt (x,y) -> (Ctypes.CTInt (x, (y,r))) 
  | Ctypes.CTRef (x,y) -> (Ctypes.CTRef (x, (y,r))) 

let ctype_of_refctype = function
  | Ctypes.CTInt (x, (y, _)) -> Ctypes.CTInt (x, y) 
  | Ctypes.CTRef (x, (y, _)) -> Ctypes.CTRef (x, y)

(* API *)
let cfun_of_refcfun   = Ctypes.precfun_map ctype_of_refctype 
let qlocs_of_refcfun  = fun ft -> ft.Ctypes.qlocs
let args_of_refcfun   = fun ft -> ft.Ctypes.args
let ret_of_refcfun    = fun ft -> ft.Ctypes.ret
let stores_of_refcfun = fun ft -> (ft.Ctypes.sto_in, ft.Ctypes.sto_out)
let mk_refcfun qslocs args ist ret ost = 
  { Ctypes.qlocs   = qslocs; 
    Ctypes.args    = args;
    Ctypes.ret     = ret;
    Ctypes.sto_in  = ist;
    Ctypes.sto_out = ost; }

let slocs_of_store st = 
  SLM.fold (fun x _ xs -> x::xs) st []


(*******************************************************************)
(******************** Operations on Refined Stores *****************)
(*******************************************************************)

let refstore_empty = SLM.empty

let refstore_mem l sto = SLM.mem l sto
let refstore_remove l sto = SLM.remove l sto

let refstore_set sto l rd =
  try SLM.add l rd sto with Not_found -> 
    assertf "refstore_set"

let refstore_get sto l =
  try SLM.find l sto with Not_found ->
    (Errormsg.error "Cannot find location %a in store\n" Sloc.d_sloc l;   
     asserti false "refstore_get"; assert false)

let plocs_of_refldesc rd = 
  Ctypes.LDesc.foldn begin fun _ plocs ploc _ -> ploc::plocs end [] rd
  |> List.rev

let sloc_binds_of_refldesc l rd = 
  Ctypes.LDesc.foldn begin fun i binds ploc rct ->
    ((name_of_sloc_ploc l ploc, rct), ploc)::binds
  end [] rd
  |> List.rev

let binds_of_refldesc l rd = 
  sloc_binds_of_refldesc l rd 
  |> List.filter (fun (_, ploc) -> not (Ctypes.ploc_periodic ploc))
  |> List.map fst
  (* |> List.map (fun (n,r) -> Pretty.printf "binds_of_refldesc: %s \n" (Sy.to_string n); (n,r)) *)

let refldesc_subs = fun rd f -> Ctypes.LDesc.mapn f rd 

let refdesc_find ploc rd = 
  match Ctypes.LDesc.find ploc rd with
  | [(ploc', rct)] -> 
      (rct, Ctypes.ploc_periodic ploc')
  | _ -> assertf "refdesc_find"

let addr_of_refctype loc = function
  | Ctypes.CTRef (cl, (i,_)) when not (Sloc.is_abstract cl) ->
      (cl, Ctypes.ploc_of_index i)
  | cr ->
      let s = cr  |> d_refctype () |> Pretty.sprint ~width:80 in
      let l = loc |> d_loc () |> Pretty.sprint ~width:80 in
      let _ = asserti false "addr_of_refctype: bad arg %s at %s \n" s l in
      assert false

let ac_refstore_read loc sto cr = 
  let (l, ploc) = addr_of_refctype loc cr in 
  SLM.find l sto 
  |> refdesc_find ploc 

(* API *)
let refstore_read loc sto cr = 
  ac_refstore_read loc sto cr |> fst

(* API *)
let is_soft_ptr loc sto cr = 
  ac_refstore_read loc sto cr |> snd

let refstore_write loc sto rct rct' = 
  let (cl, ploc) = addr_of_refctype loc rct in
  let _  = assert (not (Sloc.is_abstract cl)) in
  let ld = SLM.find cl sto in
  let ld = Ctypes.LDesc.remove ploc ld in
  let ld = Ctypes.LDesc.add ploc rct' ld in
  SLM.add cl ld sto


(*******************************************************************)
(******************(Basic) Builtin Types and Sorts *****************)
(*******************************************************************)

let so_int = So.Int
let so_ref = So.Ptr 
let so_bls = So.Func [so_ref; so_ref] 
let so_skl = So.Func [so_int; so_int]
let vv_int = Sy.value_variable so_int
let vv_ref = Sy.value_variable so_ref
let vv_bls = Sy.value_variable so_bls
let vv_skl = Sy.value_variable so_skl

let sorts  = [] (* TBD: [so_int; so_ref] *)

let uf_bbegin = name_of_string "BLOCK_BEGIN"
let uf_bend   = name_of_string "BLOCK_END"
let uf_skolem = name_of_string "SKOLEM"

(*
let ct_int = Ctypes.CTInt (Cil.bytesSizeOfInt Cil.IInt, Ctypes.ITop)

let int_refctype_of_ras ras =
  let r = C.make_reft vv_int So.Int ras in
  (refctype_of_reft_ctype r ct_int)

let true_int  = int_refctype_of_ras []
let ne_0_int  = int_refctype_of_ras [C.Conc (A.pAtom (A.eVar vv_int, A.Ne, A.zero))] *)

let mk_pure_cfun args ret = 
  mk_refcfun [] args refstore_empty ret refstore_empty

let builtins    = 
  [(uf_bbegin, C.make_reft vv_bls so_bls []);
   (uf_bend, C.make_reft vv_bls so_bls []);
   (uf_skolem, C.make_reft vv_skl so_skl [])]

let skolem =
  let (fresh_int, _) = Misc.mk_int_factory () in
    fun () -> A.eApp (uf_skolem, [A.eCon (A.Constant.Int (fresh_int ()))])

(*******************************************************************)
(****************** Tag/Annotation Generation **********************)
(*******************************************************************)

type binding = TVar of string * refctype
             | TFun of string * refcfun
             | TSto of string * refstore

let tags_of_binds s binds = 
  let s_typ = Ctypes.prectype_map (Misc.app_snd (C.apply_solution s)) in
  let s_fun = Ctypes.precfun_map s_typ in
  let s_sto = Ctypes.prestore_map_ct s_typ in
  List.fold_left begin fun (d, kts) -> function
    | TVar (x, cr) -> 
        let k,t  = x, ("variable "^x) in
        let d'   = Pretty.dprintf "%s :: @[%a@] \n \n" t d_refctype (s_typ cr) in
        (Pretty.concat d d', (k,t)::kts)
    | TFun (f, cf) -> 
        let k,t  = f, ("function "^f) in
        let d'   = Pretty.dprintf "%s :: @[%a@] \n \n" t d_refcfun (s_fun cf) in
        (Pretty.concat d d', (k,t)::kts)
    | TSto (f, st) -> 
        let kts' =  slocs_of_store st 
                 |> List.map (Pretty.sprint ~width:80 <.> Sloc.d_sloc ())
                 |> List.map (fun s -> (s, s^" |->")) in
        let d'   = Pretty.dprintf "funstore %s :: @[%a@] \n \n" f d_refstore (s_sto st) in
        (Pretty.concat d d', kts' ++ kts)
  end (Pretty.nil, []) binds

let generate_annots file d = 
  let fn = file ^ ".annot" in
  let oc = open_out fn in
  let _  = Pretty.fprint ~width:80 oc d in
  let _  = close_out oc in
  ()

let generate_tags file kts =
  let fn = file ^ ".tags" in
  let oc = open_out fn in
  let _  = kts |> List.sort (fun (k1,_) (k2,_) -> compare k1 k2) 
               |> List.iter (fun (k,t) -> ignore <| Pretty.fprintf oc "%s\t%s.annot\t/%s/\n" k file t) in
  let _  = close_out oc in
  ()

let annotr    = ref [] 

(* API *)
let annot_var  = fun x cr   -> annotr := TVar ((string_of_name x), cr) :: !annotr
let annot_fun  = fun f cf   -> annotr := TFun (f, cf) :: !annotr
let annot_sto  = fun f st   -> annotr := TSto (f, st) :: !annotr
let annot_dump = fun file s -> !annotr 
                               |> tags_of_binds s 
                               >> (fst <+> generate_annots file)
                               >> (snd <+> generate_tags file) 
                               |> ignore

(*******************************************************************)
(************************** Environments ***************************)
(*******************************************************************)

type cilenv  = refcfun SM.t * refctype YM.t

let ce_rem   = fun n cenv     -> Misc.app_snd (YM.remove n) cenv
let ce_mem   = fun n (_, vnv) -> YM.mem n vnv


let ce_find n (_, vnv) =
  try YM.find n vnv with Not_found -> 
    assertf "Unknown name! %s" (Sy.to_string n)

let ce_find_fn s (fnv, _) =
  try SM.find s fnv with Not_found ->
    assertf "Unknown function! %s" s

let ce_adds (fnv, vnv) ncrs =
  let _ = List.iter (Misc.uncurry annot_var) ncrs in
  (fnv, List.fold_left (fun env (n, cr) -> YM.add n cr env) vnv ncrs)
 
let ce_adds_fn (fnv, vnv) sfrs = 
  let _ = List.iter (Misc.uncurry annot_fun) sfrs in
  (List.fold_left (fun fnv (s, fr) -> SM.add s fr fnv) fnv sfrs, vnv)

let ce_mem_fn = fun s (fnv, _) -> SM.mem s fnv

let ce_empty = (SM.empty, YM.empty) 

let d_cilenv () (fnv, _) = failwith "TBD: d_cilenv"

(* 
let d_funbind () (fn, fr) = 
  P.dprintf "%s :: \n %a" fn (Ctypes.d_precfun ...) fr

let d_cilenv () (fnv, _) =
  let fs = Misc.sm_to_list fnv in
  Pretty.seq (Pretty.text "\n") (d_funbind ())  
*)


(*
let ce_project base_env fun_env ns =
  ns |> Misc.filter (fun vn -> not (YM.mem vn base_env))
     |> List.fold_left begin 
         fun env n -> 
           asserts (YM.mem n fun_env) "ce_project";
           YM.add n (YM.find n fun_env) env
        end base_env

let ce_iter f cenv = 
  YM.iter (fun n cr -> f n cr) cenv

*)
let builtin_env =
  List.fold_left (fun env (n, r) -> YM.add n r env) YM.empty builtins

let env_of_cilenv (_, vnv) = 
  YM.fold begin fun n rct env -> 
    YM.add n (reft_of_refctype rct) env
  end vnv builtin_env 

let print_rctype so ppf rct =
  rct |> reft_of_refctype |> C.print_reft so ppf 

let print_binding so ppf (n, rct) = 
  F.fprintf ppf "%a : %a" Sy.print n (print_rctype so) rct

let print_ce so ppf (_, vnv) =
  YM.iter begin fun n cr -> 
    F.fprintf ppf "@[%a@]@\n" (print_binding so) (n, cr) 
  end vnv

(*******************************************************************)
(************************** Templates ******************************)
(*******************************************************************)

let fresh_kvar = 
  let r = ref 0 in
  fun () -> r += 1 |> string_of_int |> (^) "k_" |> Sy.of_string

let refctype_of_ctype f = function
  | Ctypes.CTInt (i, x) as t ->
      let r = C.make_reft vv_int so_int (f t) in
      (Ctypes.CTInt (i, (x, r))) 
  | Ctypes.CTRef (l, x) as t ->
      let r = C.make_reft vv_ref so_ref (f t) in
      (Ctypes.CTRef (l, (x, r))) 

(*
let reftype_of_bindings f = function
  | None -> 
      []
  | Some sts -> 
      List.map begin fun (s, t, _) -> 
        (name_of_string s, reftype_of_ctype f t) 
      end sts
*)

let is_base = function
  | TInt _ -> true
  | _      -> false

(****************************************************************)
(************************** Refinements *************************)
(****************************************************************)

let sort_of_prectype = function
  | Ctypes.CTInt _ -> so_int 
  | Ctypes.CTRef _ -> so_ref 

let ra_zero ct = 
  let vv = ct |> sort_of_prectype |> Sy.value_variable in
  [C.Conc (A.pAtom (A.eVar vv, A.Eq, A.zero))]

let ra_fresh        = fun _ -> [C.Kvar ([], fresh_kvar ())] 
let ra_true         = fun _ -> []
let t_fresh         = fun ct -> refctype_of_ctype ra_fresh ct 
let t_true          = fun ct -> refctype_of_ctype ra_true ct

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

let mk_eq_uf uf xs ys =
  let _ = asserts (List.length xs = List.length ys) "mk_eq_uf" in
  A.pAtom ((A.eApp (uf, xs)), A.Eq, (A.eApp (uf , ys)))

let is_reference cenv x =
  if List.mem_assoc x builtins then (* TBD: REMOVE GROSS HACK *)
    false
  else
    match ce_find x cenv with 
      | Ctypes.CTRef (_,(_,_)) -> true
      | _ -> false

let t_exp_ptr cenv ct vv e = (* TBD: REMOVE UNSOUND AND SHADY HACK *)
  let erefs = A.Expression.support e |> List.filter (is_reference cenv) in
  match ct, erefs with
  | (Ctypes.CTRef _), [x]  ->
      let x  = A.eVar x  in
      let vv = A.eVar vv in
      [C.Conc (mk_eq_uf uf_bbegin [vv] [x]);
       C.Conc (mk_eq_uf uf_bend   [vv] [x])]
  | _ -> 
      []

let t_ctype vv = function
  | Ctypes.CTInt (_, Ctypes.IInt n)      -> [C.Conc (A.pAtom (A.eVar vv, A.Eq, A.eCon (A.Constant.Int n)))]
  | Ctypes.CTInt (_, Ctypes.ISeq (n, _)) -> [C.Conc (A.pAtom (A.eVar vv, A.Ge, A.eCon (A.Constant.Int n)))]
  | _                                    -> []

let t_exp cenv ct e =
  let so = sort_of_prectype ct in
  let vv = Sy.value_variable so in
  let e  = CI.expr_of_cilexp skolem e in
  let rs = (t_ctype vv ct) ++ (t_exp_ptr cenv ct vv e) ++ [C.Conc (A.pAtom (A.eVar vv, A.Eq, e))] in
  let r  = C.make_reft vv so rs in
  refctype_of_reft_ctype r ct

let t_name (_, vnv) n = 
  let _  = asserti (YM.mem n vnv) "t_name: reading unbound var %s" (string_of_name n) in
  let rct = YM.find n vnv in
  let so = rct |> reft_of_refctype |> C.sort_of_reft in
  let vv = Sy.value_variable so in
  let r  = C.make_reft vv so [C.Conc (A.pAtom (A.eVar vv, A.Eq, A.eVar n))] in
  rct |> ctype_of_refctype |> refctype_of_reft_ctype r

let t_fresh_fn = Ctypes.precfun_map t_fresh 

(*
let rec t_fresh_typ ty = 
  if !Constants.safe then assertf "t_fresh_typ" else 
    match ty with 
    | TInt _ | TVoid _ -> 
        t_fresh ct_int 
    | TFun (t, None, _, _) -> 
        Fun ([], t_fresh_typ t)
    | TFun (t, Some sts, _, _) ->
        let args = List.map (fun (s,t,_) -> (name_of_string s, t_fresh_typ t)) sts in
        Fun (args, t_fresh_typ t)
    | _ -> assertf "t_fresh_typ: fancy type" 
*)

let t_ctype_refctype ct rct = 
  refctype_of_reft_ctype (reft_of_refctype rct) ct

let refctype_subs f nzs = 
  nzs |> Misc.map (Misc.app_snd f) 
      |> C.theta
      |> Misc.app_snd
      |> Ctypes.prectype_map

let t_subs_locs    = Ctypes.prectype_subs 
let t_subs_exps    = refctype_subs (CI.expr_of_cilexp skolem)
let t_subs_names   = refctype_subs A.eVar
let refstore_fresh = fun fn st -> st |> Ctypes.prestore_map_ct t_fresh >> annot_sto fn 
let refstore_subs  = fun loc f subs st -> Ctypes.prestore_map_ct (f subs) st

(* 
let refldesc_subs_ploc f rd = 
  Ctypes.LDesc.mapn (fun _ pl rct -> f pl rct) rd

let refstore_subs_ploc f sto =
  Sloc.SlocMap.map (refldesc_subs_ploc f) sto
*)

let refstore_subs_locs loc lsubs sto = 
  List.fold_left begin fun sto (l, l') -> 
    let rv = 
    if not (refstore_mem l sto) then sto else
      let plocs = refstore_get sto l |> plocs_of_refldesc in
      let ns    = List.map (name_of_sloc_ploc l) plocs in
      let ns'   = List.map (name_of_sloc_ploc l') plocs in
      let subs  = List.combine ns ns' in
      refstore_subs loc t_subs_names subs sto
    in
    (* let _ = Pretty.printf "refstore_subs_locs: l = %a, l' = %a \n sto = %a \n sto' = %a \n"
            Sloc.d_sloc l Sloc.d_sloc l' d_refstore sto d_refstore rv in *)
    rv
  end sto lsubs

(****************************************************************)
(********************** Constraints *****************************)
(****************************************************************)

let non_tmp = fun n _ -> n |> Sy.to_string |> Co.is_cil_tempvar |> not 

let make_wfs cenv rct _ =
  let env = env_of_cilenv cenv 
            |> Ast.Symbol.sm_filter non_tmp in
  let r   = reft_of_refctype rct in
  [C.make_wf env r None]

let make_wfs_refstore env sto tag =
  SLM.fold begin fun l rd ws ->
    let ncrs = sloc_binds_of_refldesc l rd in
    let env' = ncrs |> List.filter (fun (_,ploc) -> not (Ctypes.ploc_periodic ploc)) 
                    |> List.map fst
                    |> ce_adds env in 
    let ws'  = Misc.flap (fun ((_,cr),_) -> make_wfs env' cr tag) ncrs in
    ws' ++ ws
  end sto []

let make_wfs_fn cenv rft tag =
  let args  = List.map (Misc.app_fst Sy.of_string) rft.Ctypes.args in
  let env'  = ce_adds cenv args in
  let retws = make_wfs env' rft.Ctypes.ret tag in
  let argws = Misc.flap (fun (_, rct) -> make_wfs env' rct tag) args in
  let inws  = make_wfs_refstore env' rft.Ctypes.sto_in tag in
  let outws = make_wfs_refstore env' rft.Ctypes.sto_out tag in
  Misc.tr_rev_flatten [retws ; argws ; inws ; outws]

let make_dep pol xo yo =
  (xo, yo) |> Misc.map_pair (Misc.maybe_map CilTag.tag_of_t)
           |> Misc.uncurry (C.make_dep pol)

let add_deps tago tag = 
  match tago with 
  | Some t -> [make_dep true (Some t) (Some tag)] 
  | _      -> [] 

let rec make_cs cenv p rct1 rct2 tago tag =
  let env    = env_of_cilenv cenv in
  let r1, r2 = Misc.map_pair reft_of_refctype (rct1, rct2) in
  let cs     = [C.make_t env p r1 r2 None (CilTag.tag_of_t tag)] in
  let ds     = [] (* add_deps tago tag *) in
  cs, ds

let make_cs_refldesc env p (sloc1, rd1) (sloc2, rd2) tago tag =
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
      make_cs env' p lhs rhs tago tag 
  end ncrs12
  |> Misc.splitflatten

let make_cs_refstore env p st1 st2 polarity tago tag loc =
(*  let _  = Pretty.printf "make_cs_refstore: pol = %b, st1 = %a, st2 = %a, loc = %a \n"
           polarity Ctypes.d_prestore_addrs st1 Ctypes.d_prestore_addrs st2 Cil.d_loc loc in
  let _  = Pretty.printf "st1 = %a \n" d_refstore st1 in
  let _  = Pretty.printf "st2 = %a \n" d_refstore st2 in *)
  let rv =
  (if polarity then st2 else st1)
  |> slocs_of_store 
  |> Misc.map begin fun sloc ->
       let lhs = (sloc, refstore_get st1 sloc) in
       let rhs = (sloc, refstore_get st2 sloc) in
       make_cs_refldesc env p lhs rhs tago tag 
     end 
  |> Misc.splitflatten in
(*  let _ = F.printf "make_cs_refstore: %a" (Misc.pprint_many true "\n" (C.print_t None)) rv in *) 
  rv

(* API *)
let make_cs cenv p rct1 rct2 tago tag loc =
  try make_cs cenv p rct1 rct2 tago tag with ex ->
    let _ = Cil.errorLoc loc "make_cs fails with: %s" (Printexc.to_string ex) in
    let _ = asserti false "make_cs" in 
    assert false

let make_cs_refstore env p st1 st2 polarity tago tag loc =
  try make_cs_refstore env p st1 st2 polarity tago tag loc with ex ->
    let _ = Cil.errorLoc loc "make_cs_refstore fails with: %s" (Printexc.to_string ex) in
    let _ = asserti false "make_cs_refstore" in 
    assert false

let make_cs_refldesc env p (sloc1, rd1) (sloc2, rd2) tago tag loc =
  try make_cs_refldesc env p (sloc1, rd1) (sloc2, rd2) tago tag with ex ->
    let _ = Cil.errorLoc loc "make_cs_refldesc fails with: %s" (Printexc.to_string ex) in 
    let _ = asserti false "make_cs_refldesc" in 
    assert false
