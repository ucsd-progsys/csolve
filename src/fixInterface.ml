module F  = Format
module ST = Ssa_transform
module  A = Ast
module  P = Ast.Predicate
module  C = Constraint
module Sy = Ast.Symbol
module So = Ast.Sort
module CI = CilInterface

module YM = Sy.SMap
module SM = Misc.StringMap
module  T = Ctypes

open Misc.Ops
open Cil

(*******************************************************************)
(************** Interface between LiquidC and Fixpoint *************)
(*******************************************************************)

(****************************************************************)
(********************* Sorts ************************************)
(****************************************************************)

let so_int = So.Int
let so_ref = So.Unint "ref"
let vv_int = Sy.value_variable so_int
let vv_ref = Sy.value_variable so_ref
let sorts  = [so_int; so_ref] 

(*******************************************************************)
(************************** Refined Types **************************)
(*******************************************************************)

type name = Sy.t
let name_of_varinfo = fun v -> Sy.of_string v.vname
let name_of_string  = fun s -> Sy.of_string s

type reftype = Base of (T.index * C.reft) T.prectype
             | Fun  of (name * reftype) list * reftype  


let ctype_of_rctype = function
  | T.CTInt (x, (y, _)) -> T.CTInt (x, y) 
  | T.CTRef (x, (y, _)) -> T.CTRef (x, y)

let reft_of_rctype = function
  | T.CTInt (_,(_,r)) 
  | T.CTRef (_,(_,r)) -> r

let reftype_of_reft r = function
  | T.CTInt (x,y) -> Base (T.CTInt (x, (y,r))) 
  | T.CTRef (x,y) -> Base (T.CTRef (x, (y,r))) 

let rec print_reftype so ppf = function  
  | Base rct ->
      C.print_reft so ppf (reft_of_rctype rct)
  | Fun (args, ret) ->
      F.fprintf ppf "(%a) -> %a"
        (Misc.pprint_many false "," (print_binding so)) args
        (print_reftype so) ret

and print_binding so ppf (n, cr) = 
  F.fprintf ppf "%a : %a" Sy.print n (print_reftype so) cr

let sort_of_prectype = function
  | Ctypes.CTInt _ -> so_int 
  | Ctypes.CTRef _ -> so_ref 

(*******************************************************************)
(*************************** Refined Stores ************************)
(*******************************************************************)

type refstore = (T.index * C.reft) T.prestore


(*******************************************************************)
(********************** (Basic) Builtin Types **********************)
(*******************************************************************)

(* Move to its own module *)
let ct_int = T.CTInt (Cil.bytesSizeOfInt Cil.IInt, T.ITop)
 
let int_reftype_of_ras ras =
  let r = C.make_reft vv_int So.Int ras in
  reftype_of_reft r ct_int

let true_int  = int_reftype_of_ras []
let ne_0_int  = int_reftype_of_ras [C.Conc (A.pAtom (A.eVar vv_int, A.Ne, A.zero))]

let builtins = 
  [(Sy.of_string "assert", 
      Fun ([(Sy.of_string "b", ne_0_int)], true_int));
   (Sy.of_string "nondet", 
      Fun ([], true_int))]

(*******************************************************************)
(************************** Environments ***************************)
(*******************************************************************)

type cilenv  = reftype YM.t

let ce_mem   = fun n cenv -> YM.mem n cenv

let ce_find n (cenv : reftype YM.t) =
  try YM.find n cenv with Not_found -> 
    assertf "Unknown name! %s" (Sy.to_string n)

let ce_find_fn n env = 
  match ce_find n env with
  | Fun (ncrs, cr) -> (ncrs, cr)
  | _            -> assertf "ce_args: non-function type: %s" (Sy.to_string n)

let ce_adds env ncrs = 
  List.fold_left (fun env (n, cr) -> YM.add n cr env) env ncrs
             
let ce_empty = ce_adds YM.empty builtins

let ce_project base_env fun_env ns =
  ns |> Misc.filter (fun vn -> not (YM.mem vn base_env))
     |> List.fold_left begin 
         fun env n -> 
           asserts (YM.mem n fun_env) "ce_project";
           YM.add n (YM.find n fun_env) env
        end base_env

let ce_iter f cenv = 
  YM.iter (fun n cr -> f n cr) cenv

let env_of_cilenv cenv = 
  YM.fold begin
    fun n cr env -> 
      match cr with 
      | Base rct -> YM.add n (reft_of_rctype rct) env
      | _        -> env
  end cenv YM.empty
       
let print_ce so ppf cenv =
  YM.iter begin
    fun n cr -> 
      F.fprintf ppf "@[%a@]@\n" (print_binding so) (n, cr) 
  end cenv

(*******************************************************************)
(************************** Templates ******************************)
(*******************************************************************)

let fresh_kvar = 
  let r = ref 0 in
  fun () -> r += 1 |> string_of_int |> (^) "k_" |> Sy.of_string

let rec reftype_of_ctype f = function
  | T.CTInt (i, x) as t ->
      let r = C.make_reft vv_int So.Int (f t) in
      Base (T.CTInt (i, (x, r))) 
  | T.CTRef (l, x) as t ->
      let r = C.make_reft vv_int So.Int (f t) in
      Base (T.CTRef (l, (x, r))) 

and reftype_of_bindings f = function
  | None -> 
      []
  | Some sts -> 
      List.map begin fun (s, t, _) -> 
        (name_of_string s, reftype_of_ctype f t) 
      end sts

let is_base = function
  | TInt _ -> true
  | _      -> false

(****************************************************************)
(************************** Refinements *************************)
(****************************************************************)

let t_fresh = reftype_of_ctype (fun _ -> [C.Kvar ([], fresh_kvar ())]) 

let t_true  = reftype_of_ctype (fun _ -> [])

let t_exp ct e =
  let so = sort_of_prectype ct in
  let vv = Sy.value_variable so in
  let e  = CI.expr_of_cilexp e in
  let r  = C.make_reft vv so [C.Conc (A.pAtom (A.eVar vv, A.Eq, e))] in
  reftype_of_reft r ct 

let t_name env n = 
  asserts (YM.mem n env) "t_cilname: reading unbound var -- return false reft";
  match YM.find n env with
  | Base rct -> 
      let so = rct |> reft_of_rctype |> C.sort_of_reft in
      let vv = Sy.value_variable so in
      let r  = C.make_reft vv so [C.Conc (A.pAtom (A.eVar vv, A.Eq, A.eVar n))] in
      reftype_of_reft r (ctype_of_rctype rct)
  | cr -> cr

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

let rec reftype_map f cr = 
  match cr with
  | Base rct ->
      Base (f rct) 
  | Fun (ncrs, r) -> 
      let ncrs' = Misc.map (Misc.app_snd (reftype_map f)) ncrs in
      let r'    = reftype_map f r in
      Fun (ncrs', r')

let reftype_subs f nzs = 
  nzs |> Misc.map (Misc.app_snd f) 
      |> C.theta
      |> Misc.app_snd
      |> T.prectype_map
      |> reftype_map 

let t_subs_exps  = reftype_subs CI.expr_of_cilexp
let t_subs_names = reftype_subs A.eVar

(****************************************************************)
(********************** Constraints *****************************)
(****************************************************************)

let rec make_cs cenv p lhscr rhscr loc =
  match lhscr, rhscr with
  | Base rct1, Base rct2 ->
      let env    = env_of_cilenv cenv in
      let r1, r2 = Misc.map_pair reft_of_rctype (rct1, rct2) in
      [C.make_t env p r1 r2 None]
  | _, _ ->
      assertf ("TBD:make_cs")

let rec make_wfs cenv cr loc =
  match cr with
  | Base rct ->
      let env = env_of_cilenv cenv in
      let r   = reft_of_rctype rct in
      [C.make_wf env r None]
  | Fun (args, ret) ->
      let env' = ce_adds cenv args in
      (make_wfs env' ret loc) ++ 
      (Misc.flap (fun (_, cr) -> make_wfs env' cr loc) args)
