module F   = Format
module ST  = Ssa_transform
module  A  = Ast
module  P  = Ast.Predicate
module  C  = Constraint
module Sy  = Ast.Symbol
module So  = Ast.Sort
module CI  = CilInterface

module YM  = Sy.SMap
module SM  = Misc.StringMap
(* module  T  = Ctypes *)
module SLM = Ctypes.SLM

open Misc.Ops
open Cil

(*******************************************************************)
(************** Interface between LiquidC and Fixpoint *************)
(*******************************************************************)

(****************************************************************)
(********************* Sorts ************************************)
(****************************************************************)

let so_int = So.Int
let so_ref = So.Int (* TBD: So.Unint "ref" *)
let vv_int = Sy.value_variable so_int
let vv_ref = Sy.value_variable so_ref
let sorts  = [] (* TBD: [so_int; so_ref] *)

(*******************************************************************)
(******************************** Names ****************************)
(*******************************************************************)

type name = Sy.t

let name_of_varinfo = fun v -> Sy.of_string v.vname

let name_of_string  = fun s -> Sy.of_string s

let name_fresh =
  let t, _ = Misc.mk_int_factory () in
  (fun _ -> t () |> string_of_int |> (^) "lqn#" |> name_of_string)

let name_of_sloc_ploc l p = 
  let lt,li = match l with Ctypes.ALoc i -> "ALoc",i | Ctypes.CLoc i -> "CLoc", i in
  let pt,pi = match p with Ctypes.PLAt i -> "PLAt",i | Ctypes.PLSeq i -> "PLSeq", i in
  Printf.sprintf "%s#%d#%s#%d" lt li pt pi 
  |> name_of_string

(*******************************************************************)
(************************** Refined Types **************************)
(*******************************************************************)

type reftype = Base of (Ctypes.index * C.reft) Ctypes.prectype
             | Fun  of (name * reftype) list * reftype  

let ctype_of_rctype = function
  | Ctypes.CTInt (x, (y, _)) -> Ctypes.CTInt (x, y) 
  | Ctypes.CTRef (x, (y, _)) -> Ctypes.CTRef (x, y)

let reft_of_rctype = function
  | Ctypes.CTInt (_,(_,r)) 
  | Ctypes.CTRef (_,(_,r)) -> r

let reftype_of_reft_ctype r = function
  | Ctypes.CTInt (x,y) -> Base (Ctypes.CTInt (x, (y,r))) 
  | Ctypes.CTRef (x,y) -> Base (Ctypes.CTRef (x, (y,r))) 

let rec reftype_map f cr = 
  match cr with
  | Base rct ->
      Base (f rct) 
  | Fun (ncrs, r) -> 
      let ncrs' = Misc.map (Misc.app_snd (reftype_map f)) ncrs in
      let r'    = reftype_map f r in
      Fun (ncrs', r')


let rec print_reftype so ppf = function  
  | Base rct ->
      C.print_reft so ppf (reft_of_rctype rct)
  | Fun (args, ret) ->
      F.fprintf ppf "(%a) -> %a"
        (Misc.pprint_many false "," (print_binding so)) args
        (print_reftype so) ret

and print_binding so ppf (n, cr) = 
  F.fprintf ppf "%a : %a" Sy.print n (print_reftype so) cr

(*******************************************************************)
(*************************** Refined Stores ************************)
(*******************************************************************)

type refldesc = (Ctypes.index * C.reft) Ctypes.LDesc.t
type refstore = (Ctypes.index * C.reft) Ctypes.prestore

let refstore_empty = SLM.empty

let refstore_mem l sto = SLM.mem l sto
let refstore_remove l sto = SLM.remove l sto

let refstore_set sto l rd = 
  try SLM.add l rd sto with Not_found -> 
    assertf "refstore_set"

let refstore_get sto l =
  try SLM.find l sto with Not_found ->
    assertf "refstore_get"

let binds_of_refldesc l rd = 
  Ctypes.LDesc.foldn 
    (fun i binds ploc rct -> (name_of_sloc_ploc l ploc, Base rct)::binds)
    [] rd
  |> List.rev

let refldesc_subs rd f =
  Ctypes.LDesc.mapn begin fun i _ rct -> 
      match f i (Base rct) with 
      | Base rct' -> rct' 
      | _ -> assertf "refldesc_subs: bad substitution function" 
  end rd

let refdesc_find ploc rd = 
  match Ctypes.LDesc.find ploc rd with
  | [(ploc', rct)] when ploc = ploc' -> rct
  | _ -> assertf "refdesc_find"

let addr_of_reftype = function
  | Base (Ctypes.CTRef (Ctypes.CLoc l as cl, (i,_))) -> 
      (cl, Ctypes.ploc_of_index i)
  | _ -> assertf "addr_of_reftype: bad args"

let refstore_read sto cr = 
  let (l, ploc) = addr_of_reftype cr in 
  let rct = try SLM.find l sto |> refdesc_find ploc 
            with _ -> assertf "refstore_read: bad address!" in
  Base rct

let refstore_write sto cr cr' = 
  let (cl, ploc) = addr_of_reftype cr in 
  match cr' with
  | Base rct' -> 
      let ld = SLM.find cl sto in
      let ld = Ctypes.LDesc.remove ploc ld in
      let ld = Ctypes.LDesc.add ploc rct' ld in
      SLM.add cl ld sto
  | _ -> assertf "refstore_write: bad target!" 

(*******************************************************************)
(********************** (Basic) Builtin Types **********************)
(*******************************************************************)

(* Move to its own module *)
let ct_int = Ctypes.CTInt (Cil.bytesSizeOfInt Cil.IInt, Ctypes.ITop)
 
let int_reftype_of_ras ras =
  let r = C.make_reft vv_int So.Int ras in
  reftype_of_reft_ctype r ct_int

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
  | Ctypes.CTInt (i, x) as t ->
      let r = C.make_reft vv_int So.Int (f t) in
      Base (Ctypes.CTInt (i, (x, r))) 
  | Ctypes.CTRef (l, x) as t ->
      let r = C.make_reft vv_int So.Int (f t) in
      Base (Ctypes.CTRef (l, (x, r))) 

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

let sort_of_prectype = function
  | Ctypes.CTInt _ -> so_int 
  | Ctypes.CTRef _ -> so_ref 

let t_fresh = reftype_of_ctype (fun _ -> [C.Kvar ([], fresh_kvar ())]) 

let t_true  = reftype_of_ctype (fun _ -> [])

let t_exp ct e =
  let so = sort_of_prectype ct in
  let vv = Sy.value_variable so in
  let e  = CI.expr_of_cilexp e in
  let r  = C.make_reft vv so [C.Conc (A.pAtom (A.eVar vv, A.Eq, e))] in
  reftype_of_reft_ctype r ct 

let t_name env n = 
  asserts (YM.mem n env) "t_cilname: reading unbound var -- return false reft";
  match YM.find n env with
  | Base rct -> 
      let so = rct |> reft_of_rctype |> C.sort_of_reft in
      let vv = Sy.value_variable so in
      let r  = C.make_reft vv so [C.Conc (A.pAtom (A.eVar vv, A.Eq, A.eVar n))] in
      reftype_of_reft_ctype r (ctype_of_rctype rct)
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

let t_ctype_reftype ct = function
  | Fun _    -> assertf "merge_ctype_reftype: merge with funtype"
  | Base rct -> reftype_of_reft_ctype (reft_of_rctype rct) ct 

let reftype_subs f nzs = 
  nzs |> Misc.map (Misc.app_snd f) 
      |> C.theta
      |> Misc.app_snd
      |> Ctypes.prectype_map
      |> reftype_map 

let t_subs_exps  = reftype_subs CI.expr_of_cilexp

let t_subs_names = reftype_subs A.eVar

let refstore_fresh (st : Ctypes.store) : refstore = 
  SLM.map begin fun ld -> 
    Ctypes.LDesc.map begin fun ct -> 
      match t_fresh ct with  Base rct -> rct | _ -> assertf "refstore_fresh"
    end ld
  end st

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

let make_cs_binds env p ncrs ncrs' bs loc =
  let _    = asserts (List.length ncrs = List.length ncrs') "make_cs_block 1" in
  let _    = asserts (List.length ncrs = List.length bs)    "make_cs_block 2" in
  let env' = ce_adds env ncrs in
  let subs = List.map2 (fun (n,_) (n',_)  -> (n', n) ) ncrs ncrs' in
  List.map2 (fun (n,_) (_,cr') -> (n, cr')) ncrs ncrs'
  |> List.combine bs
  |> Misc.flap (fun (b, (n,cr')) -> 
                  if not b then [] else
                    let lhs = t_name env' n in
                    let rhs = t_subs_names subs cr' in
                    make_cs env' p lhs rhs loc)

let make_wfs_refstore env sto loc =
  SLM.fold begin fun l rd ws ->
    let ncrs = binds_of_refldesc l rd in
    let env' = ce_adds env ncrs in
    let ws'  = Misc.flap (fun (_,cr) -> make_wfs env' cr loc) ncrs in
    ws' ++ ws
  end sto []
