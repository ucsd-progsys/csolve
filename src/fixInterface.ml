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
(********************* Refined Types and Stores ********************)
(*******************************************************************)

type refctype  = (Ctypes.index * C.reft) Ctypes.prectype
type refcfun   = (Ctypes.index * C.reft) Ctypes.precfun
type refldesc  = (Ctypes.index * C.reft) Ctypes.LDesc.t
type refstore  = (Ctypes.index * C.reft) Ctypes.prestore

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
let args_of_refcfun   = fun ft -> List.map (Misc.app_fst name_of_string) ft.Ctypes.args
let ret_of_refcfun    = fun ft -> ft.Ctypes.ret
let stores_of_refcfun = fun ft -> (ft.Ctypes.sto_in, ft.Ctypes.sto_out)
let mk_cfun qslocs args ist ret ost = 
  { Ctypes.qlocs   = qslocs; 
    Ctypes.args    = args;
    Ctypes.ret     = ret;
    Ctypes.sto_in  = ist;
    Ctypes.sto_out = ost; }


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
    assertf "refstore_get"

let binds_of_refldesc l rd = 
  Ctypes.LDesc.foldn 
    (fun i binds ploc rct -> (name_of_sloc_ploc l ploc, rct)::binds)
    [] rd
  |> List.rev

let refldesc_subs = fun rd f -> Ctypes.LDesc.mapn (fun i _ rct -> f i rct) rd

let refdesc_find ploc rd = 
  match Ctypes.LDesc.find ploc rd with
  | [(ploc', rct)] when ploc = ploc' -> rct
  | _ -> assertf "refdesc_find"

let addr_of_refctype = function
  | Ctypes.CTRef (Ctypes.CLoc l as cl, (i,_)) -> 
      (cl, Ctypes.ploc_of_index i)
  | _ -> assertf "addr_of_refctype: bad args"

let refstore_read sto cr = 
  let (l, ploc) = addr_of_refctype cr in 
  try SLM.find l sto |> refdesc_find ploc 
  with _ -> assertf "refstore_read: bad address!"

let refstore_write sto rct rct' = 
  let (cl, ploc) = addr_of_refctype rct in 
  let _  = match cl with Ctypes.ALoc _ -> assertf "refstore_write: abs" | _ -> () in
  (* let _  = assert (is_concrete cl) in *)
  let ld = SLM.find cl sto in
  let ld = Ctypes.LDesc.remove ploc ld in
  let ld = Ctypes.LDesc.add ploc rct' ld in
  SLM.add cl ld sto


(*******************************************************************)
(********************** (Basic) Builtin Types **********************)
(*******************************************************************)

(* Move to its own module *)
let ct_int = Ctypes.CTInt (Cil.bytesSizeOfInt Cil.IInt, Ctypes.ITop)
 
let int_refctype_of_ras ras =
  let r = C.make_reft vv_int So.Int ras in
  (refctype_of_reft_ctype r ct_int)

let true_int  = int_refctype_of_ras []
let ne_0_int  = int_refctype_of_ras [C.Conc (A.pAtom (A.eVar vv_int, A.Ne, A.zero))]

let mk_pure_cfun args ret = 
  mk_cfun [] args refstore_empty ret refstore_empty

let builtins    = []

let builtins_fn =
  [("assert", mk_pure_cfun [("b", ne_0_int)] true_int);
   ("nondet", mk_pure_cfun [] true_int)]

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
  (fnv, List.fold_left (fun env (n, cr) -> YM.add n cr env) vnv ncrs)
 
let ce_adds_fn (fnv, vnv) sfrs = 
  (List.fold_left (fun fnv (s, fr) -> SM.add s fr fnv) fnv sfrs, vnv)

let ce_empty =
  let ce = (SM.empty, YM.empty) in
  let ce = ce_adds ce builtins in
  let ce = ce_adds_fn ce builtins_fn in
  ce

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

let env_of_cilenv (_, vnv) = 
  YM.fold begin fun n rct env -> 
    YM.add n (reft_of_refctype rct) env
  end vnv YM.empty

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
      let r = C.make_reft vv_int so_ref (f t) in
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


let ra_fresh        = fun _ -> [C.Kvar ([], fresh_kvar ())] 
let ra_true         = fun _ -> []
let t_fresh         = fun ct -> refctype_of_ctype ra_fresh ct 
let t_true          = fun ct -> refctype_of_ctype ra_true ct 
let t_true_refctype = fun rct -> rct |> ctype_of_refctype |> refctype_of_ctype ra_true

(* convert {v : ct | p } into refctype *)
let t_pred ct v p = 
  let so = sort_of_prectype ct in
  let vv = Sy.value_variable so in
  let p  = P.subst p v (A.eVar vv) in
  let r  = C.make_reft vv so [C.Conc p] in
  refctype_of_reft_ctype r ct

let t_exp ct e =
  let so = sort_of_prectype ct in
  let vv = Sy.value_variable so in
  let e  = CI.expr_of_cilexp e in
  let r  = C.make_reft vv so [C.Conc (A.pAtom (A.eVar vv, A.Eq, e))] in
  refctype_of_reft_ctype r ct

let t_name (_, vnv) n = 
  asserts (YM.mem n vnv) "t_cilname: reading unbound var -- return false reft";
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

let t_subs_locs        = Ctypes.prectype_subs 
let t_subs_exps        = refctype_subs CI.expr_of_cilexp
let t_subs_names       = refctype_subs A.eVar
let refstore_fresh     = Ctypes.prestore_map_ct t_fresh
let refstore_subs_exps = fun nes st -> Ctypes.prestore_map_ct (t_subs_exps nes) st


(****************************************************************)
(********************** Constraints *****************************)
(****************************************************************)
let make_wfs cenv rct loc =
  let env = env_of_cilenv cenv in
  let r   = reft_of_refctype rct in
  [C.make_wf env r None]

let make_wfs_fn cenv rft loc =
  let args = List.map (Misc.app_fst Sy.of_string) rft.Ctypes.args in
  let env' = ce_adds cenv args in
  let rws  = make_wfs env' rft.Ctypes.ret loc in
  let aws  = Misc.flap (fun (_, rct) -> make_wfs env' rct loc) args in
  rws ++ aws

let make_wfs_refstore env sto loc =
  SLM.fold begin fun l rd ws ->
    let ncrs = binds_of_refldesc l rd in
    let env' = ce_adds env ncrs in
    let ws'  = Misc.flap (fun (_,cr) -> make_wfs env' cr loc) ncrs in
    ws' ++ ws
  end sto []

let rec make_cs cenv p rct1 rct2 loc =
  let env    = env_of_cilenv cenv in
  let r1, r2 = Misc.map_pair reft_of_refctype (rct1, rct2) in
  [C.make_t env p r1 r2 None]

let make_cs_refldesc env p (sloc1, rd1) (sloc2, rd2) loc =
  let ncrs1  = binds_of_refldesc sloc1 rd1 in
  let ncrs2  = binds_of_refldesc sloc2 rd2 in
  let _      = asserts (List.length ncrs1 = List.length ncrs2) "make_cs_refldesc" in
  let env'   = ce_adds env ncrs1 in
  let subs   = List.map2 (fun (n1,_) (n2,_)  -> (n2, n1) ) ncrs1 ncrs2 in
  Misc.flap2 begin fun (n1, _) (_, cr2) -> 
      let lhs = t_name env' n1 in
      let rhs = t_subs_names subs cr2 in
      make_cs env' p lhs rhs loc
  end ncrs1 ncrs2

let slocs_of_store st = 
  Ctypes.SLM.fold (fun x _ xs -> x::xs) st []

let make_cs_refstore env p st1 st2 polarity loc =
  (if polarity then st2 else st1)
  |> slocs_of_store 
  |> Misc.flap begin fun sloc ->
       let lhs = (sloc, refstore_get st1 sloc) in
       let rhs = (sloc, refstore_get st2 sloc) in
       make_cs_refldesc env p lhs rhs loc 
     end
