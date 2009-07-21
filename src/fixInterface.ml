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
(* module  T  = Ctypes *)
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

(*******************************************************************)
(********************* Refined Types and Stores ********************)
(*******************************************************************)

type refctype  = (Ctypes.index * C.reft) Ctypes.prectype
type refcfun   = (Ctypes.index * C.reft) Ctypes.precfun
type refldesc  = (Ctypes.index * C.reft) Ctypes.LDesc.t
type refstore  = (Ctypes.index * C.reft) Ctypes.prestore


let d_index_reft () (_,r) = 
  Misc.fsprintf (C.print_reft None) r 
  |> Pretty.text

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
    Errormsg.error "Cannot find location %a in store\n" Sloc.d_sloc l;   
    assertf "refstore_get"

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
  |> List.map (fun (n,r) -> Printf.printf "binds_of_refldesc: %s \n" (Sy.to_string n); (n,r))

let refldesc_subs = fun rd f -> Ctypes.LDesc.mapn f rd 

let refdesc_find ploc rd = 
  match Ctypes.LDesc.find ploc rd with
  | [(ploc', rct)] -> 
      (rct, Ctypes.ploc_periodic ploc') (* not (ploc = ploc') *) (* i.e. soft *)
  | _ -> assertf "refdesc_find"

let addr_of_refctype = function
  | Ctypes.CTRef (cl, (i,_)) when not (Sloc.is_abstract cl) ->
      (cl, Ctypes.ploc_of_index i)
  | cr -> 
      ignore(Errormsg.error "addr_of_refctype: bad arg = %a" d_refctype cr);
      assertf "addr_of_refctype: bad args"

let ac_refstore_read sto cr = 
  let (l, ploc) = addr_of_refctype cr in 
  SLM.find l sto 
  |> refdesc_find ploc 

(* API *)
let refstore_read sto cr = 
  ac_refstore_read sto cr |> fst

(* API *)
let is_soft_ptr sto cr = 
  ac_refstore_read sto cr |> snd

let refstore_write sto rct rct' = 
  let (cl, ploc) = addr_of_refctype rct in
  let _  = assert (not (Sloc.is_abstract cl)) in
  let ld = SLM.find cl sto in
  let ld = Ctypes.LDesc.remove ploc ld in
  let ld = Ctypes.LDesc.add ploc rct' ld in
  SLM.add cl ld sto


(*******************************************************************)
(******************(Basic) Builtin Types and Sorts *****************)
(*******************************************************************)

let so_int = So.Int
let so_ref = So.Int (* TBD: So.Unint "ref" *)
let so_ufs = So.Func [so_ref; so_int] 
let vv_int = Sy.value_variable so_int
let vv_ref = Sy.value_variable so_ref
let vv_ufs = Sy.value_variable so_ufs

let sorts  = [] (* TBD: [so_int; so_ref] *)

let uf_bbegin = name_of_string "BLOCK_BEGIN"
let uf_bend   = name_of_string "BLOCK_END"

(* Move to its own module *)
let ct_int = Ctypes.CTInt (Cil.bytesSizeOfInt Cil.IInt, Ctypes.ITop)
 
let int_refctype_of_ras ras =
  let r = C.make_reft vv_int So.Int ras in
  (refctype_of_reft_ctype r ct_int)

let true_int  = int_refctype_of_ras []
let ne_0_int  = int_refctype_of_ras [C.Conc (A.pAtom (A.eVar vv_int, A.Ne, A.zero))]

let mk_pure_cfun args ret = 
  mk_refcfun [] args refstore_empty ret refstore_empty

let builtins    = 
  [(uf_bbegin, C.make_reft vv_ufs so_ufs []);
   (uf_bend, C.make_reft vv_ufs so_ufs [])]

(* Added to lib.spec
let builtins_fn = []
  [("assert", mk_pure_cfun [("b", ne_0_int)] true_int);
   ("nondet", mk_pure_cfun [] true_int)]
*)

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

let t_exp cenv ct e =
  let so = sort_of_prectype ct in
  let vv = Sy.value_variable so in
  let e  = CI.expr_of_cilexp e in
  let rs = (t_exp_ptr cenv ct vv e) ++ [C.Conc (A.pAtom (A.eVar vv, A.Eq, e))] in
  let r  = C.make_reft vv so rs in
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

let t_subs_locs    = Ctypes.prectype_subs 
let t_subs_exps    = refctype_subs CI.expr_of_cilexp
let t_subs_names   = refctype_subs A.eVar
let refstore_fresh = Ctypes.prestore_map_ct t_fresh
let refstore_subs  = fun f subs st -> Ctypes.prestore_map_ct (f subs) st

(* 
let refldesc_subs_ploc f rd = 
  Ctypes.LDesc.mapn (fun _ pl rct -> f pl rct) rd

let refstore_subs_ploc f sto =
  Sloc.SlocMap.map (refldesc_subs_ploc f) sto
*)

let refstore_subs_locs lsubs sto = 
  List.fold_left begin fun sto (l, l') -> 
    let rv = 
    if not (refstore_mem l sto) then sto else 
      let plocs = refstore_get sto l |> plocs_of_refldesc in
      let ns    = List.map (name_of_sloc_ploc l) plocs in
      let ns'   = List.map (name_of_sloc_ploc l') plocs in
      let subs  = List.combine ns ns' in
      refstore_subs t_subs_names subs sto
    in
    let _ = Pretty.printf "refstore_subs_locs: l = %a, l' = %a \n sto = %a \n sto' = %a \n"
            Sloc.d_sloc l Sloc.d_sloc l' d_refstore sto d_refstore rv in
    rv
  end sto lsubs



(****************************************************************)
(********************** Constraints *****************************)
(****************************************************************)
let make_wfs cenv rct loc =
  let env = env_of_cilenv cenv in
  let r   = reft_of_refctype rct in
  [C.make_wf env r None]

let make_wfs_refstore env sto loc =
  SLM.fold begin fun l rd ws ->
    let ncrs = sloc_binds_of_refldesc l rd in
    let env' = ncrs |> List.filter (fun (_,ploc) -> not (Ctypes.ploc_periodic ploc)) 
                    |> List.map fst
                    |> ce_adds env in 
    let ws'  = Misc.flap (fun ((_,cr),_) -> make_wfs env' cr loc) ncrs in
    ws' ++ ws
  end sto []

let make_wfs_fn cenv rft loc =
  let args  = List.map (Misc.app_fst Sy.of_string) rft.Ctypes.args in
  let env'  = ce_adds cenv args in
  let retws = make_wfs env' rft.Ctypes.ret loc in
  let argws = Misc.flap (fun (_, rct) -> make_wfs env' rct loc) args in
  let inws  = make_wfs_refstore env' rft.Ctypes.sto_in loc in
  let outws = make_wfs_refstore env' rft.Ctypes.sto_out loc in
  Misc.tr_rev_flatten [retws ; argws ; inws ; outws]

let rec make_cs cenv p rct1 rct2 loc =
  let env    = env_of_cilenv cenv in
  let r1, r2 = Misc.map_pair reft_of_refctype (rct1, rct2) in
  [C.make_t env p r1 r2 None]

let make_cs_refldesc env p (sloc1, rd1) (sloc2, rd2) loc =
  let ncrs1  = sloc_binds_of_refldesc sloc1 rd1 in
  let ncrs2  = sloc_binds_of_refldesc sloc2 rd2 in
  let ncrs12 = Misc.join snd ncrs1 ncrs2 |> List.map (fun ((x,_), (y,_)) -> (x,y)) in  
  let env'   = List.map fst ncrs1 |> ce_adds env in
  let subs   = List.map (fun ((n1,_), (n2,_)) -> (n2, n1)) ncrs12 in
  Misc.flap begin fun ((n1, _), (_, cr2)) -> 
      let lhs = t_name env' n1 in
      let rhs = t_subs_names subs cr2 in
      make_cs env' p lhs rhs loc
  end ncrs12

let slocs_of_store st = 
  SLM.fold (fun x _ xs -> x::xs) st []

let make_cs_refstore env p st1 st2 polarity loc =
  let _  = Pretty.printf "make_cs_refstore: pol = %b, st1 = %a, st2 = %a \n"
           polarity Ctypes.d_prestore_addrs st1 Ctypes.d_prestore_addrs st2 in
  let _  = Pretty.printf "st1 = %a \n" d_refstore st1 in
  let _  = Pretty.printf "st2 = %a \n" d_refstore st2 in
  let rv =
  (if polarity then st2 else st1)
  |> slocs_of_store 
  |> Misc.flap begin fun sloc ->
       let lhs = (sloc, refstore_get st1 sloc) in
       let rhs = (sloc, refstore_get st2 sloc) in
       make_cs_refldesc env p lhs rhs loc 
     end in
  let _ = F.printf "make_cs_refstore: %a" (Misc.pprint_many true "\n" (C.print_t None)) rv in
  rv
