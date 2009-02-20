module  P = Ast.Predicate
module SM = Misc.StringMap

module type PROVER = 
  sig
    (* usage: set.valid*.finish *)
    type sort = Int 
              | Bool 
              | Unint of string 
              | Array of sort * sort 
              | Func of sort list
    type tenv = sort SM.t
 
 (* val axiom : tenv -> P.t -> unit *)
    val push:   tenv -> P.t -> unit
    val pop:    unit -> unit
    val valid:  tenv -> P.t -> bool
    val finish: unit -> unit
    val print_stats : Format.formatter -> unit -> unit
  end

(************************************* dsolve theoremProverZ3.ml ***)  
open Format
open Predef

module P = Predicate
module C = Common
module F = Frame
module Le = Lightenv

module Prover : PROVER = 
 struct

    type sort = Int | Array of sort * sort | Bool | Unint of string | Func of sort list

    type decl = Vbl of Path.t | Fun of string * int | Barrier
    type var_ast = Const of Z3.ast | Bound of int * sort
    
    type z3_instance = { 
      c                 : Z3.context;
      tint              : Z3.type_ast;
      tbool             : Z3.type_ast;
      vart              : (decl, var_ast) Hashtbl.t;
      funt              : (decl, Z3.const_decl_ast) Hashtbl.t;
      tydeclt           : (sort, Z3.type_ast) Hashtbl.t;
      mutable vars      : decl list ;
      mutable count     : int;
      mutable bnd       : int;
      mutable frtymap   : (F.t * sort) list;
    }

   (* stats *)
    let nb_z3_push  = ref 0
    let nb_z3_unsat = ref 0
    let nb_z3_pop   = ref 0
    let nb_z3_set   = ref 0

    let fresh =
      let x = ref 0 in
      (fun v -> incr x; (v^(string_of_int !x)))

(***************************************************************************************)
(********************** Typing ************************************************************)
(***************************************************************************************)

    let builtins = [
            ("__tag", Func [Unint "obj"; Int]);
            ("_DIV", Func [Int; Int; Int]);
            ("_IOFB", Func [Bool; Int]);
            ("_BOFI", Func [Int; Bool]);
    ]

    let abs p = F.Fabstract(p, [], Ident.create "", F.empty_refinement)
    let unint = Unint "obj"

    let init_frtymap = [
      (Builtins.uInt, Int); 
      (Builtins.uBool, Bool);
    ]

    let type_to_string t =
      let rec t_rec = function
        | Int -> "int"
        | Bool -> "bool"
        | Unint s -> s
        | Func ts -> "(func " ^ String.concat " " (List.map t_rec ts) ^ ")"
        | Array (s, t) -> "[| " ^ t_rec s ^ "; " ^ t_rec t ^ " |]" in
      t_rec t

    let ast_type_to_string me a =
      Z3.ast_to_string me.c (Z3.type_ast_to_ast me.c a)

    let dump_ast_type me a =
      printf "@[z3%s@]@." 
            (Z3.ast_to_string me.c (Z3.type_ast_to_ast me.c (Z3.get_type me.c a)))

    let dump_ast me a =
      printf "@[%s@]@." (Z3.ast_to_string me.c a)

    let dump_decls me =
      printf "Vars:@.";
      List.iter (function Vbl s -> printf "%s@." (Path.unique_name s) | Barrier -> printf "----@." | _ -> ()) me.vars;
      printf "@."
   
    let rec frame_to_type me = function
      | F.Farrow (_, t1, t2) -> Func (collapse me t1 t2)
      | fr -> snd (List.find (fun (fr', _) -> F.same_shape fr fr') me.frtymap)

    and collapse me t1 t2 =
      (try frame_to_type me t1 with Not_found -> unint)
      :: (match t2 with 
          | F.Farrow (_, t1, t2) -> collapse me t1 t2 
          | _ -> try [frame_to_type me t2] with Not_found -> [unint])
                          
    let rec type_to_frame me = function
      | Func (t :: []) -> type_to_frame me t
      | Func (t :: ts) ->
        (try
          List.assoc t (C.list_assoc_flip me.frtymap)
        with Not_found -> F.Farrow(F.fresh_binder (), type_to_frame me t, type_to_frame me (Func ts)))
      | t -> List.assoc t (C.list_assoc_flip me.frtymap)

    let z3VarType me t =
      C.do_memo me.tydeclt (fun () -> z3VarType me t) () t

    let rec transl_type me = function
      | Parsetree.Pprover_abs s ->
          (match s with 
          | "int" -> Int
          | "bool" -> Bool
          | s -> Unint s)
      | Parsetree.Pprover_array (t, t') ->
          Array (transl_type me t, transl_type me t')
      | Parsetree.Pprover_fun ts ->
          Func (List.map (transl_type me) ts)

    let z3ArgTypes me = function 
      | Func ts -> (match (List.rev_map (z3VarType me) ts) with
                      | x :: [] -> ([], x)
                      | x :: xs -> (List.rev xs, x)
                      | [] -> assert false)
      | _ -> assert false

    let getVarType me s env =
      let fr = try (Le.find s env) with
                 Not_found -> let p = Path.unique_name s in
                   (eprintf "@[Warning:@ type@ of@ %s@ not@ found@ at@ TP@]@." p; Builtins.uUnit) in
      try frame_to_type me fr
        with Not_found -> unint
 
    let getFunType me s env =
      if C.has_prefix "SELECT_" then Func [Int; Int] else 
        try List.assoc s builtins with Not_found -> 
          try frame_to_type me (F.find_by_name env s) with Not_found -> 
            let _ = printf "@[Warning: Untyped function %s in tpz3@]" s in
            unint

(***************************************************************************************)
(********************** Vars ***********************************************************)
(***************************************************************************************)

    let z3Var_memo env me s =
      Misc.do_memo me.vart
      (fun () -> 
        let t = getVarType me s env in
        let sym = Z3.mk_string_symbol me.c (fresh "z3v") in
        let rv = Const (Z3.mk_const me.c sym (z3VarType me t)) in
        me.vars <- (Vbl s)::me.vars; rv) 
      () (Vbl s)

    let z3Var env me s =
      match z3Var_memo env me s with
          Const v -> v
        | Bound (b, t) -> Z3.mk_bound me.c (me.bnd - b) (z3VarType me t)

    let z3Bind me p t =
      me.bnd <- me.bnd + 1; Hashtbl.replace me.vart (Vbl p) (Bound (me.bnd, t)); me.vars <- (Vbl p) :: me.vars;
      Z3.mk_string_symbol me.c (fresh "z3b")

(***************************************************************************************)
(********************** Funs ***********************************************************)
(***************************************************************************************)

    let z3Fun env me s k = 
      Misc.do_memo me.funt
      (fun () ->
        let t   = getFunType me s env in
        let sym = Z3.mk_string_symbol me.c (fresh "z3f") in
        let (ts, ret) = z3ArgTypes me t in
        let rv  = Z3.mk_func_decl me.c sym (Array.of_list ts) ret in
        me.vars <- (Fun (s,k))::me.vars; rv) 
      () (Fun (s,k))

    let rec cast env me ast (t, t') =
      if (t, t') = ("bool", "int") then z3App env me "_IOFB" [ast] else
      if (t, t') = ("int", "bool") then z3App env me "_BOFI" [ast] else
        assert false

    and z3Cast env me = function
      | (a :: sa, f :: fs) -> 
        let (t, t') = (Z3.get_type me.c a, z3VarType me f) in
        let (st, st') = C.app_pr (ast_type_to_string me) (t, t') in
        let t = if st = st' then a else (cast env me a (st, st')) in
          t :: (z3Cast env me (sa, fs))
      | ([], x :: []) -> []
      | ([], []) -> []
      | _ -> assert false

    and z3App env me s zes =
      let k   = List.length zes in
      let cf  = z3Fun env me s k in
      let ft  = match getFunType me s env with Func ts -> ts | _ -> assert false in
      let zes = z3Cast env me (zes, ft) in
      Z3.mk_app me.c cf (Array.of_list zes)

    and z3AppBuiltin env me fes aes =
      z3Cast env me (List.map (z3Exp env me) aes, fes) 

    and z3AppRel env me mk e1 e2 =
      match z3AppBuiltin env me [Int; Int] [e1; e2] with
        [a1; a2] -> mk me.c a1 a2
      | _ -> assert false

(***************************************************************************************)
(********************** Pred/Expr Transl ************************************************************)
(***************************************************************************************)

    and z3Exp env me e =
      match e with 
      | P.PInt i                -> Z3.mk_int me.c i me.tint 
      | P.Var s                 -> z3Var env me s
      | P.FunApp (f,es)         -> z3App env me f (List.map (z3Exp env me) es)
      | P.Binop (e1,P.Plus,e2)  ->
          Z3.mk_add me.c (Array.map (z3Exp env me) [|e1; e2|])
      | P.Binop (e1,P.Minus,e2) ->
          Z3.mk_sub me.c (Array.map (z3Exp env me) [|e1; e2|])
      | P.Binop (e1,P.Times,e2) ->
          Z3.mk_mul me.c (Array.map (z3Exp env me) [|e1; e2|])
      | P.Binop (e1,P.Div,e2)   -> z3App env me "_DIV" (List.map (z3Exp env me) [e1;e2])  
      | P.Field (f, e)          -> z3App env me ("SELECT_"^(Ident.unique_name f)) [(z3Exp env me e)] 
                                   (** REQUIRES: disjoint intra-module field names *)
      | P.Ite (e1, e2, e3)      -> Z3.mk_ite me.c (z3Pred env me e1) (z3Exp env me e2) (z3Exp env me e3)

    and z3Pred env me p = 
     try
      match p with 
        P.True          -> Z3.mk_true me.c
      | P.Not p' -> Z3.mk_not me.c (z3Pred env me p')
      | P.And (p1,p2) -> Z3.mk_and me.c (Array.map (z3Pred env me) [|p1;p2|])
      | P.Or (p1,p2) -> Z3.mk_or me.c (Array.map (z3Pred env me) [|p1;p2|])
      | P.Implies (p1, p2) -> Z3.mk_implies me.c (z3Pred env me p1) (z3Pred env me p2)
      | P.Iff (p, q) -> Z3.mk_iff me.c (z3Pred env me p) (z3Pred env me q)
   (* | P.Atom (e1,P.Lt,e2) -> z3Pred me (Atom (e1, P.Le, Binop(e2,P.Minus,PInt 1))) *)
      | P.Atom (e1,P.Eq,e2) ->
          (*Z3.mk_eq me.c (z3Exp env me e1) (z3Exp env me e2)*)
          z3AppRel env me Z3.mk_eq e1 e2
      | P.Atom (e1,P.Ne,e2) ->
          Z3.mk_distinct me.c (Array.of_list (z3AppBuiltin env me [Int; Int] [e1; e2]))
      | P.Atom (e1,P.Gt,e2) ->
          (*Z3.mk_gt me.c (z3Exp env me e1) (z3Exp env me e2)*)
          z3AppRel env me Z3.mk_gt e1 e2
      | P.Atom (e1,P.Ge,e2) ->
          (*Z3.mk_ge me.c (z3Exp env me e1) (z3Exp env me e2)*)
          z3AppRel env me Z3.mk_ge e1 e2
      | P.Atom (e1,P.Lt,e2) ->
          (*Z3.mk_lt me.c (z3Exp env me e1) (z3Exp env me e2)*)
          z3AppRel env me Z3.mk_lt e1 e2
      | P.Atom (e1,P.Le,e2) ->
          (*Z3.mk_le me.c (z3Exp env me e1) (z3Exp env me e2)*)
          z3AppRel env me Z3.mk_le e1 e2
      | P.Forall (ps, q) -> 
          let (ps, ss) = List.split ps in
          let ts = List.map (transl_type me) ss in
          mk_quantifier Z3.mk_forall env me ps ts q
      | P.Exists (ps, q) ->
          let (ps, ss) = List.split ps in
          let ts = List.map (transl_type me) ss in
          mk_quantifier Z3.mk_exists env me ps ts q
      | P.Boolexp e -> 
          (* shady hack *)
          let a = z3Exp env me e in
          let t = Z3.get_type me.c a in
          let t = ast_type_to_string me t in
          if t = "int" then cast env me a ("int", "bool") else a
     with Failure s -> printf "%s: %a@." s P.pprint p; raise (Failure s)

    and mk_quantifier mk env me ps ts q =
      let args = qargs me ps ts in
      let rv = mk me.c 0 [||] (qtypes me ts) args (z3Pred env me q) in
      me.bnd <- me.bnd - (List.length ps); rv

    and qargs me ps ts = 
      Array.of_list (List.map (fun (p, t) -> z3Bind me p t) (List.combine ps ts))

    and qtypes me ts = Array.of_list (List.map (z3VarType me) ts)  

    let z3Preds env me ps =
      let ps' = List.map (z3Pred env me) ps in
      Z3.mk_and me.c (Array.of_list ps')

(***************************************************************************************)
(********************** Low Level Interface ************************************************************)
(***************************************************************************************)

    let unsat me =
      let _ = incr nb_z3_unsat in
      let rv = (Bstats.time "Z3 unsat" Z3.check me.c) = Z3.L_FALSE in
      rv

    let assert_axiom me p =
      let _ = Bstats.time "Z3 assert" (Z3.assert_cnstr me.c) p in
      let _ = Common.cprintf Common.ol_axioms "@[%s@]@." (Z3.ast_to_string me.c p) in
        if unsat me then failwith "Background theory is inconsistent!"

    let rec vpop (cs,s) =
      match s with 
      | [] -> (cs,s)
      | Barrier :: t -> (cs,t)
      | h :: t -> vpop (h::cs,t) 

    let remove_decl me d = 
      match d with 
      | Barrier -> Common.asserts "TheoremProverZ3.remove_decl" false
      | Vbl _ -> Hashtbl.remove me.vart d 
      | Fun _ -> Hashtbl.remove me.funt d

    let prep_preds env me ps =
      let ps = List.rev_map (z3Pred env me) ps in
      let _  = me.vars <- Barrier :: me.vars in
      let _  = Z3.push me.c in
        ps

    let push me ps =
      let _ = incr nb_z3_push in
      let _ = me.count <- me.count + 1 in
      let _  = Z3.push me.c in
        List.iter (fun p -> Z3.assert_cnstr me.c p) ps

    let pop me =
      let _ = incr nb_z3_pop in
      let _ = me.count <- me.count - 1 in
        Z3.pop me.c 1 

    let valid me p =
      let _ = push me [(Z3.mk_not me.c p)] in
      let rv = unsat me in
      let _ = pop me in
        rv

    let me = 
      let c = Z3.mk_context_x [|("MODEL", "false"); ("PARTIAL_MODELS", "true")|] in
      (* types *)
      let tint = Z3.mk_int_type c in
      let tbool = Z3.mk_bool_type c in
      (* memo tables *)
      let vart = Hashtbl.create 37 in
      let funt = Hashtbl.create 37 in
      let tydeclt = Hashtbl.create 37 in
      { c = c; tint = tint; tbool = tbool; tydeclt = tydeclt; frtymap = init_frtymap;
        vart = vart; funt = funt; vars = []; count = 0; bnd = 0}

(***************************************************************************************)
(********************** API ************************************************************)
(***************************************************************************************)

    let clean_decls () =
      let (cs,vars') = vpop ([],me.vars) in
      let _ = me.vars <- vars' in
      let _ = List.iter (remove_decl me) cs in ()

    let set env ps =
      let _   = Hashtbl.remove me.vart (Vbl C.qual_test_var) in
      let ps  = prep_preds env me ps in
      let _   = push me ps in
        unsat me

    let filter env ps =
      let rv =
        let ps = List.rev_map (fun (q, p) -> (z3Pred env me p, (q, p))) ps in
        List.filter (fun (p, _) -> valid me p) ps in
      let _ = pop me in
      let _ = clean_decls () in
        snd (List.split rv)

    let finish () =
      pop me

    let axiom env p =
      assert_axiom me (z3Pred env me p)

    let embed_type (fr, t) =
      me.frtymap <- (fr, transl_type me t) :: me.frtymap

    let frame_of pt =
      try
        type_to_frame me (transl_type me pt)
      with Not_found -> raise (Failure (C.prover_t_to_s pt))

    let print_stats ppf () = 
      Format.fprintf ppf "@[implies(API):@ %i,@ Z3@ {pushes:@ %i,@ pops:@ %i,@ unsats:@ %i}@]"
      !nb_z3_set !nb_z3_push !nb_z3_pop !nb_z3_unsat

    let _ = 
      let (x, y) = C.app_pr Path.mk_ident ("x", "y") in
      let bol = Parsetree.Pprover_abs "bool" in
      let itn = Parsetree.Pprover_abs "int" in
      let func s x = P.FunApp(s, [P.Var x]) in
        axiom Le.empty
          (P.Forall ([(x, bol)], P.Iff(P.Atom(func "_IOFB" x, P.Eq, P.PInt(1)), P.Boolexp(P.Var x))));
        axiom Le.empty 
          (P.Forall ([(x, itn)], P.Iff(P.Boolexp(func "_BOFI" x), P.Atom(P.Var x, P.Eq, P.PInt(1)))));
end
   

(************************************* dsolve theoremProver.ml *****)  
(* Common theorem prover interface *)
module P = Predicate
module C = Common
module Cl = Clflags
module Prover = TheoremProverZ3.Prover
module BS = Bstats

(********************************************************************************)
(************************** Rationalizing Division ******************************)
(********************************************************************************)

let rec fixdiv p = 
  let expr_isdiv = 
    function P.Binop(_, P.Div, _) -> true
      | _ -> false in 
  let pull_const =
    function P.PInt(i) -> i
      | _ -> 1 in
  let pull_divisor =
    function P.Binop(_, P.Div, d1) ->
      pull_const d1 
      | _ -> 1 in
  let rec apply_mult m e =
    match e with
        P.Binop(n, P.Div, P.PInt(d)) ->
          (*let _ = assert ((m/d) * d = m) in*)
            P.Binop(P.PInt(m/d), P.Times, n) 
      | P.Binop(e1, rel, e2) ->
          P.Binop(apply_mult m e1, rel, apply_mult m e2) 
      | P.PInt(i) -> P.PInt(i*m)
      | e -> P.Binop(P.PInt(m), P.Times, e)
  in
  let rec pred_isdiv = 
    function P.Atom(e, _, e') -> (expr_isdiv e) || (expr_isdiv e')
      | P.Iff (p, q) -> pred_isdiv p || pred_isdiv q
      | P.And(p, p') -> (pred_isdiv p) || (pred_isdiv p')
      | P.Or(p, p') -> (pred_isdiv p) || (pred_isdiv p')
      | P.Implies (p, q) -> (pred_isdiv p) || (pred_isdiv q)
      | P.True -> false
      | P.Not p -> pred_isdiv p
      | P.Forall (_, q) | P.Exists (_, q) -> pred_isdiv q 
      | P.Boolexp e -> expr_isdiv e in
  let calc_cm e1 e2 =
    pull_divisor e1 * pull_divisor e2 in
  if pred_isdiv p then
     match p with
       P.Atom(e, r, e') -> 
         let m = calc_cm e e' in
         let e'' = P.Binop(e', P.Minus, P.PInt(1)) in
         let bound (e, r, e', e'') = 
           P.And(P.Atom(apply_mult m e, P.Gt, apply_mult m e''),
                 P.Atom(apply_mult m e, P.Le, apply_mult m e')) in
           (match (e, r, e') with
                (P.Var v, P.Eq, e') ->
                  bound (e, r, e', e'')
              | (P.PInt v, P.Eq, e') ->
                  bound (e, r, e', e'')
              | _ -> p) 
     | P.And(p1, p2) -> 
         let p1 = if pred_isdiv p1 then fixdiv p1 else p1 in
         let p2 = if pred_isdiv p2 then fixdiv p2 else p2 in
           P.And(p1, p2)      
     | P.Or(p1, p2) ->
         let p1 = if pred_isdiv p1 then fixdiv p1 else p1 in
         let p2 = if pred_isdiv p2 then fixdiv p2 else p2 in
           P.Or(p1, p2) 
     | P.Implies(p1, p2) ->
         let p1 = if pred_isdiv p1 then fixdiv p1 else p1 in
         let p2 = if pred_isdiv p2 then fixdiv p2 else p2 in
           P.Implies(p1, p2)
     | P.Not p1 -> P.Not(fixdiv p1) 
     | p -> p
    else p

(********************************************************************************)
(*********************** Memo tables and Stats Counters  ************************)
(********************************************************************************)

let nb_push = ref 0
let nb_queries = ref 0
let nb_cache_misses = ref 0
let nb_cache_hits = ref 0
let nb_qp_miss = ref 0
let qcachet: (string * string, bool) Hashtbl.t = Hashtbl.create 1009 
let buftlhs = Buffer.create 300
let buftrhs = Buffer.create 300
let lhsform = Format.formatter_of_buffer buftlhs
let rhsform = Format.formatter_of_buffer buftrhs
(*let qcachet: (P.t * P.t, bool) Hashtbl.t = Hashtbl.create 1009*)


(********************************************************************************)
(************************************* AXIOMS ***********************************)
(********************************************************************************)

let push_axiom env p =
  C.cprintf C.ol_axioms "@[Pushing@ axiom:@ %a@]@." P.pprint p; Prover.axiom env p

(********************************************************************************)
(************************************* API **************************************)
(********************************************************************************)

(* API *)
let print_stats ppf () =
  C.fcprintf ppf C.ol_solve_stats "@[TP@ API@ stats:@ %d@ pushes@ %d@ queries@ cache@ %d@ hits@ %d@ misses@]@." !nb_push !nb_queries !nb_cache_hits !nb_cache_misses;
  C.fcprintf ppf C.ol_solve_stats "@[Prover @ TP@ stats:@ %a@]@." Prover.print_stats ()

(* API *)
let reset () =
  Hashtbl.clear qcachet; 
  nb_push  := 0;
  nb_queries := 0; 
  nb_cache_misses := 0;
  nb_cache_hits := 0;
  nb_qp_miss := 0

let is_not_taut p =
  not (P.is_taut p)

let set_and_filter env ps qs =
  let _ = incr nb_push in
  let _ = nb_queries := !nb_queries + (List.length ps) in
  let ps = List.rev_map fixdiv ps in
  let ps = BS.time "TP taut" (List.filter is_not_taut) ps in
  if BS.time "TP set" (Prover.set env) ps then (Prover.finish (); qs) else
      let qs = List.rev_map (C.app_snd fixdiv) qs in
      let (qs, qs') = List.partition (fun (_, q) -> is_not_taut q) qs in
        List.rev_append qs' (BS.time "TP filter" (Prover.filter env) qs)

