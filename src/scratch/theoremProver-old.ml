module  E = Ast.Expression
module  P = Ast.Predicate
module SM = Misc.StringMap
open Misc.Ops 

type sort = Int 
          | Array of sort * sort 
          | Bool 
          | Unint of string 
          | Func of sort list * sort

type decl = Vbl of string 
          | Fun of string * int 
          | Barrier

type vast = Const of Z3.ast 
          | Bound of int * sort

(* stats ********************************************************************)

let nb_z3_push  = ref 0
let nb_z3_unsat = ref 0
let nb_z3_pop   = ref 0
let nb_z3_set   = ref 0

let nb_push = ref 0
let nb_queries = ref 0
let nb_cache_misses = ref 0
let nb_cache_hits = ref 0
let nb_qp_miss = ref 0
let qcachet: (string * string, bool) Hashtbl.t = Hashtbl.create 1009 

let d_stats ppf () =
  Pretty.dprintf "TBD: TP.d_stats"

(* utils *******************************************************************)
let builtins = [("__tag", Func ([Unint "obj"], Int)); 
                ("_DIV",  Func ([Int; Int], Int));
                ("_IOFB", Func ([Bool], Int));
                ("_BOFI", Func ([Int],  Bool))]

let unint = Unint "obj"

let fresh =
  let x = ref 0 in
  fun v -> 
    let _ = incr x in
    v^(string_of_int !x)

let ast_type_to_string ctx a =
  Z3.type_ast_to_ast ctx a |> Z3.ast_to_string ctx

let mk_rel = function
  | Cil.Eq -> Z3.mk_eq
  | Cil.Gt -> Z3.mk_gt
  | Cil.Ge -> Z3.mk_ge 
  | Cil.Lt -> Z3.mk_lt
  | Cil.Le -> Z3.mk_le 
  | Cil.Ne -> fun ctx a1 a2 -> Z3.mk_distinct ctx [|a1; a2|]
  | op     -> pretty_string Cil.d_binop op |> failure "ERROR: mk_rel %s"

let splitFunType = function
  | Func (ts, t) -> (ts, t)
  | _            -> failure "ERROR: splitFunType in TPZ3"
   

let rec vpop (cs,s) =
  match s with 
  | []           -> (cs, s)
  | Barrier :: t -> (cs, t)
  | h :: t       -> vpop (h::cs, t) 

(* class-definition *********************************************************)

class ['a] prover env = 
  let ctx : Z3.context                          = Z3.mk_context_x 
                                                  [|("MODEL", "false"); 
                                                    ("PARTIAL_MODELS", "true")|] in
  let tint: Z3.type_ast                         = Z3.mk_int_type ctx  in
  let tboo: Z3.type_ast                         = Z3.mk_bool_type ctx in
  let tydt: (sort, Z3.type_ast) Hashtbl.t       = Hashtbl.create 37 in
  let vart: (decl, vast) Hashtbl.t              = Hashtbl.create 37 in
  let funt: (decl, Z3.const_decl_ast) Hashtbl.t = Hashtbl.create 37 in 
  let zero: Z3.ast                              = Z3.mk_int ctx 0 tint in
  object(self) 
    val decs: decl list ref                     = ref [] 
    val cnt : int ref                           = ref 0 
    val bnd : int ref                           = ref 0 

(* type env ******************************************************************)

  method private getFunType s =
    (if Misc.is_prefix "SELECT_" s then Func ([Int], Int) else 
      try List.assoc s builtins with Not_found -> 
        try SM.find s env with Not_found -> 
          failure "ERROR: Untyped function %s in TPZ3" s)
    |> splitFunType 
  method private getVarType s =
    try SM.find s env with Not_found ->
      failure "ERROR: Untyped variable %s in TPZ3" s 

(* types *********************************************************************)
  method private z3TypeAst = function
    | Int     -> tint
    | Bool    -> tboo
    | Unint _ -> tint
    | Func _  -> tint (* self#z3TypeAst (Unint ("fun")) *)
    | Array _ -> failure "ERROR: z3TypeAst"

  method private z3ArgTypes ts t =
    (List.map self#z3TypeAst ts, self#z3TypeAst t)
 
  method private z3Cast a f = 
    let (t, t') = (Z3.get_type ctx a, self#z3TypeAst f) in
    let (s, s') = Misc.map_pair (ast_type_to_string ctx) (t, t') in
    match (s, s') with
    | _   when s = s' -> a
    | ("bool", "int") -> self#z3App "_IOFB" [a] 
    | ("int", "bool") -> self#z3App "_BOFI" [a]
    | _               -> assert false

(* vars *********************************************************************)
  method private z3Var_memo s =
    Misc.do_memo vart
      (fun () -> 
        let t   = self#getVarType s in
        let sym = Z3.mk_string_symbol ctx (fresh "z3v") in
        let rv  = Const (Z3.mk_const ctx sym (self#z3TypeAst t)) in
        let _   = decs := (Vbl s)::!decs in rv) 
      () (Vbl s)
    
  method private z3Var s =
    match self#z3Var_memo s with
    | Const v      -> v
    | Bound (b, t) -> Z3.mk_bound ctx (!bnd - b) (self#z3TypeAst t)

  method private z3Bind p t =
    incr bnd; 
    Hashtbl.replace vart (Vbl p) (Bound (!bnd, t)); 
    decs := (Vbl p) :: !decs;
    Z3.mk_string_symbol ctx (fresh "z3b")

  method private z3Fun s k = 
    Misc.do_memo funt
    (fun () ->
      let (ts, t) = self#getFunType s in
      let (ts, t) = self#z3ArgTypes ts t in
      let _       = assert (List.length ts = k) in
      let sym     = Z3.mk_string_symbol ctx (fresh "z3f") in
      let rv      = Z3.mk_func_decl ctx sym (Array.of_list ts) t in
      let _       = decs := (Fun (s,k))::!decs in rv) 
    () (Fun (s,k))
      
(* fun-app ***********************************************************)
  
  method private z3App s args =
    let ts,_ = self#getFunType s in
    let args = List.map2 self#z3Cast args ts in
    let k    = List.length args in
    Z3.mk_app ctx (self#z3Fun s k) (Array.of_list args)

  method private z3AppBuiltin es ts =
    let args = List.map self#z3Exp es in
    List.map2 self#z3Cast args ts 
   
  method private z3AppRel mk e1 e2 =
    let a1 = self#z3Cast (self#z3Exp e1) Int in
    let a2 = self#z3Cast (self#z3Exp e2) Int in
    mk ctx a1 a2

(* expr *************************************************************)

  method private z3Exp = function
    | Cil.Lval ((Cil.Var v), Cil.NoOffset) -> 
        self#z3Var (v.Cil.vname)
    | Cil.UnOp (Cil.Neg, e, _) ->
        Z3.mk_sub ctx [|zero; (self#z3Exp e) |]
    | Cil.BinOp (Cil.PlusA, e1, e2, _) ->
        Z3.mk_add ctx (Array.map self#z3Exp [|e1; e2|])
    | Cil.BinOp (Cil.MinusA, e1, e2,_) ->
        Z3.mk_sub ctx (Array.map self#z3Exp [|e1; e2|])
    | Cil.BinOp (Cil.Mult, e1, e2, _) ->
        Z3.mk_mul ctx (Array.map self#z3Exp [|e1; e2|])
    | Cil.BinOp (Cil.Div, e1, e2, _) ->
        self#z3App "_DIV" (List.map self#z3Exp [e1;e2])  
    | Cil.Lval ((Cil.Var v), Cil.Field (f, Cil.NoOffset)) -> 
        self#z3App ("SELECT_"^f.Cil.fname) [self#z3Var v.Cil.vname]
    | e -> 
        pretty_string Cil.d_exp e |> failure "ERROR: z3Exp %s"

(* {{{  | P.Ite (e1, e2, e3) -> 
        Z3.mk_ite ctx (self#z3Pred env e1) (self#z3Exp env e2) (self#z3Exp env e3) }}} *)

(* pred *************************************************************)
   
  method private z3Pred = function 
    | Cil.UnOp (Cil.LNot, p, _) ->
        Z3.mk_not ctx (self#z3Pred p)
    | Cil.BinOp (Cil.LAnd, p1, p2, _) ->
        Z3.mk_and ctx (Array.map self#z3Pred [|p1;p2|])
    | Cil.BinOp (Cil.LOr, p1, p2, _) ->
        Z3.mk_or  ctx (Array.map self#z3Pred [|p1;p2|])
    | Cil.BinOp (op, e1, e2, _) ->
        self#z3AppRel (mk_rel op) e1 e2
    | p -> 
        pretty_string Cil.d_exp p |> failure "ERROR: z3Pred %s"
        
(* {{{ 
    | P.True -> 
        Z3.mk_true ctx 
    | P.Implies (p1, p2) -> 
        Z3.mk_implies ctx (self#z3Pred env p1) (self#z3Pred env p2)
    | P.Iff (p, q) -> 
        Z3.mk_iff ctx (self#z3Pred env p) (self#z3Pred env q)
    | P.Boolexp e -> (* shady hack *)
        self#z3Cast env (self#z3Exp env e) Bool 
    | P.Forall (ps, q) -> 
        let (ps, ss) = List.split ps in
        let ts       = List.map (transl_type me) ss in
        mk_quantifier Z3.mk_forall env me ps ts q
     | P.Exists (ps, q) ->
        let (ps, ss) = List.split ps in
        let ts = List.map (transl_type me) ss in
        mk_quantifier Z3.mk_exists env me ps ts q 
     
    and mk_quantifier mk env me ps ts p =
      let args = Array.of_list (List.map2 self#z3Bind ps ts) in
      let qts  = Array.of_list (List.map self#z3TypeAst ts) in
      let a    = self#z3Pred env p in
      let rv   = mk ctx 0 [||] qts args a in
      let _    = bnd := !bnd - (List.length ps) in 
      rv

    let axiom env p =
      let p = self#z3Pred env p in 
      let _ = Z3.assert_cnstr ctx p in
      let _ = Printf.printf "%s \n" (Z3.ast_to_string ctx p) in
      if self#unsat () then failwith "Background theory is inconsistent!")

    let _ = 
      let (x, y) = C.app_pr Path.mk_ident ("x", "y") in
      let bol = Parsetree.Pprover_abs "bool" in
      let itn = Parsetree.Pprover_abs "int" in
      let func s x = P.FunApp(s, [P.Var x]) in
        axiom Le.empty
          (P.Forall ([(x, bol)], P.Iff(P.Atom(func "_IOFB" x, P.Eq, P.PInt(1)), P.Boolexp(P.Var x))));
        axiom Le.empty 
          (P.Forall ([(x, itn)], P.Iff(P.Boolexp(func "_BOFI" x), P.Atom(P.Var x, P.Eq, P.PInt(1)))));
}}} *)

(* low-level *****************************************************************)
  method private clean_decs () =
    let (cs, decs') = vpop ([], !decs) in
    decs := decs';
    List.iter 
      (function | Vbl _ as d -> Hashtbl.remove vart d
                | Fun _ as d -> Hashtbl.remove funt d
                | _          -> failure "ERROR: TP.clean_decs")
      cs


  method private unsat () = 
    incr nb_z3_unsat; 
    ((* Bstats.time "Z3 unsat" *) Z3.check ctx) = Z3.L_FALSE

  method private push ps =
    incr nb_z3_push;
    incr cnt;
    Z3.push ctx;
    List.iter (Z3.assert_cnstr ctx) ps

  method private pop () =
    incr nb_z3_pop; 
    decr cnt;
    Z3.pop ctx 1 

  method private valid p =
    let _  = self#push [(Z3.mk_not ctx p)] in
    let rv = self#unsat () in
    let _  = self#pop () in
    rv
    
(* api **********************************************************************)
  
    (* {{{      
  method set ps =
    (* let _  = Hashtbl.remove vart (Vbl C.qual_test_var) in *)
    let ps = List.rev_map self#z3Pred ps in
    let _  = decs := Barrier :: !decs; Z3.push ctx in
    let _  = self#push ps in
    self#unsat () 

  method filter (xps : ('a * P.t) list) =
    let xps = List.rev_map (fun x -> (x, self#z3Pred (snd x))) xps in 
    let rv  = List.filter (fun (_, p) -> self#valid p) xps in
    let _   = self#pop (); self#clean_decs () in
    fst (List.split rv) }}} *)

  method set_and_filter ps (xqs : ('a * P.t) list) = 
(* let _  = Hashtbl.remove vart (Vbl C.qual_test_var) in *)
    let ps = List.rev_map self#z3Pred ps in
    let _  = decs := Barrier :: !decs; Z3.push ctx in
    let _  = self#push ps in
    if self#unsat () then xqs else 
      let xas = List.rev_map (fun x -> (x, self#z3Pred (snd x))) xqs in 
      let rv  = List.filter (fun (_, p) -> self#valid p) xas in
      let _   = self#pop (); self#clean_decs () in
      fst (List.split rv)
end

let embed_type = function
  | Cil.TInt _ -> Int
  | _          -> failure "ERROR: embed_type in TPZ3" (* Unint "obj" *)
