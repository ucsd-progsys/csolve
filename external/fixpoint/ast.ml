(*
 * Copyright © 2009 The Regents of the University of California. All rights reserved. 
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

(**
 * This module implements a DAG representation for expressions and 
 * predicates: each sub-predicate or sub-expression is paired with
 * a unique int ID, which enables constant time hashing. 
 * However, one must take care when using DAGS:
 * (1) they can only be constructed using the appropriate functions
 * (2) when destructed via pattern-matching, one must discard the ID
 *)

open Misc.Ops

module Sort = 
  struct
    type t = 
      | Int 
      | Bool 
      | Ptr
      | Array of t * t 
      | Unint of string 
      | Func of t list

    let rec to_string = function
      | Int     -> "int"
      | Ptr     -> "ptr" 
      | Bool    -> "bool"
      | Unint s -> "uit "^s
      | Func ts -> Printf.sprintf "func([%s])" (ts |> List.map to_string |> String.concat " ; ")
      | Array _ -> failwith "TBD: Sort.to_string"

    let to_string_short = function
      | Int     -> "int"
      | Ptr     -> "ptr"
      | Bool    -> "bool"
      | Unint s -> "uit "^s
      | Func ts -> "func"
      | Array _ -> failwith "TBD: Sort.to_string"


    let print fmt t = 
      to_string t |> Format.fprintf fmt "%s"
  end

module Symbol = 
  struct 
    type t = string
    
    module SMap = Map.Make (struct type t = string 
                                   let compare i1 i2 = compare i1 i2 end)

    module SSet = Set.Make (struct type t = string
                                   let compare i1 i2 = compare i1 i2 end)

    let is_wild s = 
      if s = "" then false else 
        (s.[0] = '~' || s.[0] = '@')

    let is_safe s = 
      (* let re = Str.regexp "[a-zA-Z][a-z A-Z 0-9 _]*" in *)
      let re = Str.regexp "[A-Za-z '~' '_' '\'' '@' ][0-9 a-z A-Z '_' '@' '\'' '.' '#']*$" in
      Str.string_match re s 0
    
    let of_string, to_string = 
      let of_t = Hashtbl.create 117 in
      let to_t = Hashtbl.create 117 in
      let bind = fun s sy -> Hashtbl.replace of_t s sy; Hashtbl.replace to_t sy s in
      let f,_  = Misc.mk_string_factory "FIXPOINTSYMBOL_" in
      ((fun s -> 
         if is_safe s then s else
           try Hashtbl.find of_t s with Not_found ->
             let sy = f () in
             let _  = bind s sy in sy),
       (fun sy -> try Hashtbl.find to_t sy with Not_found -> sy))
                   
    let to_string = fun s -> s (* if is_safe s then s else "'" ^ s ^ "'" *)

    let print fmt s =
      to_string s |> Format.fprintf fmt "%s" 

    let value_variable t = 
      "VV_"^(Sort.to_string_short t)
 
    let sm_length m = 
      SMap.fold (fun _ _ i -> i+1) m 0

    let sm_filter f sm = 
      SMap.fold begin fun x y sm -> 
        if f x y then SMap.add x y sm else sm 
    end sm SMap.empty 

    let sm_to_list sm =
      SMap.fold (fun x y acc -> (x,y)::acc) sm []
  
  end

module Constant =
  struct
    type t = Int of int

    let to_string = function
      | Int i -> string_of_int i


    let print fmt s =
      to_string s |> Format.fprintf fmt "%s"
  end
 

type tag = int

type brel = Eq | Ne | Gt | Ge | Lt | Le 

type bop  = Plus | Minus | Times | Div
    
type expr = expr_int * tag 
    
and expr_int =
  | Con of Constant.t
  | Var of Symbol.t
  | App of Symbol.t * expr list
  | Bin of expr * bop * expr  
  | Ite of pred * expr * expr
  | Fld of Symbol.t * expr             (* NOTE: Fld (s, e) == App ("field"^s,[e]) *) 
      
and pred = pred_int * tag

and pred_int =
  | True
  | False
  | And  of pred list
  | Or   of pred list
  | Not  of pred
  | Imp  of pred * pred
  | Bexp of expr
  | Atom of expr * brel * expr 
  | Forall of ((Symbol.t * Sort.t) list) * pred

let list_hash b xs = 
  List.fold_left (fun v (_,id) -> 2*v + id) b xs

module Hashcons (X : sig type t 
                         val sub_equal : t -> t -> bool 
                         val hash : t -> int end) = struct

  module HashStruct = struct
    type t = X.t * int
    let equal (x,_) (y,_) = X.sub_equal x y
    let hash (x,_) = X.hash x
  end

  module Hash = Weak.Make(HashStruct)
  
  let wrap = 
    let tab = Hash.create 251 in
    let ctr = ref 0 in
    fun e -> 
      let res = Hash.merge tab (e, !ctr) in
      let _   = if snd res = !ctr then incr ctr in
      res

  let unwrap (e,_) = e

end

module ExprHashconsStruct = struct
  type t = expr_int
  let sub_equal e1 e2 =
    match e1, e2 with
      | Con c1, Con c2 -> 
          c1 = c2
      | Var x1, Var x2 -> 
          x1 = x2
      | App (s1, e1s), App (s2, e2s) ->
	  (s1 = s2) && (try List.for_all2 (==) e1s e2s with _ -> false)
      | Bin (e1, op1, e1'), Bin (e2, op2, e2') ->
          op1 = op2 && e1 == e2 && e1' == e2'
      | Ite (ip1,te1,ee1), Ite (ip2,te2,ee2) ->
	  ip1 == ip2 && te1 == te2 && ee1 == ee2
      | Fld (s1, e1), Fld (s2, e2) ->
          s1 = s2 && e1 == e2
      | _ -> 
          false
  
  let hash = function
    | Con (Constant.Int x) -> 
        x
    | Var x -> 
        Hashtbl.hash x
    | App (s, es) -> 
        list_hash ((Hashtbl.hash s) + 1) es 
    | Bin ((_,id1), op, (_,id2)) -> 
        (Hashtbl.hash op) + 1 + (2 * id1) + id2 
    | Ite ((_,id1), (_,id2), (_,id3)) -> 
        32 + (4 * id1) + (2 * id2) + id3
    | Fld (s, (_,id)) ->
        (Hashtbl.hash s) + 12 + id
end
  
module ExprHashcons = Hashcons(ExprHashconsStruct)

module PredHashconsStruct = struct
  
  type t = pred_int
  
  let sub_equal p1 p2 =
    match p1, p2 with
      | True, True | False, False -> 
          true
      | And p1s, And p2s  | Or  p1s, Or p2s -> 
          (try List.for_all2 (==) p1s p2s with _ -> false)
      | Not p1, Not p2 -> 
          p1 == p2
      | Imp (p1, p1'), Imp (p2, p2') -> 
          p1 == p2 && p1' == p2'
      | Bexp e1, Bexp e2 -> 
          e1 == e2
      | Atom (e1, r1, e1'), Atom (e2, r2, e2') ->
          r1 = r2 && e1 == e2 && e1' == e2'
      | Forall(q1s,p1), Forall(q2s,p2) -> 
          q1s = q2s && p1 == p2
      | _ -> 
          false
 
 let hash = function
   | True -> 
       0
   | False -> 
       1
   | And ps -> 
       list_hash 2 ps
   | Or ps -> 
       list_hash 3 ps
   | Not (_,id) -> 
       8 + id 
   | Imp ((_,id1), (_,id2)) ->
       20 + (2 * id1) + id2
   | Bexp (_, id) ->
       32 + id
   | Atom ((_,id1), r, (_,id2)) ->
       36 + (Hashtbl.hash r) + (2 * id1) + id2
   | Forall(qs,(_,id)) -> 
       50 + (2 * (Hashtbl.hash qs)) + id
end
  
module PredHashcons = Hashcons(PredHashconsStruct)

let ewr = ExprHashcons.wrap
let euw = ExprHashcons.unwrap
let pwr = PredHashcons.wrap 
let puw = PredHashcons.unwrap

let zero = ewr (Con (Constant.Int(0)))
let one  = ewr (Con (Constant.Int(1)))

(* Constructors: Expressions *)
let eCon = fun c -> ewr (Con c)
let eVar = fun s -> ewr (Var s)
let eApp = fun (s,es) -> ewr (App (s,es))
let eBin = fun (e1, op, e2) -> ewr (Bin (e1, op, e2)) 
let eIte = fun (ip,te,ee) -> ewr (Ite(ip,te,ee))
let eFld = fun (s,e) -> ewr (Fld (s,e))
let eTim = function 
  | (Con (Constant.Int n1), _), (Con (Constant.Int n2), _) -> 
      ewr (Con (Constant.Int (n1 * n2)))
  | (e1, e2) -> eBin (e1, Times, e2)

(* Constructors: Predicates *)
let pTrue  = pwr True
let pFalse = pwr False
let pAtom  = fun (e1, r, e2) -> pwr (Atom (e1, r, e2))
let pAnd   = fun ps -> pwr (And ps)
let pOr    = fun ps -> pwr (Or ps)
let pNot   = fun p  -> pwr (Not p)
let pBexp  = fun e  -> pwr (Bexp e)
let pImp   = fun (p1,p2) -> pwr (Imp (p1,p2))
let pIff   = fun (p1,p2) -> pAnd [pImp (p1,p2); pImp (p2,p1)]
let pForall= fun (qs, p) -> pwr (Forall (qs, p))

module ExprHash = Hashtbl.Make(struct
  type t = expr
  let equal (_,x) (_,y) = (x = y)
  let hash (_,x) = x
end)

module PredHash = Hashtbl.Make(struct
  type t = pred
  let equal (_,x) (_,y) = (x = y)
  let hash (_,x) = x
end)

let bop_to_string = function 
  | Plus  -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Div   -> "/"

let brel_to_string = function 
  | Eq -> "="
  | Ne -> "!="
  | Gt -> ">"
  | Ge -> ">="
  | Lt -> "<"
  | Le -> "<="

let bind_to_string (s,t) = 
  Printf.sprintf "%s:%s" (Symbol.to_string s) (Sort.to_string t)

let rec expr_to_string e = 
  match euw e with
  | Con c -> 
      Constant.to_string c
  | Var s -> 
      Symbol.to_string s
  | App (s, es) ->
      Printf.sprintf "%s([%s])" 
      (Symbol.to_string s) (List.map expr_to_string es |> String.concat "; ")
  | Bin (e1, op, e2) ->
      Printf.sprintf "(%s %s %s)" 
      (expr_to_string e1) (bop_to_string op) (expr_to_string e2)
  | Ite(ip,te,ee) -> 
      Printf.sprintf "(%s ? %s : %s)" 
      (pred_to_string ip) (expr_to_string te) (expr_to_string ee)
  | Fld(s,e) -> 
      Printf.sprintf "%s.%s" (expr_to_string e) s 
     
and pred_to_string p = 
  match puw p with
    | True -> 
        "true"
    | False -> 
        "false"
    | Bexp e ->
        Printf.sprintf "(Bexp %s)" (expr_to_string e)
    | Not p -> 
        Printf.sprintf "(~ (%s))" (pred_to_string p) 
    | Imp (p1, p2) -> 
        Printf.sprintf "(%s -> %s)" (pred_to_string p1) (pred_to_string p2)
    | And ps -> 
        Printf.sprintf "&& [%s]" (List.map pred_to_string ps |> String.concat " ; ")
    | Or ps -> 
        Printf.sprintf "|| [%s]" (List.map pred_to_string ps |> String.concat ";")
    | Atom (e1, r, e2) ->
        Printf.sprintf "(%s %s %s)" 
        (expr_to_string e1) (brel_to_string r) (expr_to_string e2)
    | Forall (qs,p) -> 
        Printf.sprintf "forall %s: %s" 
        (List.map bind_to_string qs |> String.concat ",") (pred_to_string p)

let rec pred_map hp he fp fe p =
  let rec pm p =
    try PredHash.find hp p with Not_found -> begin
      let p' = 
        match puw p with
        | True | False as p1 -> 
            p1
        | And ps -> 
            And (List.map pm ps)  
        | Or ps -> 
            Or (List.map pm ps)  
        | Not p -> 
            Not (pm p) 
        | Imp (p1, p2) -> 
            Imp (pm p1, pm p2) 
        | Bexp e ->
            Bexp (expr_map hp he fp fe e) 
        | Atom (e1, r, e2) ->
            Atom (expr_map hp he fp fe e1, r, expr_map hp he fp fe e2)
        | Forall (qs, p) ->
            Forall (qs, pm p) in
      let rv = fp (pwr p') in
      let _  = PredHash.add hp p rv in 
      rv
    end in pm p 

and expr_map hp he fp fe e =
  let rec em e =
    try ExprHash.find he e with Not_found -> begin
      let e' = 
        match euw e with
        | Con _ | Var _ as e1 -> 
            e1
        | App (f, es) ->
            App (f, List.map em es)
        | Bin (e1, op, e2) ->
            Bin (em e1, op, em e2)
        | Ite (ip, te, ee) ->
            Ite (pred_map hp he fp fe ip, em te, em ee) 
        | Fld (s, e1) -> 
            Fld (s, em e1) in
      let rv = fe (ewr e') in
      let _  = ExprHash.add he e rv in
      rv
    end in em e

let rec pred_iter fp fe pw =
  begin match puw pw with
    | True | False -> ()
    | Bexp e -> expr_iter fp fe e
    | Not p -> pred_iter fp fe p
    | Imp (p1, p2) -> pred_iter fp fe p1; pred_iter fp fe p2
    | And ps | Or ps -> List.iter (pred_iter fp fe) ps
    | Atom (e1, _, e2) -> expr_iter fp fe e1; expr_iter fp fe e2
    | Forall (_, p) -> pred_iter fp fe p (* pmr: looks wrong, but so does pred_map *)
  end;
  fp pw

and expr_iter fp fe ew =
  begin match puw ew with
    | Con _ | Var _ -> ()
    | App (_, es) -> List.iter (expr_iter fp fe) es
    | Bin (e1, _, e2) -> expr_iter fp fe e1; expr_iter fp fe e2
    | Ite (ip, te, ee) -> pred_iter fp fe ip; expr_iter fp fe te; expr_iter fp fe ee
    | Fld (_, e1) -> expr_iter fp fe e1
  end;
  fe ew

let esub x e = function
  | (Var y), _ when x = y -> e
  | _ as e1 -> e1 

let expr_subst hp he e x e' =
  expr_map hp he (fun p -> p) (esub x e') e 

let pred_subst hp he p x e' =
  pred_map hp he (fun p -> p) (esub x e') p 

module Expression = 
  struct
      
    module Hash   = ExprHash 
      
    let to_string = expr_to_string

    let print     = fun fmt e -> Format.pp_print_string fmt (to_string e)

    let show      = print Format.std_formatter

    let map fp fe e =
      let hp = PredHash.create 251 in
      let he = ExprHash.create 251 in 
      expr_map hp he fp fe e 

    let iter fp fe e =
      expr_iter fp fe e

    let subst e x e' =
      map id (esub x e') e

    let support e =
      let xs = ref Symbol.SSet.empty in
      iter un (function (Var x), _ 
              | (App (x,_)),_ -> xs := Symbol.SSet.add x !xs
              | _             -> ()) e;
      Symbol.SSet.elements !xs |> List.sort compare

    let unwrap = euw

  end
    
module Predicate =
    struct
      module Hash = PredHash 
	
      let to_string = pred_to_string

      let print     = fun fmt p -> Format.pp_print_string fmt (to_string p)

      let show      = print Format.std_formatter
			
      let map fp fe p =
	let hp = PredHash.create 251 in
	let he = ExprHash.create 251 in 
        pred_map hp he fp fe p
	
      let iter fp fe p =
        pred_iter fp fe p

      let subst p x e' =
        map id (esub x e') p

      let substs p xes =
        map id (fun e -> List.fold_left (esub |> Misc.uncurry |> Misc.flip) e xes) p

      let support p =
        let xs = ref Symbol.SSet.empty in
        iter un (function (Var x), _ 
                     | (App (x,_)),_ -> xs := Symbol.SSet.add x !xs;
                     | _             -> ()) p; 
        Symbol.SSet.elements !xs |> List.sort compare

      let size p =
	let c = ref 0 in
        let f = fun _ -> incr c in
        let _ = iter f f p in 
        !c

      let size p =
	let c = ref 0 in
        let _ = iter (fun _ -> incr c) p in 
        !c

      let unwrap = puw

      let is_contra = 
        let t = PredHash.create 17 in
        let _ = [pFalse; pNot pTrue; pAtom (zero, Eq, one); pAtom (one, Eq, zero)]
                |> List.iter (fun p-> PredHash.replace t p ()) in 
        fun p -> PredHash.mem t p 
        
      let is_tauto  = function
        | Atom(e1, Eq, e2), _ -> e1 == e2
	| Imp (p1, p2), _ -> 
	    (* matching (p -> p) && (p -> p)) *)
	    p1 == p2
        | True, _              -> true
        | _                   -> false

    end



let print_stats _ = 
  Printf.printf "Ast Stats. [none] \n"


(********************************************************************************)
(************************** Rationalizing Division ******************************)
(********************************************************************************)

let expr_isdiv = function
  | Bin (_, Div, _), _ -> true
  | _                  -> false

let pull_divisor = function 
  | Bin (_, Div, (Con (Constant.Int i),_)), _ -> i 
  | _ -> 1

let calc_cm e1 e2 =
    pull_divisor e1 * pull_divisor e2 

let rec apply_mult m = function 
  | Bin (e, Div,  (Con (Constant.Int d),_)), _ ->
      let _   = assert ((m/d) * d = m) in
      eTim ((eCon (Constant.Int (m/d))), e)  
  | Bin (e1, op, e2), _ ->
      eBin (apply_mult m e1, op, apply_mult m e2)
  | Con (Constant.Int i), _ -> 
      eCon (Constant.Int (i*m))
  | e -> 
      eTim (eCon (Constant.Int m), e)

let rec pred_isdiv = function 
  | True,_ | False,_ -> 
      false
  | And ps,_ | Or ps,_ -> 
      List.exists pred_isdiv ps
  | Not p, _ | Forall (_, p), _ -> 
      pred_isdiv p
  | Imp (p1, p2), _ ->
      pred_isdiv p1 || pred_isdiv p2
  | Bexp e, _ ->
      expr_isdiv e
  | Atom (e1, _, e2), _ -> 
      expr_isdiv e1 || expr_isdiv e2
       
let bound m e e1 e2 =
  pAnd [pAtom (apply_mult m e, Gt, apply_mult m e2);
        pAtom(apply_mult m e, Le, apply_mult m e1)] 

let rec fixdiv = function
  | p when not (pred_isdiv p) -> 
      p
  | Atom ((Var _,_) as e, Eq, e1), _ | Atom ((Con _, _) as e, Eq, e1), _ ->
      bound (calc_cm e e1) e e1 (eBin (e1, Minus, one))
  | And ps, _ ->
      pAnd (List.map fixdiv ps) 
  | Or ps, _ ->
      pOr (List.map fixdiv ps)
  | Imp (p1, p2), _ ->
      pImp (fixdiv p1, fixdiv p2)
  | Not p, _ -> 
      pNot (fixdiv p) 
  | p -> p

let rec sortcheck_expr f e = 
  match euw e with
  | Con c -> 
      Some Sort.Int 
  | Var s -> 
      (try Some (f s) with _ -> None)
  | Bin (e1, op, e2) -> 
      begin 
        match Misc.map_pair (sortcheck_expr f) (e1,e2) with
        | (Some Sort.Int, Some Sort.Int) -> Some Sort.Int
        | (Some Sort.Int, Some Sort.Ptr) -> Some Sort.Ptr
        | (Some Sort.Ptr, Some Sort.Int) -> Some Sort.Ptr
        | (Some Sort.Ptr, Some Sort.Ptr) -> if op = Minus then Some Sort.Int else None 
        | _                              -> None 
      end
  | Ite(p,e1,e2) -> 
      if sortcheck_pred f p then 
        match Misc.map_pair (sortcheck_expr f) (e1, e2) with
        | (Some t1, Some t2) when t1 = t2 -> Some t1 
        | _ -> None
      else None
  | App (uf, es) -> begin
      let ft = try f uf with _ -> assertf "ERROR: unknown uf = %s" (Symbol.to_string uf) in
      match ft with
      | Sort.Func ts when List.length ts = 1 + List.length es -> begin
        match List.rev ts with
        | ret_t :: arg_ts ->
            let par_ts = List.rev_map (sortcheck_expr f) es in
            if List.for_all2 (fun tp ta -> tp = Some ta) par_ts arg_ts then 
              Some ret_t
            else 
              None
        | _ -> assertf "impossible"
      end
      | _ -> None
  end
  | _ -> None

and sortcheck_rel f (e1, r, e2) = 
  let t1o, t2o = (e1,e2) |> Misc.map_pair (sortcheck_expr f) 
                         |> Misc.map_pair (Misc.maybe_map (function Sort.Ptr -> Sort.Int | x -> x)) in
  match r, t1o, t2o with
  | Eq, Some t1, Some t2 
  | Ne, Some t1, Some t2 when t1 = t2 -> true 
  | Gt, Some t1, Some t2 
  | Ge, Some t1, Some t2 
  | Lt, Some t1, Some t2 
  | Le, Some t1, Some t2 when t1 = t2 -> t1 != Sort.Bool
  | _                                 -> false 
 
and sortcheck_pred f p = 
  match puw p with
    | True  
    | False -> 
        true 
    | Bexp e ->
        sortcheck_expr f e = Some Sort.Bool 
    | Not p -> 
        sortcheck_pred f p
    | Imp (p1, p2) -> 
        List.for_all (sortcheck_pred f) [p1; p2]
    | And ps  
    | Or ps ->
        List.for_all (sortcheck_pred f) ps
    | Atom (e1, r, e2) ->
        sortcheck_rel f (e1, r, e2)
    | Forall (qs,p) ->
        let f' = fun x -> try List.assoc x qs with _ -> f x in
        sortcheck_pred f' p

(********************************************************************************)
(*********************************** Qualifiers *********************************)
(********************************************************************************)

module Qualifier = struct
  
  type t = Sort.t * pred
  
  let sort_of_t = fst 
  
  let pred_of_t = snd 
  
  let create vo t p =
    match vo with 
    | None -> 
        (t, p)
    | Some v ->
      (t, Predicate.subst p v (eVar (Symbol.value_variable t)))

  let print ppf (t, p) = 
    Format.fprintf ppf "qualif qq(%a:%a):%a \n" 
      Symbol.print (Symbol.value_variable t)
      Sort.print t
      Predicate.print p
end



(* {{{
let rec expr_subst hp he e x e' =
  let rec esub e =
    try ExprHash.find he e with Not_found -> begin
      let rv = 
        match euw e with
        | Var y when x = y ->
            e' 
        | Con _ | Var _ -> 
            e
        | App (s, es) ->
            App (s, List.map esub es) |> ewr
        | Bin (e1, op, e2) ->
            Bin (esub e1, op, esub e2) |> ewr
        | Ite (ip, te, ee) ->
            Ite (pred_subst hp he ip x e', esub te, esub ee) |> ewr
        | Fld (s, e1) ->
            Fld (s, esub e1) |> ewr in
      let _  = ExprHash.add he e rv in
      rv 
    end in esub e

and pred_subst hp he e x e' =
  let rec s e =
    try PredHash.find h e with
	Not_found -> (let foo = s1 e in PredHash.add h e foo; foo)
  and s1 e =
    match puw e with
	True -> e
      | False -> e
      | And plist -> pwr (And(List.map s plist))
      | Or plist -> pwr (Or(List.map s plist))
      | Not p -> pwr (Not(s p))
      | Implies (p1, p2) -> pwr (Implies (s p1, s p2))
      | Equality (x,y) -> pwr (Equality(expr_subst h he x v vv,expr_subst h he y v vv))
      | Atom (_) -> e
      | Leq(x,y) -> pwr (Leq(expr_subst h he x v vv, expr_subst h he y v vv))
  in s e
}}} *)  
(** {{{
      let rec support pred =
        let h = Hash.create 251 in
        let eh = Expression.Hash.create 251 in
        let sh = Hashtbl.create 251 in
        let res = ref [] in
        let add s = if not(Hashtbl.mem sh s) then Hashtbl.add sh s (); res := s :: !res in

        let se exp =
          let rec s exp =
            try Expression.Hash.find eh exp with
                Not_found -> Expression.Hash.add eh exp (); s1 exp
          and s1 exp =
            match euw exp with
                Constant(_) -> ()
              | Application (func, args) -> 
                  add func; List.iter s args
              | Variable(sym) -> add sym
              | Sum(args) -> List.iter s args
              | Coeff(c,t) -> s t
              | Ite _ -> failwith "ite not supported"
          in s exp in
          
        let rec s exp =
          try Hash.find h exp with
              Not_found -> Hash.add h exp (); s1 exp
        and s1 pred =
          match puw pred with
              True -> ()
            | False -> ()
            | And plist -> List.iter s plist
            | Or plist -> List.iter s plist
            | Not p -> s p
            | Implies (p1, p2) -> s p1; s p2
            | Equality (x,y) -> se x; se y
            | Leq (x,y) -> se x; se y
            | Atom (s) -> ()
        in s pred; List.rev !res
        
      let h = PredHash.create 251 in
        let rec ip p =
          let _ = f p in
          if not (PredHash.mem h p) then begin
            let _ = PredHash.add h p () in
            match puw p with
            | And ps | Or ps -> 
                List.iter ip plist
            | Not p  | Forall (_,p) -> 
                ip p 
            | Imp (p1, p2) -> 
                ip p1; ip p2
            | _ -> ()
          end in
        ip p 
   }}} *)
(* {{{
  
      (* Translate predicate to a satisfiability-equivalent predicate without Ite *)
      
      let temp_ctr = ref 0
      let new_temp () =
	let n = "$$$" ^ (string_of_int !temp_ctr) in
	  (temp_ctr := !temp_ctr + 1; n)
	  
      let elim_ite sp =
	let cnsts = ref [] in
	let he = Expression.Hash.create 251 in
	let hp = Hash.create 251 in
	let rec te e =
	  try Expression.Hash.find he e
	  with Not_found -> (let foo = te1 e in Expression.Hash.add he e foo; foo)
	and te1 e =
	  match euw e with
	      Constant(c) -> e
	    | Application (func, args) -> 
		ewr (Application (func, List.map te args))
	    | Variable(v) -> ewr (Variable(v))
	    | Sum(args) -> ewr (Sum(List.map te args))
	    | Coeff(c,t) -> ewr (Coeff(c,te t))
	    | Ite(si,st,se) ->
		let temp = ewr (Variable(new_temp())) in
		let i = tp si in
		let tv = te st and ev = te se in
		  begin
		    cnsts := pwr (Or [pwr (Not i); pwr (Equality(temp,(tv)))]) :: (!cnsts);
		    cnsts := pwr (Or [i; pwr (Equality(temp,(ev)))]) :: (!cnsts);
		    temp
		  end
	and tp p = 
	  try Hash.find hp p
	  with Not_found -> (let foo = tp1 p in Hash.add hp p foo; foo)
	and tp1 p =
	  match puw p with
	      True -> p
	    | False -> p
	    | And plist -> pwr (And (List.map tp plist))
	    | Or plist -> pwr (Or (List.map tp plist))
	    | Not p -> pwr (Not (tp p))
	    | Implies (p1, p2) -> pwr (Implies((tp p1),(tp p2)))
	    | Equality (x,y) -> pwr(Equality((te x),(te y)))
	    | Leq (x,y) -> pwr(Leq((te x),(te y)))
	    | Atom (s) -> p
	in
	let foo = tp sp in
	  pwr (And(foo :: !cnsts))
    }}} *)
