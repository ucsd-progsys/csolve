module M   = FixMisc
module P   = Pretty
module F   = FixConstraint
module A   = Ast  
module As  = A.Symbol  
module Ac  = A.Constant
module Asm = As.SMap
  
open M.Ops

type class_bound = int option (* None = unbounded *)

let scale_bound x b =
  M.map_opt (( * ) x) b

let bound_offset x b =
  M.map_opt (( + ) x) b

let bound_plus b1 b2 = match b1, b2 with
  | Some n, Some m -> Some (m + n)
  | _              -> None

let lower_bound_le b1 b2 = match b1, b2 with
  | Some n, Some m -> n <= m
  | Some _, None   -> false
  | _              -> true

let upper_bound_le b1 b2 = match b1, b2 with
  | Some n, Some m -> n <= m
  | None, Some _   -> false
  | _              -> true

let bound_min le b1 b2 =
  if le b1 b2 then b1 else b2

let bound_max le b1 b2 =
  if le b1 b2 then b2 else b1

let lower_bound_min = bound_min lower_bound_le
let lower_bound_max = bound_max lower_bound_le
let upper_bound_min = bound_min upper_bound_le
let upper_bound_max = bound_max upper_bound_le

(* Describes the integers lb <= n <= ub s.t. n = c (mod m) *)
type bounded_congruence_class = {
  lb : class_bound; (* Lower bound; lb mod m = c *)
  ub : class_bound; (* Upper bound; ub mod m = c; lb < ub *)
  c  : int;         (* Remainder; 0 <= c < m *)
  m  : int;         (* Modulus; 0 < m <= ub - lb *)
}

type t =
  | IBot                                 (* Empty *)
  | IInt    of int                       (* Single integer *)
  | ICClass of bounded_congruence_class  (* Subset of congruence class *)

type tIndex = t

let top = ICClass {lb = None; ub = None; c = 0; m = 1}

let ind_of_any = top

let nonneg = ICClass {lb = Some 0; ub = None; c = 0; m = 1}

let d_index () = function
  | IBot                                           -> P.dprintf "{ }"
  | i when i = top                                 -> P.dprintf "{true}"
  | IInt n                                         -> P.dprintf "{%d}" n
  | ICClass {lb = None; ub = None; c = c; m = m}   -> P.dprintf "{%d +/- %d*}" c m
  | ICClass {lb = Some n; ub = None; m = m}        -> P.dprintf "{%d + %d*}" n m
  | ICClass {lb = Some l; ub = Some u; m = m}      -> P.dprintf "{%d + %d* <= %d}" l m u
  | ICClass {lb = None; ub = Some u; c = c; m = m} -> P.dprintf "{%d +/- %d* <= %d}" c m u

let repr_suffix = function
  | IBot                                     -> "B"
  | IInt n                                   -> string_of_int n
  | ICClass {lb = lb; ub = ub; m = m; c = c} ->
    let lbs = match lb with Some n -> string_of_int n | None -> "-" in
    let ubs = match ub with Some n -> string_of_int n | None -> "+" in
    lbs ^ "#c" ^ string_of_int c ^ "#m" ^ string_of_int m ^ "#" ^ ubs

let repr_prefix = "Ix#"

let repr i =
  repr_prefix ^ repr_suffix i

let compare i1 i2 = compare i1 i2

let of_int i =
  IInt i

let mk_sequence start period lbound ubound = match lbound, ubound with
  | Some m, Some n when m = n -> IInt n
  | _                         -> ICClass {lb = lbound; ub = ubound; m = period; c = start mod period}

let mk_geq n =
  ICClass {lb = Some n; ub = None; m = 1; c = 0}

let mk_leq n =
  ICClass {lb = None; ub = Some n; m = 1; c = 0}

let mk_eq_mod c m =
  ICClass {lb = None; ub = None; m = m; c = c}

let is_unbounded = function
  | ICClass {lb = None} | ICClass {ub = None} -> true
  | _                                         -> false

let period = function
  | ICClass {m = m} -> Some m
  | IInt _ | IBot   -> None

let is_periodic i =
  period i != None

let is_subindex i1 i2 = match i1, i2 with
  | IBot, _                                          -> true
  | _, IBot                                          -> false
  | IInt n, IInt m                                   -> n = m
  | ICClass _, IInt _                                -> false
  | IInt n, ICClass {lb = lb; ub = ub; c = c; m = m} ->
    lower_bound_le lb (Some n) && upper_bound_le (Some n) ub && ((n mod m) = c)
  | ICClass {lb = lb1; ub = ub1; c = c1; m = m1},
    ICClass {lb = lb2; ub = ub2; c = c2; m = m2} ->
    lower_bound_le lb2 lb1 && upper_bound_le ub1 ub2 && (m1 mod m2) = 0 && (c1 mod m2) = c2

let lub i1 i2 =
  if is_subindex i1 i2 then
    i2
  else if is_subindex i2 i1 then
    i1
  else match i1, i2 with
    | IBot, _ | _, IBot -> assert false
    | IInt m, IInt n    ->
      let d = abs (m - n) in
      ICClass {lb = Some (min m n); ub = Some (max m n); m = d; c = m mod d}
    | IInt n, ICClass {lb = lb; ub = ub; m = m; c = c}
    | ICClass {lb = lb; ub = ub; m = m; c = c}, IInt n ->
      let m = M.gcd m (abs (c - (n mod m))) in
      ICClass {lb = lower_bound_min lb (Some n);
               ub = upper_bound_max ub (Some n);
               m  = m;
               c  = c mod m}
    | ICClass {lb = lb1; ub = ub1; m = m1; c = c1},
      ICClass {lb = lb2; ub = ub2; m = m2; c = c2} ->
      let m = M.gcd m1 (M.gcd m2 (abs (c1 - c2))) in
      ICClass {lb = lower_bound_min lb1 lb2;
               ub = upper_bound_max ub1 ub2;
               m  = m;
               c  = c1 mod m}

(* pmr: There is surely a closed form for this, to be sought
        later. *)
let rec search_congruent c1 m1 c2 m2 n =
  if n < 0 then None else
    if (n mod m1) = c1 && (n mod m2) = c2 then
      Some n
    else
      search_congruent c1 m1 c2 m2 (n - 1)

let glb i1 i2 =
  if is_subindex i1 i2 then
    i1
  else if is_subindex i2 i1 then
    i2
  else match i1, i2 with
    | IBot, _ | _, IBot         -> assert false
    | IInt m, IInt n when m = n -> IInt m
    | IInt _, IInt _            -> IBot
    | IInt n, ICClass {lb = lb; ub = ub; m = m; c = c}
    | ICClass {lb = lb; ub = ub; m = m; c = c}, IInt n ->
      if lower_bound_le lb (Some n) &&
         upper_bound_le (Some n) ub &&
         (n mod m) = c then
        IInt n
      else IBot
    | ICClass {lb = lb1; ub = ub1; m = m1; c = c1},
      ICClass {lb = lb2; ub = ub2; m = m2; c = c2} ->
      let m = M.lcm m1 m2 in
      match search_congruent c1 m1 c2 m2 (m - 1) with
        | None   -> IBot
        | Some c ->
          let lb = lower_bound_max lb1 lb2 |> M.map_opt (fun l -> m * ((l + m - c - 1) / m) + c) in
          let ub = upper_bound_min ub1 ub2 |> M.map_opt (fun l -> m * ((l - c) / m) + c) in
          match lb, ub with
            | Some l, Some u when u = l -> IInt u
            | Some l, Some u when u < l -> IBot
            | _                         -> ICClass {lb = lb; ub = ub; m = m; c = c}

let overlaps i1 i2 =
  glb i1 i2 <> IBot

let offset n = function
  | IBot                                     -> IBot
  | IInt m                                   -> IInt (n + m)
  | ICClass {lb = lb; ub = ub; c = c; m = m} ->
    ICClass {lb = bound_offset n lb;
             ub = bound_offset n ub;
             c  = (c + n) mod m;
             m  = m}

let plus i1 i2 = match i1, i2 with
  | IBot, _ | _, IBot     -> IBot
  | IInt n, i | i, IInt n -> offset n i
  | ICClass {lb = lb1; ub = ub1; c = c1; m = m1},
    ICClass {lb = lb2; ub = ub2; c = c2; m = m2} ->
    ICClass {lb = bound_plus lb1 lb2;
             ub = bound_plus ub1 ub2;
             c  = 0;
             m  = M.gcd m1 (M.gcd m2 (M.gcd c1 c2))}

let minus i1 i2 = match i1, i2 with
  | IBot, _ | _, IBot                                                -> IBot
  | IInt n, IInt m                                                   -> IInt (n - m)
  | ICClass {lb = Some l; ub = ub; c = c; m = m}, IInt n when l >= n ->
    ICClass {lb = Some (l - n);
             ub = bound_offset (-n) ub;
             c = (c - n) mod m;
             m = m}
  | _ -> top

let scale x = function
  | IBot                 -> IBot
  | _ when x = 0         -> IInt 0
  | IInt n               -> IInt (n * x)
  | ICClass _ when x < 0 -> top
  | ICClass {lb = lb; ub = ub; c = c; m = m} ->
    ICClass {lb = scale_bound x lb;
             ub = scale_bound x ub;
             c = x * c;
             m = x * m}

let mult i1 i2 = match i1, i2 with
  | IBot, _ | _, IBot     -> IBot
  | IInt n, i | i, IInt n -> scale n i
  | _                     -> top

let constop op i1 i2 = match i1, i2 with
  | IBot, _ | _, IBot -> IBot
  | IInt n, IInt m    -> IInt (op n m)
  | _                 -> top

let div =
  constop (/)

let unsign i = match i with
  | IBot                              -> IBot
  | IInt n when n >= 0                -> i
  | ICClass {lb = Some n} when n >= 0 -> i
  | _                                 -> nonneg


(* v < i *)      
let le_lt f i = match i with
  | IBot -> IBot
  | IInt n -> mk_leq (f n)
  | ICClass { ub = Some ub } -> mk_leq (f ub)
  | _ -> top

(* v > i *)	
let ge_gt f i = match i with
  | IBot -> top
  | IInt n -> mk_geq (f n)
  | ICClass { lb = Some lb } -> mk_geq (f lb)
  | _ -> top

let lt = le_lt (fun n -> (n-1))
let le = let id x = x in le_lt id

let gt = ge_gt (fun n -> (n+1))
let ge = let id x = x in ge_gt id

module OrderedIndex = struct
  type t = tIndex

  let compare = compare
end

module IndexSet        = Set.Make (OrderedIndex)
module IndexSetPrinter = P.MakeSetPrinter (IndexSet)

let d_indexset () is =
  IndexSetPrinter.d_set ", " d_index () is

let asint = true
let asref = false
let ckint = A.Sort.is_int

let rec index_of_pred env solution sym is_int (pred,x) =
  let index_of_p = index_of_pred env solution sym in
  let index_of_e = index_of_expr env solution sym in
  match pred with
    | A.True -> top
    | A.False -> IBot
    | A.Or ps -> List.map (index_of_p is_int) ps
                 |> List.fold_left lub IBot
    | A.And ps -> List.map (index_of_p is_int) ps
                 |> List.fold_left glb top
    (* This beast matches (v - e) mod k = 0 *)
    | A.Atom ((A.Bin ((A.Bin ((A.Var vv,_),
			      A.Minus,
			      e),
		       _),
		      A.Mod,
		      (A.Con (Ac.Int k),
		       _)),
	       _),
	      A.Eq,
	      (A.Con (Ac.Int 0), _)) when vv = sym ->
	begin match index_of_e asint e with
	  | IInt n -> mk_eq_mod n k
	  | _ -> top
	end
    | A.Atom ((A.Var vv, _),A.Eq,e)
    | A.Atom (e,A.Eq,(A.Var vv, _)) when vv = sym -> index_of_e is_int e
    | A.Atom ((A.Var vv, _),r,e) when vv = sym ->
	let eVal = index_of_e is_int e in
	  begin match r with
	    | A.Gt -> gt eVal
	    | A.Ge -> ge eVal
	    | A.Lt -> lt eVal
	    | A.Le -> le eVal
	    | A.Ne -> top
	  end
    | A.Atom (e,r,(A.Var vv, _)) when vv = sym ->
	let eVal = index_of_e is_int e in
	  begin match r with
	    | A.Gt -> lt eVal
	    | A.Ge -> le eVal
	    | A.Lt -> gt eVal
	    | A.Le -> ge eVal
	    | A.Ne -> top
	  end
    | A.Imp ((A.Atom ((A.Var sym1,_), A.Ne, (A.Con (Ac.Int 0),_)),_),
	     (A.Atom ((A.Var sym2,_), A.Eq, (A.App (sym3,e),_)),_))
	when sym1 = sym && sym2 = sym && sym3 = As.of_string "BLOCK_BEGIN"
	  -> IInt 0
    | A.Bexp e ->
	begin match fst e with
	  | A.Con (Ac.Int n) -> if is_int then of_int n else IBot
	  | _ -> top
	end
    | _ -> top
and index_of_expr env solution sym is_int expr =
  let index_of_e = index_of_expr env solution sym in
  match fst expr with
    | A.Con (A.Constant.Int n) -> 
	if is_int then IInt n else IBot
    (* pmr: Stopgap. If we have (e: int) and is_int is true, should we just
       interpret e as an int and return the result? Alex? *)
    | A.Cst ((A.Con (A.Constant.Int n), _), s) when s = A.Sort.t_int ->
	if is_int then IInt n else IBot
    | A.Var sym ->
	begin match F.lookup_env env sym with
	  | Some ((vv, sort, refas) as reft) ->
	      index_of_reft env solution (ckint sort) reft
	  | _ -> top
	end
    | A.App (sym,e)
	when expr = A.eApp (As.of_string "BLOCK_BEGIN", e) ->
	IInt 0
    | A.Bin (e1, oper, e2) ->
	let e1v = index_of_e asint e1 in
	let e2v = index_of_e asint e2 in
	  begin match oper with
	    | A.Plus  -> plus  e1v e2v
	    | A.Minus -> minus e1v e2v
	    | A.Times -> mult  e1v e2v
	    | A.Div   -> div   e1v e2v
	    | A.Mod   -> top
	  end
    | A.Ite (_,e1,e2) (* Ignore the predicate for now? *) ->
	lub (index_of_e is_int e1) (index_of_e is_int e2)
    | _ -> top
and index_of_refa env sol v is_int refa = match refa with
  | F.Kvar (_, k) -> sol k
  | F.Conc pred -> index_of_pred env sol v is_int pred
and index_of_reft env sol is_int (v,t,refas) =
  List.fold_left glb top
    (List.map (index_of_refa env sol v (ckint t)) refas)

let int_bindings_of ((e1,_),op,(e2,_)) = 
  let vv = As.value_variable A.Sort.t_int in
  match e1, op, e2 with
  | A.Var v1, op, A.Var v2 -> 
    [(v1, (vv, A.Sort.t_int, [F.Conc (A.pAtom  (A.eVar vv,op,A.eVar v2))]))]
      (* Let this be a reminder: THIS creates a cycle! *)
     (* (v2, (vv, A.Sort.t_int, [F.Conc (A.pAtom  (A.eVar v1,op,A.eVar vv))]))]  *)
     |> Asm.of_list
  | A.Var v1, op, (A.Con (A.Constant.Int n)) -> 
    let reft = (vv, A.Sort.t_int, [F.Conc (A.pAtom (A.eVar vv,op,A.eInt n))]) in
    [(v1,reft)]
     |> Asm.of_list
  | _ -> Asm.empty 
  
let apply_int_grd_bs_pred vv eq n = function
  | A.Atom ((A.Var v, _),
             A.Eq,
            (A.Ite ((A.Atom (e1, op, e2), _),
                    (A.Con (A.Constant.Int tn),_),
                    (A.Con (A.Constant.Int fn),_)),_)),_
    when v = vv && (n = tn or n = fn) ->
      if ((n = tn) && eq) or ((n = fn) && (not eq)) then
        int_bindings_of (e1, op, e2)
      (* else if (n = tn && (not eq)) then *)
      (*   int_bindings_of (e1, A.neg_brel op, e2) *)
      else
        Asm.empty
  | _ -> Asm.empty
    
let concat_refs (v,s,refas) ((v',s',refas') as reft') =
  let rs = List.map begin function
    | F.Conc p -> 
      F.Conc (A.pAnd (p :: F.preds_of_reft F.empty_solution reft'))
    | k -> k
  end refas
  in (v,s,rs ++ refas')
    
let join_envs env env' = Asm.fold (fun v reft env ->
  Asm.add v (concat_refs (Asm.find v env) reft) env) env' env
    
let apply_int_grd_bs_refas env vv eq n refas =
  List.map begin function
      | F.Conc p -> apply_int_grd_bs_pred vv eq n p |> Asm.to_list
      | _ -> []
  end refas
  |> (List.concat <+> Asm.of_list)
                          
let apply_grd2 env (p,_) = match p with
  | A.Atom ((A.Var v,_), A.Ne, (A.Con (A.Constant.Int n), _))
  | A.Atom ((A.Con (A.Constant.Int n),_), A.Ne, (A.Var v,_))
    -> (match F.lookup_env env v with
        | Some (vv, t, refas) when t = A.Sort.t_int -> 
          apply_int_grd_bs_refas env vv false n refas
        | _ -> Asm.empty)
  | A.Atom ((A.Var v,_), A.Eq, (A.Con (A.Constant.Int n), _))
  | A.Atom ((A.Con (A.Constant.Int n),_), A.Eq, (A.Var v,_))
    -> (match F.lookup_env env v with
        | Some (vv, t, refas) when t = A.Sort.t_int -> 
          apply_int_grd_bs_refas env vv true n refas
        | _ -> Asm.empty)
  | _ -> Asm.empty
    
let apply_grd env = function
  | (A.And ps, _) as p -> 
    List.map (apply_grd2 env <+> Asm.to_list) ps
   |> List.concat 
   |> Asm.of_list
   |> join_envs env
  | p -> apply_grd2 env p |> join_envs env
  
let index_of_reft env sol ((v,t,refas) as reft) =
  index_of_reft env sol (ckint t) reft

let data_index_of_pred vv p =
  index_of_pred Asm.empty (fun _ -> IBot) vv asint p

let ref_index_of_pred vv p =
  index_of_pred Asm.empty (fun _ -> IBot) vv asref p
