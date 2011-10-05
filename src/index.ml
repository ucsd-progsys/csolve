module M   = Misc
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

let congruent i1 i2 = match i1, i2 with
  | IBot, IBot -> true
  | IInt n1, IInt n2 -> n1 = n2
  | ICClass {lb = lb1; ub = ub1; c = c1; m = m1},
      ICClass {lb = lb2; ub = ub2; c = c2; m = m2} ->
      lb1 = lb2 && ub1 = ub2 && m1 = m2 && (c1 - c2) mod m1 = 0
  | _ -> false
  
let top = ICClass {lb = None; ub = None; c = 0; m = 1}

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

let widen i1 i2 =
  if is_subindex i2 i1 then
    i1
  else match i1, i2 with
    | IBot, i | i, IBot -> i 
    | IInt n, IInt m    ->
      let d = abs (n - m) in
      ICClass {lb = Some (min n m);
               ub = Some (max n m);
               c  = n mod d;
               m  = d}
    | IInt n, ICClass {lb = lb; ub = ub; c = c; m = m}
    | ICClass {lb = lb; ub = ub; c = c; m = m}, IInt n ->
      ICClass {lb = lower_bound_min (Some n) lb;
               ub = upper_bound_max (Some n) ub;
               c  = 0;
               m  = M.gcd m (M.gcd c n)}
    | ICClass {lb = lb1; ub = ub1; c = c1; m = m1},
      ICClass {lb = lb2; ub = ub2; c = c2; m = m2} ->
	let m = M.gcd m1 (M.gcd m2 (abs (c1 - c2))) in
	begin match lb1,lb2,ub1,ub2 with
	  | Some lb1', Some lb2', _, _ when lb1' < lb2' ->
	      ICClass { lb = Some lb1'; ub = None; c = c1 mod m; m = m }
	  | _, _, Some ub1', Some ub2' when ub1' > ub2' ->
	      ICClass { lb = None; ub = Some ub1'; c = c1 mod m; m = m }
	  | _  -> ICClass {lb = if lb1 = lb2 then lb1 else None;
               ub = if ub1 = ub2 then ub1 else None;
               c  = c1 mod m;
               m  = m}
	end

let widen i1 i2 =
  if is_subindex i2 i1 then
    i1
  else match i1, i2 with
    | IBot, i | i, IBot -> i
    | IInt n1, IInt n2 ->
	if n1 < n2 then
	  mk_sequence 0 (n2 - n1) (Some n1) None
	else
	  mk_sequence n1 (n1 - n2) None None
    | _ -> top


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
  | IBot   -> IBot
  | IInt n -> IInt (n * x)
  | ICClass {lb = lb; ub = ub; c = c; m = m} ->
    ICClass {lb = scale_bound x lb;
             ub = scale_bound x ub;
             c = c;
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
  match pred with
    | A.True -> top
    | A.False -> IBot
    | A.Or ps -> List.map (index_of_pred env solution sym is_int) ps
                 |> List.fold_left lub IBot
    | A.And ps -> List.map (index_of_pred env solution sym is_int) ps
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
	begin match index_of_expr env solution sym asint e with
	  | IInt n -> mk_eq_mod n k
	  | _ -> top
	end
    | A.Atom ((A.Var vv, _),A.Eq,e)
    | A.Atom (e,A.Eq,(A.Var vv, _)) when vv = sym ->
	index_of_expr env solution sym is_int e
    | A.Atom ((A.Var vv, _),r,e) when vv = sym ->
	let eVal = index_of_expr env solution sym is_int e in
	  begin match r with
	    | A.Gt -> gt eVal
	    | A.Ge -> ge eVal
	    | A.Lt -> lt eVal
	    | A.Le -> le eVal
	    | A.Ne -> top
	  end
    | A.Atom (e,r,(A.Var vv, _)) when vv = sym ->
	let eVal = index_of_expr env solution sym is_int e in
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
  match fst expr with
    | A.Con (A.Constant.Int n) -> 
	if is_int then IInt n else IBot
    | A.Var sym ->
	begin match F.lookup_env env sym with
	  | Some ((vv, sort, refas) as reft) ->
	      index_of_reft env solution is_int reft
	  | _ -> top
	end
    | A.App (sym,e)
	when expr = A.eApp (As.of_string "BLOCK_BEGIN", e) ->
	IInt 0
    | A.Bin (e1, oper, e2) ->
	let e1v = index_of_expr env solution sym asint e1 in
	let e2v = index_of_expr env solution sym asint e2 in
	  begin match oper with
	    | A.Plus  -> plus  e1v e2v
	    | A.Minus -> minus e1v e2v
	    | A.Times -> mult  e1v e2v
	    | A.Div   -> div   e1v e2v
	    | A.Mod   -> top
	  end
    | _ -> top
	(*
	  and index_of_preds env sol v is_int preds =
	  List.map (index_of_pred env sol v is_int) preds|> List.fold_left glb top *)
and index_of_refa env sol v is_int refa = match refa with
  | F.Kvar (_, k) -> if Asm.mem k sol then Asm.find k sol else IBot
  | F.Conc pred -> index_of_pred env sol v is_int pred 
and index_of_reft env sol is_int (v,_,refas) =
  List.fold_left glb top
    (List.map (index_of_refa env sol v is_int) refas)
let index_of_reft env sol ((v,t,refas) as reft) =
  index_of_reft env sol (ckint t) reft

let data_index_of_pred vv p =
  index_of_pred Asm.empty Asm.empty vv asint p

let ref_index_of_pred vv p =
  index_of_pred Asm.empty Asm.empty vv asref p
    
    


