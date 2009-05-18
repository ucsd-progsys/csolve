
module SM = Misc.StringMap
module IM = Misc.IntMap
open Cil
open Misc.Ops

exception Unhandled

(******************************* expressions ********************************)
module Expression = struct
  type t = exp             

  let of_var v = Lval ((Var v), NoOffset)
end

(******************************** predicates *********************************)
module Predicate = struct
  type t = exp     

  let d_pred = Cil.d_exp

  let mk_not p = 
    UnOp (LNot, p, typeOf p)

  let mk_eq e1 e2 =
    BinOp (Eq, e1, e2, intType)

  let mk_and e1 e2 = 
    BinOp (LAnd, e1, e2, intType) 

  let truep = 
    mk_eq (integer 0) (integer 0)

  let conjoin = function
    | []    -> truep
    | [p]   -> p
    | p::ps -> List.fold_left (fun acc p -> mk_and p acc) p ps

  class subsVisitor xem = object(self) 
    inherit nopCilVisitor
    
    method vexpr = function
      | Lval ((Var x), NoOffset) -> 
          (try ChangeTo (SM.find x.vname xem) 
           with Not_found -> DoChildren) 
      | _ -> DoChildren
  end

  let substitute xes e = 
    let xem = List.fold_left (fun m (x,e) -> SM.add x e m) SM.empty xes in
    visitCilExpr (new subsVisitor xem) e 

end

(********************* refinement types and templates ***********************)

module Template = struct
  type psub = (string * Expression.t) list
  type kvar = int 
  type refn = Conc of Predicate.t | Kvar of kvar * psub
  type soln = Predicate.t list IM.t 

  type uty  = typ
  and  t    = uty * refn
  type env  = (t * varinfo)  SM.t

  let d_refn () = function
    | Conc p    -> Pretty.dprintf "%a" Predicate.d_pred p
    | Kvar (i,_) -> Pretty.dprintf "k_%d" i 

  let d_tplt () (u, r) = 
    Pretty.dprintf "{V: %a | %a}" d_type u d_refn r 

  let value_var u : Cil.varinfo =
    let vn = Pretty.sprint ~width:80 (Pretty.dprintf "VV_%a" d_type u) in
    makeGlobalVar vn u

  let mk_ty u r = (u, r)

  let get_refn (t: t) : refn = snd t

  let get_kvars (t: t) : kvar list =
    match get_refn t with Kvar (k, _) -> [k] | _ -> []

  let t_const e : t =
    let u  = typeOf e in
    let vv = Lval ((Var (value_var u)), NoOffset) in
    let r  = Conc (Predicate.mk_eq vv e) in
    mk_ty u r 

  let t_var (g:env) (v:varinfo) : t =
    let vn = v.vname in
    try fst (SM.find vn g) with Not_found -> 
      failwith ("t_var: unknown var"^vn)

  let t_exp (g:env) (e:Expression.t) : t = 
    match e with
    | Const _   | UnOp _  | BinOp _ -> 
        t_const e  
    | _ ->
        raise Unhandled

  (********************** solution accessors *******************************)

  let s_lookup s k = 
    try IM.find k s with Not_found -> 
      failwith "Not_found in lookup_solution"

  let s_update s k v = 
    IM.add k v s

  let s_apply_tplt s (u, t) =
    match t with
    | Conc p       -> (u, t)
    | Kvar (k,xes) -> let p  = Predicate.conjoin (s_lookup s k) in
                      (u, Conc (Predicate.substitute xes p))

  let s_apply_env s (g : env) = 
    SM.map (fun (t,v) -> ((s_apply_tplt s t), v)) g

  (************************************************************************)

  let p_env (g: env) : Predicate.t = 
    let ps = 
      SM.fold 
        (fun x (t,vi) acc ->
          match get_refn t with 
          | Conc p -> 
              let vvn = (value_var vi.vtype).vname in 
              let xp  = Predicate.substitute [(vvn, Expression.of_var vi)] p in
              xp::acc
          | _ -> failwith "p_env: kvar in template -- liquid")
        g [] in
    Predicate.conjoin ps

end

(********************************** Constraints ************************************)

module Constraint = struct

  type t = {
    cid : int;
    fid : string;                 (* function identifier *)
    grd : (int * bool) list;      (* guard selectors     *)
    lhs : Template.t;
    rhs : Template.t;
    loc : location;
  }

  let get_lhs c = c.lhs

  let get_rhs c = c.rhs

  let get_kvars c = (Template.get_kvars c.lhs) @ (Template.get_kvars c.rhs)

  let mk_constr =
    let id = ref 0 in
    fun fid grd lhs rhs loc ->
      {cid = (incr id; !id);
       fid = fid; 
       grd = grd; 
       lhs = lhs; 
       rhs = rhs; 
       loc = loc}

  let expand_guard ifs c =
    List.map
      (fun (i,b) -> 
        match ifs.(i) with 
        | Some (e,_,_) when b     -> e
        | Some (e,_,_) when not b -> Predicate.mk_not e
        | _                       -> asserts false "ERROR: expand_guard"; assert false)
      c.grd

  let d_constr ifs () c =
    Pretty.dprintf "id = %d; fid = %s; grd = %a; lhs = %a; rhs = %a \n" 
      c.cid c.fid 
      (Pretty.d_list "," d_exp) (expand_guard ifs c) 
      Template.d_tplt c.lhs 
      Template.d_tplt c.rhs

end
