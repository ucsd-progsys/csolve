module P = Pretty
module E = Errormsg

open Cil
open Misc.Ops

(* Assume: all guards are from "if" statements. *)
let mydebug = false

type ginfo = (exp * int option * int option) option

(****************************************************************)             
let print_ifs ifs = 
  if mydebug then 
    Array.iteri begin fun i -> function 
      | None -> 
          ()
      | Some (e,tbo,ebo) -> 
          let ts = Misc.o2s string_of_int tbo in 
          let es = Misc.o2s string_of_int ebo in
          ignore(E.log "block i = %d, grd = (%a), thn = (%s), els = (%s)\n" i d_exp e ts es)
  end ifs

let print_gdoms gdoms =
  if mydebug then
    Array.iteri begin fun i (j, bo) -> 
        Printf.printf "block i = %d, idom = %d, guard = %s \n" 
        i j (Misc.o2s string_of_bool bo)
    end gdoms

(****************************************************************)             

let block_id b = 
  match b.bstmts with 
  | s::_ -> Some s.sid 
  | _    -> None 

class guardVisitor ifs = object(self)
  inherit nopCilVisitor
  method vstmt (s: stmt) : stmt visitAction =
    match s.skind with
    | If (e, tb, eb, _) ->
        Array.set ifs s.sid (Some (e, block_id tb, block_id eb));
        DoChildren
    | _                 -> 
        DoChildren
end

(** [mk_ifs n fdec] = block :-> ginfo option 
 * s.t.  i :-> None                     if i not an IF-STM
	 i :-> Some (e, tbo, ebo)       if i is  an IF-STM with:
                                        guard expr = e, 
                                        then block = tbo, 
                                        else block = ebo.       *)

let mk_ifs (n:int) (fdec:fundec) : ginfo array =
  let ifs = Array.make n None in
  let _   = visitCilFunction (new guardVisitor ifs) fdec in
  ifs

(****************************************************************)
(** [dom_by_then ifs i j] = true iff i has idom j and is dom by "then" cond of j *)
let dom_by_then preds ifs i j =
  match ifs.(j) with
  | Some (_, Some i', _)        -> i = i' 
  | Some (_, None, (Some i'))   -> i != i' && preds.(i) = [j] 
  | _ 			        -> false

(** [dom_by_else ifs i j] = true iff i has idom j and is dom by "else" cond of j *)
let dom_by_else preds ifs i j = 
  match ifs.(j) with
  | Some (_, _, Some i')        -> i = i' 
  | Some (_, (Some i'),None)    -> i != i' && preds.(i) = [j] 
  | _			        -> false

(** [mk_gdoms ifs doms] = block :-> block * (bool option) 
 *  s.t. i :-> j, Some true	if j is idom i and i is dom by "then" cond 
	 i :-> j, Some false	if j is idom i and i is dom by "else" cond
	 i :-> j, None 		if j is idom i o.w. *)
let mk_gdoms preds ifs idom = 
  Array.mapi 
    (fun i j -> 
      let _  = asserts (j < i) "ERROR: mk_gdoms: bad idom" in
      if j < 0 then (j, None) else 
        let tb = dom_by_then preds ifs i j in
        let eb = dom_by_else preds ifs i j in
        let _  = asserts  (not (tb && eb)) "ERROR: then/else dom! i=%d, j=%d" i j in
        (j, if tb || eb then Some tb else None))
    idom
