(*
 * Copyright © 1990-2009 The Regents of the University of California. All rights reserved. 
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

(* This file is part of the liquidC Project.*)

(*******************************************************************)
(************* Constraint Generation Infrastructure ****************)
(*******************************************************************)

module F  = Format
module ST = Ssa_transform
module H  = Hashtbl
module CI = CilInterface

open Misc.Ops
open Cil

type binding = Exp of Cil.exp | Phi | Undef 

type t = {
  sci     : ST.ssaCfgInfo;
  size    : int;                            (* number of blocks *)
  vart    : (string, Cil.varinfo) H.t;      (* string|-> var *) 
  expt    : (string, binding) H.t;          (* string|-> defining assignment *)
  defa    : string list array;              (* block |-> vars defined inside block *)
  doma    : (int * bool option) list array; (* block |-> (dom guard * bool) list *)
  phidefa : (string * string) list array;   (* block |-> (x, xi) list, 
                                                            st. xi defined in block,
                                                             x = phi(...xi...) *)
}

(*******************************************************************)
(*************  Visitor: Gather block-binding information  *********)
(*******************************************************************)

class consInfraVisitor size bindr = object(self) 
  inherit nopCilVisitor
  
  val sid = ref 0

  method vinst = function
    | Set (((Var v), NoOffset), e, _) ->
        bindr := (!sid, v.Cil.vname, e) :: !bindr;
        DoChildren 
    | _ -> 
        asserts false "TBD: consInfraVisitor vinst";
        assert false

  method vstmt s =
    asserts (0 <= s.Cil.sid && s.Cil.sid < size) "consInfraVisitor";
    sid := s.sid;
    DoChildren
end

let defa_of_bindings size binds = 
  let defa = Array.make size [] in
  List.iter (fun (i, vn, _) -> defa.(i) <- vn :: defa.(i)) binds;
  defa

let var_expt_of_bindings binds phia = 
  let t = H.create 37 in
  List.iter 
    (fun (_, vn, e) -> 
      asserts (not (H.mem t vn)) "duplicate binding";
      H.add t vn (Exp e)) 
    binds;
  Array.iter (List.iter (fun (v,_) -> H.add t v.Cil.vname Phi)) phia;
  t 

(*******************************************************************)
(********** Dom-Tree: Guards and Reaching Definitions **************) 
(*******************************************************************)

let dom_closure gdoma = 
  let n    = Array.length gdoma in
  let doma = Array.make n [] in 
  for i = 1 to n - 1 do
    let j = fst (gdoma.(i)) in
    asserts (0 <= j && j < n) "dom_closure bounds check";
    Array.set doma i (gdoma.(i) :: doma.(j))
  done;
  doma

let guard me = function 
  | (_, None)   -> 
      None 
  | (i, Some b) -> begin 
      match me.sci.ST.ifs.(i) with 
      | None -> 
          assertf "ERROR: expand_guard"
      | Some (e,_,_) -> 
          let p  = CI.pred_of_cilexp e in
          Some (if b then p else (Ast.pNot p))
  end

(*****************************************************************)
(********** (Inverted) Phi-bindings  *****************************)
(********** block |-> (x, xi) list,  *****************************)
(********** st. xi defined in block, *****************************) 
(********** and x = phi(...xi...)    *****************************) 
(*****************************************************************)

let phidefa_of_phia phia =
  let a       = Array.make (Array.length phia) [] in
  let phidefs = Misc.array_flapi begin fun _ asgns -> 
                  Misc.flap begin fun (v, ivis) -> 
                    Misc.map begin fun (i, vi) -> 
                      (i, v.Cil.vname, vi.Cil.vname)
                    end ivis
                  end asgns
                end phia in
  List.iter (fun (i, v, vi) -> a.(i) <- (v, vi) :: (a.(i))) phidefs;
  a

(*****************************************************************)
(************************ API ************************************)
(*****************************************************************)

(* API *)
let create sci =
  let n = Array.length sci.ST.gdoms in
  let bindsr = ref [] in
  let vs     = sci.ST.fdec.Cil.slocals in
  let phia   = sci.ST.phis in 
  let vis    = new consInfraVisitor n bindsr in
  let _      = Cil.visitCilFunction vis sci.ST.fdec in
  let rv     =
  { sci      = sci;
    size     = n;
    doma     = dom_closure sci.ST.gdoms;
    defa     = defa_of_bindings n !bindsr; 
    expt     = var_expt_of_bindings !bindsr phia;
    phidefa  = phidefa_of_phia phia;
    vart     = vs |> List.map (fun v -> (v.Cil.vname, v))
                  |> Misc.hashtbl_of_list; 
  } in
  rv

(* API *)
let location me i =
  Cil.get_stmtLoc me.sci.ST.cfg.Ssa.blocks.(i).Ssa.bstmt.skind

(* API *)
let ssa_srcs me i = 
  Misc.do_catch "ssa_srcs" (Array.get me.phidefa) i 
  |> List.map (Misc.map_pair (H.find me.vart))

(* API *)
let ssa_targs me i = 
  Misc.do_catch "ssa_targs" (Array.get me.sci.ST.phis) i
  |> List.map fst 

(* API *)
let var_exp me v =
  try H.find me.expt v.Cil.vname with Not_found -> Undef

(* API *)
let def_vars me i = 
  Misc.do_catch "def_vars" (Array.get me.defa) i 
  |> List.map (H.find me.vart)

(* API *)
let reach_vars me i = 
  (Misc.do_catch "reach_vars" (Array.get me.doma) i)
  |> Misc.map_partial (function (i, None) -> Some i | _ -> None)
  |> Misc.flap (def_vars me)

(* API *)
let guardp me i = 
  Misc.do_catch "guardp" (Array.get me.doma) i 
  |> Misc.map_partial (guard me) 
  |> Ast.pAnd
