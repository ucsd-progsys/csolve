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
module ST  = Ssa_transform
module H   = Hashtbl
module E   = Errormsg
module Misc = FixMisc 
module SM  = Misc.StringMap
module SIM = Misc.EMap (struct type t = string * int 
                               let compare x y = compare x y 
                               let print ppf (s, i) = Format.fprintf ppf "(%s, %d)" s i
                        end)

(* open Cil *)
open Misc.Ops

type t = FixConstraint.tag (* {v: int list | len v = 4 && Hashtbl.mem invt v} *)
                           (* Each t is a [callgraph rank; block_id; instr_id; fname_id] *)
type o = { vem  : Cil.exp CilMisc.VarMap.t
         ; funm : int SM.t
         ; blkm : int SIM.t; 
         }

type cause = Raw   of string 
           | Call  of Cil.exp 
           | Deref of Cil.exp 
           | Spec  of string * Cil.varinfo


type cinfo = { loc   : Cil.location
             ; fname : string
             ; block : int
             ; cause : cause }

let invt : (t, cinfo) H.t = H.create 37

let make_cinfo loc fn block cause = 
  { loc   = loc 
  ; fname = fn
  ; block = block
  ; cause = cause
  }
 


(**********************************************************)
(**************** Call Graph SCC Order ********************)
(**********************************************************)

class calleeVisitor calleesr = object(self)
  inherit Cil.nopCilVisitor
    method vinst = function
    | Cil.Call (_, Cil.Lval ((Cil.Var v), Cil.NoOffset), _, _) ->
        calleesr := v.Cil.vname :: !calleesr; Cil.DoChildren
    | _ -> Cil.DoChildren
end

let callgraph_of_scis scis = 
  Misc.flap begin fun sci ->
    let fd  = sci.ST.fdec in
    let fn  = fd.Cil.svar.Cil.vname in
    let vsr = ref [] in
    let _   = Cil.visitCilFunction (new calleeVisitor vsr) fd in
    Misc.map (fun v -> (fn, v)) !vsr
  end scis 

let fn_to_i, i_to_fn =
  let fn_i_t = H.create 37 in
  let i_fn_t = H.create 37 in
  let tick,_ = Misc.mk_int_factory () in
  ((fun fn -> 
      try H.find fn_i_t fn with Not_found -> 
        let i = tick () in
        let _ = H.replace fn_i_t fn i; H.replace i_fn_t i fn in i), 
   (fun i  -> Misc.do_catch "CilTag.i_to_fn!" (H.find i_fn_t) i))

let create_funm scis =
  let is = Misc.map (fun sci -> fn_to_i sci.ST.fdec.Cil.svar.Cil.vname) scis in 
  scis |> callgraph_of_scis
       |> Misc.map (Misc.map_pair fn_to_i)
       |> Fcommon.scc_rank "callgraph" (fun i -> Printf.sprintf "%d:%s" i (i_to_fn i)) is 
       |> Misc.map (Misc.app_fst i_to_fn)
       |> List.fold_left (fun fm (fn, r) -> SM.add fn r fm) SM.empty

(**********************************************************)
(************* Control-Flow Graph SCC Order ***************)
(**********************************************************)

(* is j is a dominator of i *)
let rec is_dominator doma j i =
  if i < j then false else
    let idom = doma.(i) |> fst in
    if idom = j then true else
      is_dominator doma j idom

let is_backedge sci (i,j) = 
  is_dominator sci.ST.gdoms j i

(* returns edges, adds self edges, filters backedges *)
let edges_of_sci sci : (int * int) list =
  Misc.array_to_index_list sci.ST.cfg.Ssa.successors
  |> Misc.flap (fun (i,js) -> List.map (fun j -> (i,j)) js)
  |> Misc.negfilter (is_backedge sci)

let blockranks_of_sci sci : ((string * int) * int) list =
  let fn = sci.ST.fdec.Cil.svar.Cil.vname in
  let is = Misc.array_to_index_list sci.ST.cfg.Ssa.successors |> Misc.map fst in
  sci |> edges_of_sci
      |> Fcommon.scc_rank ("blocks-"^fn) string_of_int is
      |> Misc.map (fun (i,r) -> ((fn, i), r))

let create_blkm scis =
  scis |> Misc.flap blockranks_of_sci
       |> List.fold_left (fun bm (k,r) -> SIM.add k r bm) SIM.empty  

(**********************************************************)
(******************** API Accessors ***********************)
(**********************************************************)

let t_of_tag     = fun t -> asserti (H.mem invt t) "bad tag!"; t 

(* API *)
let loc_of_tag   = fun me t -> (H.find invt t).loc
let cause_of_tag = fun me t -> (H.find invt t).cause
let tag_of_t     = id 

(*let fname_of_t   = fun me t -> (H.find invt t).fname
  let block_of_t   = fun me t -> (H.find invt t).block 
*)

(* API *)
let d_exp_reSugar me ()   = Cil.d_exp ()   <.> (CilMisc.reSugar_exp me.vem)
let d_instr_reSugar me () = Cil.d_instr () <.> (CilMisc.reSugar_instr me.vem)

(* API *)
let make_t me loc fn blk instr cause =
  try
    ([SM.find fn me.funm; SIM.find (fn, blk) me.blkm; (0 - instr); fn_to_i fn], fn)
    >> Misc.flip (H.replace invt) (make_cinfo loc fn blk cause)
  with Not_found ->
    Errormsg.error "CilTag.make_t: bad args fn=%s, blk=%d" fn blk;
    assertf "CilTag.make_t"

(* API *)
let make_global_t =
  let glob_index = ref (-1) in
    fun me loc cause ->
      glob_index := !glob_index - 1;
      ([!glob_index; -1; -1; -1], "global")
      >> Misc.flip (H.replace invt) (make_cinfo loc "global" 0 cause)

let make_vem scis = 
  scis |>: (fun sci -> sci.Ssa_transform.fdec) |> CilMisc.varExprMap


(* API *)
let create scis = 
  { vem  = make_vem scis  
  ; funm = create_funm scis
  ; blkm = create_blkm scis
  }

(* API *)
let d_cause me () = function
  | Raw s       -> Pretty.dprintf "Raw   %s" s
  | Call e      -> Pretty.dprintf "Call  %a" Cil.d_exp e
  | Deref e     -> Pretty.dprintf "Deref %a" Cil.d_exp e
  | Spec (s, v) -> Pretty.dprintf "Spec %s %s" s v.Cil.vname


