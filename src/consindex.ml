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


module Misc = FixMisc 
module BS = BNstats
module  P = Pretty
module  C = FixConstraint
module CM = CilMisc
module So = Ast.Sort
module YM = Ast.Symbol.SMap
module SM = Misc.StringMap
module IM = Misc.IntMap
module  Q = Qualifier 

module FA = FixAstInterface
module FI = FixInterface

module Ct = Ctypes
module PA  = PredAbs 
module SPA = Solve.Make (PA)
module SIA = Solve.Make (IndexDomain)
module Ix = Index  
module Cx = Counterexample
module Co = Constants
module Sc = ScalarCtypes

open Misc.Ops
open Cil

let mydebug = false

(****************************************************************)
(********************** Constraint Indexing *********************)
(****************************************************************)

type result = 
  { solution   : FixConstraint.soln
  ; unsatCs    : FixConstraint.t list
  ; unsatCones : (CilTag.t * CilTag.t Ast.Cone.t) list
  }

type t = {
  scim : Ssa_transform.t SM.t;
  wfm  : C.wf list SM.t;
  cm   : C.t list SM.t;
  defm : (varinfo * Ct.refctype) list SM.t;
  depm : C.dep list SM.t;
}

type bind = PA.bind

(* API *)
let create (ws, cs, des, ds) = 
  { scim  = SM.empty
  ; wfm   = SM.empty |> SM.add Co.global_name ws
  ; cm    = SM.empty |> SM.add Co.global_name cs
  ; defm  = SM.empty |> SM.add Co.global_name des
  ; depm  = SM.empty |> SM.add Co.global_name ds }

(* API *)
let add me fn sci (ws, cs, des, ds) =
  { scim = SM.add fn sci me.scim 
  ; wfm  = SM.add fn ws  me.wfm 
  ; cm   = SM.add fn cs  me.cm 
  ; defm = SM.add fn des me.defm
  ; depm = SM.add fn ds  me.depm }

let find me fn = (SM.find fn me.scim, SM.find fn me.wfm, SM.find fn me.cm)

let iter me f = 
  SM.iter (fun fn _ -> f fn (find me fn)) me.scim

let fold me f =
  SM.fold (fun fn _ b -> f fn (find me fn) b) me.scim

let get_wfs  = fun me -> SM.fold (fun _ ws acc -> ws ++ acc) me.wfm  []
let get_cs   = fun me -> SM.fold (fun _ cs acc -> cs ++ acc) me.cm   []
let get_defs = fun me -> SM.fold (fun _ des acc -> des ++ acc) me.defm []
let get_deps = fun me -> SM.fold (fun _ ds acc -> ds ++ acc) me.depm []

let (++) = P.concat

(*
let print so () me =
  match so with 
  | None -> (* print constraints *) 
      fold me begin
        fun fn (_, wfs, cs) d ->
          P.dprintf "Ref-Constraints for %s \n" fn ++
          CM.doc_of_formatter (Misc.pprint_many true "\n" (C.print_t None)) cs ++
          P.dprintf "WF-Constraints for %s \n" fn ++
          CM.doc_of_formatter (Misc.pprint_many true "\n" (C.print_wf None)) wfs ++
          d
      end P.nil
  | Some _ -> (* print solution *)
      me |> get_wfs
         |> Misc.map C.env_of_wf 
         |> Misc.flap C.bindings_of_env 
         |> CM.doc_of_formatter (Misc.pprint_many false "\n" (C.print_binding so))
         |> P.concat (P.text "Liquid Types:\n\n")
*)


type ('a, 'b, 'c, 'd, 'e, 'f) domain = 
  { create      : 'a 
  ; save        : 'b
  ; solve       : 'c
  ; read        : 'd
  ; min_read    : 'd
  ; read_bind   : 'e
  ; cone        : 'f
  }


let d_predAbs = 
  { create    = SPA.create
  ; save      = SPA.save
  ; solve     = SPA.solve
  ; read      = SPA.read
  ; min_read  = SPA.min_read
  ; read_bind = SPA.read_bind
  ; cone      = SPA.cone  
  }

let d_indexAbs = 
  { create    = SIA.create
  ; save      = SIA.save
  ; solve     = SIA.solve
  ; read      = SIA.read
  ; min_read  = SIA.min_read
  ; read_bind = SIA.read_bind
  ; cone      = SPA.cone  
  }

let dump_counterexamples = function
  | []  -> ()
  | cxs -> Format.printf "Counterexamples:\n%a" (Misc.pprint_many true "\n" Cx.print_cex) cxs 

let cones dd ctx cs' = match !Co.cex with
  | false -> []
  | true  -> List.map (C.id_of_t <+> dd.cone ctx) cs' 

let ac_solve dd me fn (ws, cs, ds) qs so kf =
  let env       = YM.map FixConstraint.sort_of_reft FA.builtinm in
  let assm      = match so with Some s0 -> s0 | _ -> C.empty_solution in
  let kuts      = Errormsg.warn "TODO: kuts"; [] in
  let cfg       = FixConfig.create_raw FA.sorts env FA.axioms 4 ds cs ws qs kuts assm in
  let ctx, s    = BS.time "Qual Inst" dd.create cfg kf in
  let _         = Errormsg.log "DONE: qualifier instantiation \n" in
  let _         = Errormsg.log "DONE: solution strengthening \n" in
  let _         = BS.time "save in" (dd.save (fn^".in.fq") ctx) s in
  let _         = Errormsg.log "DONE: saving input constraints \n" in
  let s',cs',cx = BS.time "Cons: Solve" (dd.solve ctx) s in 
  let _         = Errormsg.log "DONE: constraint solving \n" in
  let _         = BS.time "save out" (dd.save (fn^".out.fq") ctx) s' in
  let _         = Errormsg.log "DONE: saving output constraints \n" in
  let _         = dump_counterexamples cx                           in
  let _         = asserts ((not !Co.check_is) || cs' = []) "ERROR: failed index constraint %s" fn in 
  s', cs', cones dd ctx cs' 

let filter_cstrs dd s fp (ws, cs) = 
  let sol = dd.read s in
  let fr  = fun ((v,_,_) as r) -> r |> C.preds_of_reft sol |> Ast.pAnd |> fp v in
  (Misc.filter (fr <.> C.reft_of_wf) ws, Misc.filter (fr <.> C.rhs_of_t) cs)

let ac_scalar_solve dd me fn fp (ws, cs, ds) = (*  (eqs, _, _) = *)
  Misc.with_ref_at Co.slice false begin fun () ->
    ac_solve dd me (fn^".eq")  (ws, cs, ds) [] (* eqs *) None None |> fst3
  end

let get_cstrs me = 
  (get_wfs me, get_cs me, get_deps me)
  (* >> (fun (ws, cs, ds) -> if ws = [] then failwith "NO WF CONSTRAINTS")
*)

let idx_solve me fn qs = 
  BS.time "index solution" (ac_solve d_indexAbs me (fn^".index") (get_cstrs me) qs None) None
  |> fst3
  |> d_indexAbs.read

(*
let make_cones me ucs =
  let cs = get_cstrs me |> snd3                                    in
  let cm = cs |>: Misc.pad_fst C.id_of_t |> IM.of_list             in
  let f  = Cindex.data_cones cs                                    in
  foreach ucs begin fun c -> 
    c |> C.id_of_t 
      |> f 
      |> (Ast.Cone.map (fun i -> C.tag_of_t <| IM.safeFind i cm "make_cones")) 
      |> (fun z -> Ast.Cone.Cone [(C.tag_of_t c, z)])
  end
*)

(* API *)
let solve me fn qs =
  (if !Co.prune_index then some <| idx_solve me fn qs else None)
  |> ac_solve d_predAbs me fn (get_cstrs me) qs None
  |> (fun (x, ucs, cones) ->  
       { FI.soln   = d_predAbs.min_read x
       ; FI.unsats = ucs
       ; FI.ucones = cones })
  
let value_var = Ast.Symbol.value_variable Ast.Sort.t_int

let index_of_refc s v cr =
  cr 
  |>  Ctypes.reft_of_refctype
  |>  C.ras_of_reft
  |>: begin function 
       | C.Conc p         -> Sc.index_of_var v (cr, p)
       | C.Kvar (subs, k) -> IndexDomain.read_bind s k
      end
  |>  List.fold_left Index.glb Index.top

let apply sol (vv,t,ras) =
  let su  = Ast.Subst.of_list [(value_var, Ast.eVar vv)] in
  let p2i = if So.is_int t then Sc.pred_of_index_int else Sc.non_null_pred_of_index_ref in
  ras |>: begin function 
            | C.Kvar (subs, k) -> 
                d_indexAbs.read_bind sol k 
                |> p2i 
                |> snd
                |> (fun p -> C.Conc (Ast.substs_pred p su)) 
            | C.Conc _ as ra  -> ra
          end
      |> (fun ras' -> (vv, t, ras'))


let apply_cstr s c = 
  C.make_t 
    (YM.map (apply s) (C.env_of_t c)) 
    (C.grd_of_t c)
    (apply s (C.lhs_of_t c))
    (apply s (C.rhs_of_t c))
    None
    (C.tag_of_t c)

let scalar_solution_check me fn fp s =
  let cs' = me.cm |> SM.map (List.map (apply_cstr s))
                  |> SM.to_list 
                  |> List.map snd 
                  |> List.concat in
  let _   = Pretty.printf "Checking indices...\n" in
  let s'  = ac_scalar_solve d_predAbs me fn fp (get_wfs me, cs', get_deps me)  in
  let _   = Pretty.printf "Indices look sound\n" in 
  ()

let scalar_solve me fn fp =
  let s = ac_scalar_solve d_indexAbs me fn fp (get_cstrs me)  in
  let _ = if !Co.check_is then scalar_solution_check me fn fp s     in
  me.defm
  |> SM.map (List.map (fun (v, cr) -> (v, index_of_refc s v cr)))
  |> SM.map CM.VarMap.of_list
    
