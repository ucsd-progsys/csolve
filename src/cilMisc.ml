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

(****************************************************************)
(************** Misc Operations on CIL entities *****************)
(****************************************************************)

module M = Misc

open Cil
open Misc.Ops
 
(******************************************************************************)
(************************ Ensure Expression/Lval Purity ***********************)
(******************************************************************************)

(** Ensures no expression contains a memory access and another
    operation. *)
class purifyVisitor (fd: fundec) = object(self)
  inherit nopCilVisitor

  method vexpr = function
    | Lval (Mem (Lval (Var _, _)), NoOffset) ->
        SkipChildren
    | Lval ((Mem _, _) as lv) ->
        let tmp = makeTempVar fd (typeOfLval lv) in
        let tlv = (Var tmp, NoOffset) in
        let _   = self#queueInstr [Set (tlv, Lval lv, !currentLoc)] in
          ChangeDoChildrenPost (Lval tlv, id)
    | _ -> DoChildren
end

let purifyFunction (fd: fundec) =
  fd.sbody <- visitCilBlock (new purifyVisitor fd) fd.sbody

let doGlobal = function
  | GFun (fd, _) -> purifyFunction fd
  | _            -> ()

let purify file =
  iterGlobals file doGlobal

(**********************************************************)
(********** Check Expression/Lval Purity ******************)
(**********************************************************)

exception ContainsDeref

class checkPureVisitor = object(self)
  inherit nopCilVisitor
  method vlval = function
  | (Mem _), _ -> raise ContainsDeref 
  | _          -> DoChildren
end

(* API *)
let check_pure_expr e =
  try visitCilExpr (new checkPureVisitor) e |> ignore
  with ContainsDeref ->
    let _ = Errormsg.error "impure expr: %a" Cil.d_exp e in
    assertf "impure expr"

(**********************************************************)
(********** Stripping Casts from Exprs, Lvals *************)
(**********************************************************)

class castStripVisitor = object(self)
  inherit nopCilVisitor
    method vexpr = function
      | CastE (_, e) -> ChangeDoChildrenPost (e, id)
      | _            -> DoChildren
end

(* API *)
let stripcasts_of_lval = visitCilLval (new castStripVisitor)
let stripcasts_of_expr = visitCilExpr (new castStripVisitor)

(******************************************************************************)
(********************************** Printing **********************************)
(******************************************************************************)

(* API *)
let doc_of_formatter f a =
  Misc.fsprintf f a |> Pretty.text

let nprintf a = Pretty.gprintf (fun _ -> Pretty.nil) a

let bprintf b = if b then Pretty.printf else nprintf

(******************************************************************************)
(***************************** Type Maniupulation *****************************)
(******************************************************************************)

let bytesSizeOf t =
  1 + ((Cil.bitsSizeOf t - 1) / 8)

let ptrRefType = function
  | TPtr (t, _)      -> t
  | TArray (t, _, _) -> t
  | _                -> failwith "ptrRefType called with non-pointer argument"

let rec typ_width (t: typ): int =
  match unrollType t with
    | TInt (ik, _)                  -> bytesSizeOfInt ik
    | TPtr _                        -> typ_width !upointType
    | TComp (ci, _) when ci.cstruct -> List.fold_left (fun w fi -> w + typ_width fi.ftype) 0 ci.cfields
    | t                             -> Errormsg.s <| Errormsg.bug "Unimplemented typ_width: %a@!@!" d_type t

(******************************************************************************)
(**************************** Misc. Pretty Printers ***************************)
(******************************************************************************)

let d_var () (v: varinfo): Pretty.doc =
  Pretty.text v.vname

(******************************************************************************)
(******************************** Variable Maps *******************************)
(******************************************************************************)

module VarMap = Map.Make(struct
                           type t = varinfo
                           let compare v1 v2 = compare v1.vid v2.vid
                         end)

module VarMapPrinter = Pretty.MakeMapPrinter(VarMap)

let vm_print_keys vm =
  VarMapPrinter.d_map ~dmaplet:(fun d1 _ -> d1) ", " d_var (fun () _ -> Pretty.nil) vm

let vm_of_list xs =
  List.fold_left (M.flip (M.uncurry VarMap.add)) VarMap.empty xs

let vm_to_list vm =
  VarMap.fold (fun v x xs -> (v, x) :: xs) vm []


let definedHere vi =
  vi.vdecl.line > 0 && vi.vstorage != Extern
