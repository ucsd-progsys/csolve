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

(******************************************************************************)
(********************* Misc Operations on CIL entities ************************)
(******************************************************************************)
module E  = Errormsg 
module M  = Misc
module SM = Misc.StringMap

open Cil
open Misc.Ops
 
module ComparableVar =
  struct
    type t            = varinfo
    let compare v1 v2 = compare v1.vid v2.vid
    let equal v1 v2   = v1.vid = v2.vid
    let hash          = Hashtbl.hash
  end

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
    let _ = error "impure expr: %a" Cil.d_exp e in
    assertf "impure expr"

(******************************************************************************)
(*************************** Wipe Float Expressions ***************************)
(******************************************************************************)

class unfloatVisitor = object(self)
  inherit nopCilVisitor

  method vexpr = function
    | BinOp (_, _, _, TFloat (fk, _)) -> ChangeTo (Const (CReal (0.0, fk, Some "0.0")))
    | Lval _                          -> SkipChildren
    | _                               -> DoChildren
end

let unfloatGlobal = function
  | GFun (fd, _) -> fd.sbody <- visitCilBlock (new unfloatVisitor) fd.sbody
  | _            -> ()

let unfloat file =
  iterGlobals file unfloatGlobal

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
(****************************** Type Manipulation *****************************)
(******************************************************************************)

let int_width   = bytesSizeOfInt IInt
let short_width = bytesSizeOfInt IShort
let char_width  = bytesSizeOfInt IChar

let bytesSizeOf t =
  1 + ((Cil.bitsSizeOf t - 1) / 8)

let bytesOffset t off =
  fst (bitsOffset t off) / 8

let ptrRefType = function
  | TPtr (t, _)      -> t
  | TArray (t, _, _) -> t
  | _                -> failwith "ptrRefType called with non-pointer argument"

let rec typ_width (t: typ): int =
  t |> unrollType |> bytesSizeOf

let bytesSizeOfFloat: fkind -> int = function
  | FFloat      -> 4
  | FDouble     -> 8
  | FLongDouble -> 12

let isVararg (t: typ): bool =
  isFunctionType t &&
    let _, _, vararg, _ = splitFunctionType t in
      vararg

let is_attr s = function Attr (s',_) when s = s' -> true | _ -> false

let is_array_attr     = is_attr "array"
let is_pos_attr       = is_attr "pos"
let is_unchecked_attr = is_attr "unchecked"

let has_array_attr     = fun a -> List.exists is_array_attr a
let has_pos_attr       = fun a -> List.exists is_pos_attr a
let has_unchecked_attr = fun a -> List.exists is_unchecked_attr a

let is_unchecked_ptr_type t =
  isPointerType t && t |> typeSig |> typeSigAttrs |> has_unchecked_attr

let id_of_po = function
  | None   -> ""
  | Some n -> string_of_int n

let id_of_ciltype t po =  
  Pretty.dprintf "%a ### %a ### %s" 
    Cil.d_typsig (Cil.typeSig t) 
    Cil.d_attrlist (Cil.typeAttrs t)
    (id_of_po po)
  |> Pretty.sprint ~width:80
(* Cil.typeSig <+> Cil.d_typsig () <+> Pretty.sprint ~width:80 *)



(******************************************************************************)
(**************************** Misc. Pretty Printers ***************************)
(******************************************************************************)

let d_var () (v: varinfo): Pretty.doc =
  Pretty.text v.vname

(******************************************************************************)
(******************************** Variable Maps *******************************)
(******************************************************************************)

module VarMap = Map.Make(ComparableVar)

  (*
  Map.Make(struct
                           type t = varinfo
                           let compare v1 v2 = compare v1.vid v2.vid
                         end)
 *)
module VarMapPrinter = Pretty.MakeMapPrinter(VarMap)

let vm_print_keys vm =
  VarMapPrinter.d_map ~dmaplet:(fun d1 _ -> d1) ", " d_var (fun () _ -> Pretty.nil) vm

let vm_of_list xs =
  List.fold_left (M.flip (M.uncurry VarMap.add)) VarMap.empty xs

let vm_to_list vm =
  VarMap.fold (fun v x xs -> (v, x) :: xs) vm []

let definedHere vi =
  vi.vdecl.line > 0 && vi.vstorage != Extern

let assertLoc (loc: Cil.location) (b: bool) (fmt : ('a,unit,Pretty.doc) format) : 'a =
  let f d = 
    if b then Pretty.nil else begin
      ignore (Pretty.eprintf "%a: Error: %a@!" Cil.d_loc loc Pretty.insert d);
      assert false
      (* ignore (0/0);
         Pretty.nil *)
    end in
  Pretty.gprintf f fmt



(******************************************************************************)
(************************** Callgraph Construction ****************************)
(******************************************************************************)

module G   = Graph.Imperative.Digraph.Concrete(ComparableVar)
module SCC = Graph.Components.Make(G)
module TRV = Graph.Traverse.Dfs(G)

class bodyVisitor (cg: G.t) (caller: varinfo) = object
  inherit nopCilVisitor

  method vinst = function
    | Call (_, Lval (Var callee, NoOffset), _, _) -> G.add_edge cg caller callee; SkipChildren
    | Call (_, _, _, _)                           -> failwith "Can't generate callgraph for non-variable function"
    | _                                           -> SkipChildren
end

class callgraphVisitor (cg: G.t) = object
  inherit nopCilVisitor

  method vglob = function
    GFun (fundec, _) ->
      G.add_vertex cg fundec.svar;
      visitCilBlock (new bodyVisitor cg fundec.svar) fundec.sbody |> ignore;
      SkipChildren
  | _ -> DoChildren
end

let sccs (f: file) =
  let cg = G.create () in
  let _  = visitCilFile (new callgraphVisitor cg) f in
  SCC.scc_list cg

let reach (f: file) (root : varinfo) =
  let cg = G.create () in
  let _  = visitCilFile (new callgraphVisitor cg) f in
  let rv = ref [] in
  let _  = TRV.prefix_component (fun v -> rv := v :: !rv) cg root in
  let _  = Printf.printf "Reachable funs: %s \n" (String.concat "," (List.map (fun v -> v.vname) !rv)) in
  !rv

let fdec_of_name cil fn = 
  cil.globals 
  |> List.filter (function GFun (f,_) -> f.svar.vname = fn | _ -> false)
  |> (function GFun (f,_) :: _ -> f.svar | _ ->  assertf "Unknown function: %s \n" fn)

let reachable cil =
  match !Constants.root with 
  | "" -> 
      (fun _ -> true)
  | fn -> 
      fn 
      |> fdec_of_name cil 
      |> reach cil
      |> List.map (fun v -> (v.vname, ())) 
      >> (List.map fst <+> String.concat "," <+> Printf.printf "Reachable from %s : %s \n" fn) 
      |> Misc.sm_of_list
      |> Misc.flip SM.mem 


(****************************************************************************************)
(************** Error Message Wrappers **************************************************)
(****************************************************************************************)

let g_error (b: bool) (fmt: ('a, unit, Pretty.doc) format) : 'a = 
  if b then error fmt else warn fmt

let g_errorLoc (b: bool) (loc: location) (fmt: ('a, unit, Pretty.doc) format) : 'a = 
  if b then errorLoc loc fmt else warnLoc loc fmt

let g_halt (b: bool) = 
  if b then E.s else (fun _ -> ())

