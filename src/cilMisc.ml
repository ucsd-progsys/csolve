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
open Formatcil
open Misc.Ops

let mydebug = false 

type dec =
  | FunDec of string * Cil.fundec * location
  | VarDec of Cil.varinfo * location * init option

module ComparableVar =
  struct
    type t            = varinfo
    let compare v1 v2 = compare v1.vid v2.vid
    let equal v1 v2   = v1.vid = v2.vid
    let hash          = Hashtbl.hash
  end

let pretty_to_string f x = x |> f () |> Pretty.sprint ~width:80 

type srcinfo = (* note *) string * (* provenance *) location 

let d_srcinfo () (str, loc) =
  Pretty.dprintf "%s at %a" str d_loc loc

let mk_srcinfo prefix d_x x lo =
  let str = prefix ^ (pretty_to_string d_x x) in
  let loc = Misc.get_option !currentLoc lo in
  (str, loc) 

let srcinfo_of_lval     = mk_srcinfo "lval: " d_lval 
let srcinfo_of_type     = mk_srcinfo "type: " d_type 
let srcinfo_of_constant = mk_srcinfo "constant: " d_const
let srcinfo_of_instr    = mk_srcinfo "instr: " d_instr
let d_varinfo () v      = d_lval () (Var v, NoOffset)
let srcinfo_of_var      = mk_srcinfo "var: " d_varinfo  
let srcinfo_of_string s = ("str: "^s, locUnknown)

(* Does this local var contain an array? *)
let rec containsArray (t:typ) : bool =  (* does this type contain an array? *)
  match unrollType t with
    TArray _ -> true
  | TComp(ci, _) -> (* look at the types of the fields *)
      List.exists (fun fi -> containsArray fi.ftype) ci.cfields
  | _ ->
    (* Ignore other types, including TInt and TPtr.  We don't care whether
       there are arrays in the base types of pointers; only about whether
       this local variable itself needs to be moved to the heap. *)
   false

let isCompoundType t = match unrollType t with
  | TComp _ | TArray _ -> true
  | _                  -> false

let fresh_arg_name, _ = M.mk_string_factory "ARG"

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

type considerStringsPure =
  | StringsArePure
  | StringsAreNotPure

class checkPureVisitor (stringsPure) = object(self)
  inherit nopCilVisitor
  method vlval = function
  | Mem _, _ -> raise ContainsDeref
  | _        -> DoChildren

  method vexpr = function
  | Const (CStr _)
    when stringsPure = StringsAreNotPure -> raise ContainsDeref
  | _                                    -> DoChildren
end

(* API *)
let is_pure_expr stringsPure e =
  try 
    e |> visitCilExpr (new checkPureVisitor (stringsPure)) >| true
  with ContainsDeref ->
    false

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

(******************************************************************************)
(**************** Drilling Into Variable Reference Expressions ****************)
(******************************************************************************)

let rec referenced_var_of_exp = function
  | CastE (_, e)              -> referenced_var_of_exp e
  | Lval (Var v, NoOffset)    -> v
  | StartOf (Var v, NoOffset) -> v
  | _                         -> failwith "referenced_var_of_exp"

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
  try 
    1 + ((Cil.bitsSizeOf t - 1) / 8)
    >> (bprintf mydebug "CM.bytesSizeOf %a = %d \n" d_type t)
  with _ -> E.s <| E.error "bytesSizeOf %a \n" d_type t

let bytesOffset t off =
  fst (bitsOffset t off) / 8

let ptrRefType t = match unrollType t with
  | TPtr (t, _)      -> t
  | TArray (t, _, _) -> t
  | _                -> failwith "ptrRefType called with non-pointer argument"

let rec typ_width (t: typ): int =
  t |> unrollType |> bytesSizeOf

let bytesSizeOfFloat: fkind -> int = function
  | FFloat      -> 4
  | FDouble     -> 8
  | FLongDouble -> 12

let typeName t = match unrollType t with
  | TComp ({cname = n}, _)  -> Some n
  | TNamed ({tname = n}, _) -> Some n
  | _                       -> None

let isVararg (t: typ): bool =
  isFunctionType t &&
    let _, _, vararg, _ = splitFunctionType t in
      vararg

let getAttr name ats = match filterAttributes name ats with
  | [a]    -> a
  | _ :: _ -> failwith <| "Multiple attributes named " ^ name
  | []     -> failwith <| "No attribute named " ^ name

let getStringAttrs name ats =
     ats
  |> filterAttributes name
  |> List.map begin function
      | Attr (_, [AStr s]) -> s
      | _                  -> failwith <| "Attribute " ^ name ^ " given a param which is not a single string"
     end

let setStringAttr name s ats =
     ats
  |> dropAttribute name
  |> addAttribute (Attr (name, [AStr s]))

(* Must be "array" because CIL inserts these itself. *)
let arrayAttribute          = "array"
let finalAttribute          = "lcc_final"
let slocAttribute           = "lcc_sloc"
let globalAttribute         = "lcc_global_loc"
let instantiateAttribute    = "lcc_inst_sloc"
let predAttribute           = "lcc_predicate"
let externOkAttribute       = "lcc_extern_ok"
let checkTypeAttribute      = "lcc_check_type"
let layoutAttribute         = "lcc_layout"
let roomForAttribute        = "lcc_room_for"
let nonnullRoomForAttribute = "lcc_nonnull_room_for"
let ignoreBoundAttribute    = "lcc_ignore_bound"
let writeEffectAttribute    = "lcc_write_effect"
let readEffectAttribute     = "lcc_read_effect"

let has_array_attr     = hasAttribute arrayAttribute
let has_pos_attr       = hasAttribute "pos"
let has_unchecked_attr = hasAttribute "lcc_unchecked"

let is_unchecked_ptr_type t =
  isPointerType t && t |> typeAttrs |> has_unchecked_attr

let cobeginAttribute     = "lcc_cobegin"
let coroutineAttribute   = "lcc_coroutine"
let foreachIterAttribute = "lcc_foreach_iter"
let foreachAttribute     = "lcc_foreach"

let block_has_attribute a b =
  hasAttribute a b.battrs

let ssa_block_has_attribute a b =
  match b.Ssa.bstmt.skind with
    | Block b -> block_has_attribute a b
    | _       -> false

let is_cobegin_ssa_block b =
  ssa_block_has_attribute cobeginAttribute b

let is_coroutine_ssa_block b =
  ssa_block_has_attribute coroutineAttribute b

let is_foreach_iter_ssa_block b =
  ssa_block_has_attribute foreachIterAttribute b

let is_foreach_ssa_block b =
  ssa_block_has_attribute foreachAttribute b

let ssa_block_has_fresh_effects b =
  is_coroutine_ssa_block b || is_foreach_iter_ssa_block b

let is_coroutine_block b =
  block_has_attribute coroutineAttribute b

let is_foreach_iter_block b =
  block_has_attribute foreachIterAttribute b

let is_parallel_body_block b =
  is_coroutine_block b || is_foreach_iter_block b

let coroutines_of_ssa_block b = match b.Ssa.bstmt.skind with
  | Block ({bstmts = ss}) ->
    M.map_partial begin function
      | {skind = If (_, ({bstmts = [{sid = sid; skind = Block b}]}), _, _)}
          when is_coroutine_block b ->
        Some sid
      | _ -> None
    end ss
  | _ -> E.s <| error "Malformed cobegin block@!@!"

let index_var_of_foreach b = match b.Ssa.bstmt.skind with
  | Block ({bstmts = [{skind = Instr [Call (Some (Var v, NoOffset), _, _, _)]}; _]}) -> v
  | _ -> E.s <| error "Malformed foreach block@!"

let is_reference t =
  match Cil.unrollType t with
  | Cil.TPtr _ | Cil.TArray (_,_,_) -> true
  | _ -> false

(*********************************************************************)
(**********************Misc. Pretty Printers *************************)
(*********************************************************************)

let d_var () (v: varinfo): Pretty.doc =
  Pretty.text v.vname

(*********************************************************************)
(******************************** Variable Sets **********************)
(*********************************************************************)

module VarSet = Misc.ESet (ComparableVar)

(*********************************************************************)
(******************************** Variable Maps **********************)
(*********************************************************************)

module VarMap = Misc.EMap (ComparableVar)

module VarMapPrinter = Pretty.MakeMapPrinter(VarMap)

let vm_print_keys vm =
  VarMapPrinter.d_map ~dmaplet:(fun d1 _ -> d1) ", " d_var (fun () _ -> Pretty.nil) vm

let vm_of_list xs =
  List.fold_left (M.flip (M.uncurry VarMap.add)) VarMap.empty xs

let vm_to_list vm =
  VarMap.fold (fun v x xs -> (v, x) :: xs) vm []

let vm_union vm1 vm2 =
  VarMap.fold (fun k v vm -> VarMap.add k v vm) vm1 vm2

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

(*****************************************************************************)
(********************** Iterate over Variables *******************************)
(*****************************************************************************)

class defVarVisitor (f: Cil.varinfo -> unit) = object
  inherit nopCilVisitor
  
  method vglob = function
    | GFun (fd, _)    -> List.iter f ([fd.svar] ++ fd.sformals ++ fd.slocals); SkipChildren
    | GVar (v, _ , _) -> f v; SkipChildren
    | _               -> SkipChildren
end

(* API *)
let iterDefVars (cil: Cil.file) (f: Cil.varinfo -> unit): unit = 
  visitCilFile (new defVarVisitor f) cil

class usedVarVisitor (f: Cil.varinfo -> unit) = object
  inherit nopCilVisitor
  method vvrbl v = f v; SkipChildren 
end

(* API *)
let iterUsedVars (cil: Cil.file) (f: Cil.varinfo -> unit): unit = 
  visitCilFile (new usedVarVisitor f) cil

(* API *)
let iterExprVars (e: Cil.exp) (f: Cil.varinfo -> unit): unit = 
  visitCilExpr (new usedVarVisitor f) e |> ignore

(* API *)
let is_local_expr e =
  let b = ref true in
  let _ = iterExprVars e (fun v -> b := !b && not (v.Cil.vglob)) in
  !b

(* API *)
let is_null_expr = function
  | CastE (TPtr (_, _), (Const (CInt64 (i, _, _))))
    when i = Int64.zero -> true
  | _ -> false

(******************************************************************************)
(********************** Iterate over Expressions ******************************)
(******************************************************************************)

class exprVisitor f = object(self)
  inherit nopCilVisitor
  method vexpr e = if f e then DoChildren else SkipChildren
end

(* API 
let iterConsts cil f = 
  visitCilFile (new exprVisitor (function Const c -> f c; false | _ -> true)) cil
*)

(* API *)
let iterExprs = fun cil f -> visitCilFile (new exprVisitor f) cil

(*
(******************************************************************************)
(********************** Iterate over Constants ********************************)
(******************************************************************************)

class constVisitor f = object(self)
  inherit nopCilVisitor

  method vexpr e = 
    match (* Cil.constFold true *) e with
      | Const c  -> f c; SkipChildren
      | _        -> DoChildren
end

(* API *)
let iterConsts cil f = visitCilFile (new constVisitor f) cil 

*)

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
  |> (function GFun (f,_) :: _ -> f.svar | _ ->  assertf "fdec_of_name: Unknown function: %s \n" fn)

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
      |> SM.of_list
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

(***************************************************************************************)
(*************** Misc. Predicates ******************************************************)
(***************************************************************************************)

let is_fun    = fun v -> match unrollType v.vtype with TFun (_,_,_,_) -> true | _ -> false
let is_scalar = fun v -> match unrollType v.vtype with TInt (_,_) -> true | _ -> false

(***************************************************************************************)
(*************** Cil Summarizers *******************************************************)
(***************************************************************************************)

module type Summarizer =
sig
  type summary =
    {has_prop: bool; metric: int}
  val build_summary: Cil.file -> summary
end

module FunPtrDetector : Summarizer =
  struct
    type summary = {has_prop: bool; metric: int}

    class t = object(self)
      inherit nopCilVisitor
    
      val mutable has_funptr = false
      method get_has_funptr = has_funptr

      val mutable funptrness = 0
      method get_funptrness = funptrness
    
      val typ_is_funptr = function
        | TPtr (TFun _, _) -> true
        | _                -> false
    
      val typ_is_func = function
        | TFun _ -> true
        | _      -> false
    
      method vtyp t : typ visitAction =
        let _ = if typ_is_func t then
          has_funptr <- true in
        DoChildren
    
      method vlval lv =
        let _ = if typeOfLval lv |> typ_is_funptr then
          (has_funptr <- true; funptrness <- funptrness + 1) in
        DoChildren
    
      method vexpr = function
        | AddrOf lv ->
            let _ = if typeOfLval lv |> typ_is_func then
              has_funptr <- true in
            DoChildren
        | _ -> DoChildren
    end

    let build_summary cil =
      let rv = new t in
      let _  = visitCilFile (rv :> cilVisitor) cil in
      {has_prop = rv#get_has_funptr; metric = rv#get_funptrness}
  end

(***************************************************************************************)
(*************** Cil Visitors **********************************************************)
(***************************************************************************************)

module type Visitor =
sig
  val doVisit: Cil.file -> unit 
end

module CopyGlobal: Visitor = struct
  class expVisitor fd = object(self)
    inherit nopCilVisitor

    val mutable expLevel = 0

    val globalMem = Hashtbl.create 17

    method vvrbl v =
      if expLevel > 0 && v.vglob && not (isFunctionType v.vtype) then
        ChangeTo (Misc.do_memo globalMem (fun v -> makeTempVar fd v.vtype) v v)
      else SkipChildren

    method vexpr e =
      expLevel <- expLevel + 1;
      ChangeDoChildrenPost (e, fun e -> expLevel <- expLevel - 1; e)

    method vfunc fd =
      ChangeDoChildrenPost
        (fd,
         begin fun fd ->
           Hashtbl.iter
             (fun v tmp -> self#queueInstr [Set (var tmp, Lval (var v), fd.svar.vdecl)])
             globalMem;
           fd
         end)
  end

  class globVisitor = object(self)
    inherit nopCilVisitor

    method vglob = function
      | GFun (fd, _) -> visitCilFunction (new expVisitor fd :> cilVisitor) fd |> ignore; SkipChildren
      | _            -> SkipChildren
  end

  let doVisit = visitCilFile (new globVisitor)
end

module NameNullPtrs: Visitor = struct
  let fresh_ptr_name =
    let counter = ref 0 in
      fun () -> incr counter; "nullptr" ^ string_of_int !counter

  class funVisitor fd = object(self)
    inherit nopCilVisitor

    method vexpr = function
      | CastE (TPtr (t, attrs), (Const (CInt64 (i, _, _)) as cz)) when i = Int64.zero ->
          let tptr = TPtr (t, addAttribute (Attr (fresh_ptr_name (), [])) attrs) in
            ChangeDoChildrenPost (CastE (tptr, cz), id)
      | _ -> DoChildren
  end

  class globVisitor = object(self)
    inherit nopCilVisitor
      
    method vglob = function
      | GFun (fd, _) -> visitCilFunction (new funVisitor fd :> cilVisitor) fd |> ignore; SkipChildren
      | _            -> SkipChildren
  end

  let doVisit = visitCilFile (new globVisitor)
end

(* pmr: Globally change Mems to mkMem *)
(* Variation on the heapify transformation included with CIL. *)
module Pheapify: Visitor = struct
  class heapifyModifyVisitor hvars = object(self)
    inherit nopCilVisitor
      
    method private is_heapified vi =
      List.mem_assoc vi hvars
        
    method vexpr = function
      | StartOf (Var vi, NoOffset) | AddrOf (Var vi, NoOffset)
        when self#is_heapified vi ->
          let lv = var (List.assoc vi hvars) in
            ChangeTo (if vi.vglob then mkAddrOrStartOf lv else Lval lv)
      | _ -> DoChildren
        
    method vlval = function
      | Var vi, vi_offset when self#is_heapified vi ->
          let hvi = List.assoc vi hvars in
          begin match unrollType vi.vtype with
            | TArray _ ->
                begin match vi_offset with
                  | Index (e, o) -> ChangeDoChildrenPost (mkMem (BinOp (PlusPI, Lval (var hvi), e, hvi.vtype)) o, id)
                  | NoOffset     -> ChangeDoChildrenPost (var hvi, id)
                  | _            -> assert false
                end
            | _ ->
                if vi.vglob then
                  ChangeDoChildrenPost (mkMem (hvi |> var |> mkAddrOrStartOf) vi_offset, id)
                else ChangeDoChildrenPost (mkMem (Lval (var hvi)) vi_offset, id)
          end
      | _ -> DoChildren
  end

  let should_heapify vi =
    if vi.vglob then
      not (isArrayType vi.vtype) && not (isFunctionType vi.vtype)
    else
      (isCompoundType vi.vtype) || (vi.vaddrof && !Constants.heapify_nonarrays)

  let heapifiedType v = match unrollType v.vtype with
    | TArray (t, _, _) -> TPtr (t, [])
    | t when v.vglob   -> TArray (t, Some one, [])
    | t                -> TPtr (t, [])

  let arrayifiedInit v i =
    {init = M.maybe_map (fun ii -> CompoundInit (v.vtype, [Index (integer 0, NoOffset), ii])) i.init}

  let heapName v =
    v.vname ^ "__lcc_heapify__"

  class heapifyGlobalVisitor = object(self)
    inherit nopCilVisitor

    val hglobals = ref []

    method get_hglobals = !hglobals

    method heapifiedGlobal vi =
         {(makeGlobalVar (heapName vi) (heapifiedType vi)) with
            vaddrof = true;
            vstorage = vi.vstorage;
            vattr = vi.vattr}
      >> fun hvi -> hglobals := (vi, hvi) :: !hglobals

    method vglob = function
      | GVar (vi, init, loc) when should_heapify vi ->
        ChangeTo [GVar (self#heapifiedGlobal vi, arrayifiedInit vi init, loc)]
      | GVarDecl (vi, loc) when should_heapify vi ->
        ChangeTo [GVarDecl (self#heapifiedGlobal vi, loc)]
      | _ -> DoChildren
  end

  class heapifyAnalyzeVisitor f alloc globmap = object
    inherit nopCilVisitor

    method vglob = function
      | GFun (fundec, funloc) ->
          let mkVar v = makeLocalVar fundec (heapName v) (heapifiedType v) in
          let hvars   = List.filter should_heapify fundec.slocals in
            if (hvars <> [] || globmap <> []) then begin
              let localvars = List.combine hvars (List.map mkVar hvars) in
              let allocs    = List.map (alloc funloc) localvars in
                visitCilBlock (new heapifyModifyVisitor (localvars @ globmap)) fundec.sbody |> ignore;
                fundec.sbody.bstmts <- mkStmt (Instr allocs) :: fundec.sbody.bstmts;
                fundec.slocals      <- List.filter (fun vi -> not (List.mem vi hvars)) fundec.slocals;
                ChangeTo ([GFun (fundec, funloc)])
            end else DoChildren
      | _ -> DoChildren
  end

  let mallocType =
    cType
      "void * __attribute__ ((lcc_predicate (%p:outRef))) __attribute__ ((lcc_sloc (%p:outSloc))) () (int __attribute__ ((lcc_predicate (%p:inRef))) sz)"
      [("inRef",   Fp (AStr "V >= 0"));
       ("outRef",  Fp (AStr "&& [V > 0; V = BLOCK_BEGIN([V]); BLOCK_END([V]) = (V + sz)]"));
       ("outSloc", Fp (AStr "!L"))]

  let alloc f =
    let alloc_fun = findOrCreateFunc f "malloc" mallocType in
      fun loc (v, hv) -> Call (Some (var hv), Lval (var alloc_fun), [SizeOf v.vtype], loc)

  let doVisit (f : file) =
    let hgv = new heapifyGlobalVisitor in
    let _   = visitCilFile (hgv :> cilVisitor) f in
      visitCilFile (new heapifyAnalyzeVisitor f (alloc f) hgv#get_hglobals) f
end


let tag_of_global = function
  | GType (_,_)    -> "GType"
  | GCompTag (_,_) -> "GCompTag"
  | _              -> "Global"

let dec_of_global = function
    | GFun (fdec, loc) 
      -> Some (FunDec (fdec.svar.vname, fdec, loc))
    | GVar (v, ii, loc) when not (isFunctionType v.vtype)
      -> Some (VarDec (v, loc, ii.init))
    | GVarDecl (v, loc) when not (isFunctionType v.vtype) 
      -> Some (VarDec (v, loc, None)) 
    | GVarDecl (_, _) | GType _ | GCompTag _ | GCompTagDecl _|
    GText _ | GPragma _
      -> None
    | _ when !Constants.safe                   
      -> assertf "decs_of_file"
    | g  -> (ignore <| E.warn "Ignoring %s: %a \n" (tag_of_global g) d_global g; None) 

(******************************************************************************)
(********************** Printing Without Block Attributes *********************)
(******************************************************************************)

class noBlockAttrPrinterClass : cilPrinter = object (self)
  inherit defaultCilPrinterClass as super

  method pBlock () (blk: block) =
    super#pBlock () {blk with battrs = []}
end

let noBlockAttrPrinter = new noBlockAttrPrinterClass
