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

module Misc = FixMisc 
module M  = Misc
module SM = Misc.StringMap

open Cil
open Formatcil
open Misc.Ops

let mydebug = false 

type dec =
  | FunDec of string * Cil.fundec * location
  | VarDec of Cil.varinfo * location * init option

(***************************************************************************)
(************************ Name Related Hackery *****************************)
(***************************************************************************)

let is_pure_function s =
  s = "validptr" || 
  s = "csolve_assert" || 
  s = "csolve_assume"

let is_cil_tempvar s = 
     Misc.is_prefix "__cil_tmp" s 
  || Misc.is_prefix "mem_" s 

let suffix_of_fn = fun fn -> "_" ^ fn

let origName_t   = Hashtbl.create 37

let rename_local fn vn = 
  (vn ^ (suffix_of_fn fn))
  >> Misc.flip (Hashtbl.add origName_t) (fn, vn) 


let unrename_local fn vn = 
  let s = suffix_of_fn fn in 
  if not (Misc.is_suffix s vn) then vn else 
    String.sub vn 0 (String.length vn - (String.length s))

let unrename_local vn =
  try 
    snd <| Hashtbl.find origName_t vn
  with Not_found -> 
    let _ = E.warn "Unknown local:%s" vn in
    vn

(***************************************************************************)
(************************ varinfo Maps  ************************************)
(***************************************************************************)

module ComparableVar =
  struct
    type t            = varinfo
    let compare v1 v2 = compare v1.vid v2.vid
    let equal v1 v2   = v1.vid = v2.vid
    let hash          = Hashtbl.hash
    let print ppf v   = Format.fprintf ppf "%s" v.vname
  end

let pretty_to_string f x = f () x |> Pretty.sprint ~width:80 

let pretty_to_format f ppf x = Format.fprintf ppf "%s" (f () x |> Pretty.sprint ~width:80)



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
(********************** Stripping Attributes, Casts, etc. *********************) 
(******************************************************************************)

class removeAttrVisitor = object(self)
  inherit nopCilVisitor
  method vattr _ = ChangeTo []
end

let typStripAttrs (t: Cil.typ) : Cil.typ =
  visitCilType (new removeAttrVisitor :> cilVisitor) t

let exprStripAttrs (e: Cil.exp) : Cil.exp =
  visitCilExpr (new removeAttrVisitor :> cilVisitor) e 

class removeCastVisitor = object(self)
  inherit nopCilVisitor
  method vexpr = function
    | CastE (_, e) -> ChangeDoChildrenPost (e, id) 
    | _            -> DoChildren  
end

let exprStripCasts (e: Cil.exp) : Cil.exp = 
  visitCilExpr (new removeCastVisitor :> cilVisitor) e 
  
(******************************************************************************)
(************************ Ensure Expression/Lval Purity ***********************)
(******************************************************************************)

(** Ensures 1) no expression contains a memory access and another
    operation, 2) all instructions contain at most one memory
    operation. *)
class purifyVisitor (fd: fundec) = object(self)
  inherit nopCilVisitor

  method private mkTemp lv =
    let tmp = makeTempVar fd (typeOfLval lv) in
    let tlv = (Var tmp, NoOffset) in
    let _   = self#queueInstr [Set (tlv, Lval lv, !currentLoc)] in
    tlv



  method vexpr = function
    | Lval (Mem (Lval (Var _, _)), NoOffset) ->
      SkipChildren
    | Lval ((Mem _, _) as lv) ->
      ChangeDoChildrenPost (Lval (self#mkTemp lv), id)
    | _ -> DoChildren

  method vinst = function
    | Set ((Mem _, _) as lvl, Lval ((Mem _, _) as lvr), loc) ->
      ChangeDoChildrenPost ([Set (lvl, Lval (self#mkTemp lvr), loc)], id)
    | _ -> DoChildren
end

let purifyFunction (fd: fundec) =
  fd.sbody <- visitCilBlock (new purifyVisitor fd :> cilVisitor) fd.sbody

let purify file =
  iterGlobals file begin function
    | GFun (fd, _) -> purifyFunction fd
    | _            -> ()
  end


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
(************************* Cil Versions of misc/printers **********************)
(******************************************************************************)

let nprintf a = Pretty.gprintf (fun _ -> Pretty.nil) a

let bprintf b = if b then Pretty.printf else nprintf

let rec d_many_box brk s f () = function
  | []              -> Pretty.nil 
  | [x]             -> Pretty.dprintf "%a" f x
  | x::xs' when brk -> Pretty.dprintf "%a@!%s%a" f x s (d_many_box brk s f) xs'
  | x::xs'          -> Pretty.dprintf "%a@?%s%a" f x s (d_many_box brk s f) xs'

let d_many_box brk l s r f () = function
  | []     -> Pretty.dprintf "[]"
  | xs     -> Pretty.dprintf "@[%s%a%s@]" l (d_many_box brk s f) xs r

let d_many_brackets brk = d_many_box brk "[ " "; " "]"
let d_many_parens brk   = d_many_box brk "( " ", " ")"
(*  Pretty.dprintf "%a" (d_many_box brk "[ " "; " "]" f) x *)
let d_many_braces brk   = d_many_box brk "{ " "; " "}"
let d_opt f () xo       = Misc.maybe_apply (fun x _ -> f () x) xo Pretty.nil
let d_pair fx fy () (x, y) = Pretty.dprintf "%a : %a" fx x fy y




let rec d_many brk s f () = function
  | []              -> Pretty.nil 
  | [x]             -> Pretty.dprintf "%a" f x
  | x::xs' when brk -> Pretty.dprintf "%a%s@?%a" f x s (d_many brk s f) xs'
  | x::xs'          -> Pretty.dprintf "%a%s%a" f x s (d_many brk s f) xs'

(* API *)
let d_formatter f () x =
  Pretty.dprintf "@[%s@]" (Misc.fsprintf f x)

(* API *)
let doc_of_formatter f a =
  d_formatter f () a
  (* RJ, this gets mangled: Misc.fsprintf f a |> Pretty.text *)

(* API *)
let concat_docs = List.fold_left Pretty.concat Pretty.nil

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
let singleAttribute         = "csolve_single"
let finalAttribute          = "csolve_final"
let slocAttribute           = "csolve_sloc"
let globalAttribute         = "csolve_global_loc"
let instantiateAttribute    = "csolve_inst_sloc"
let predAttribute           = "csolve_predicate"
let externOkAttribute       = "csolve_extern_ok"
let checkTypeAttribute      = "csolve_check_type"
let layoutAttribute         = "csolve_layout"
let roomForAttribute        = "csolve_room_for"
let nonnullRoomForAttribute = "csolve_nonnull_room_for"
let ignoreIndexAttribute    = "csolve_ignore_index"
let useIndexAttribute       = "csolve_use_index"
let ignoreBoundAttribute    = "csolve_ignore_bound"
let effectAttribute         = "csolve_effect"
let anyRefAttribute         = "csolve_any_ref"
let anyTypeAttribute        = "csolve_any_type"
let hasRoomAttribute        = "csolve_has_room"         (* TODO: subsume roomForAttribute *)
let nonnullHasRoomAttribute = "csolve_nonnull_has_room" (* TODO: subsume nonnullRoomForAttribute *)
let typeVarAttribute        = "csolve_type_var"
let instantiateTypeVarAttribute = "csolve_inst_tvar"

let has_array_attr     = hasAttribute arrayAttribute
let has_pos_attr       = hasAttribute "pos"
let has_unchecked_attr = hasAttribute "csolve_unchecked"

let is_unchecked_ptr_type t =
  isPointerType t && t |> typeAttrs |> has_unchecked_attr

let cobeginAttribute     = "csolve_cobegin"
let coroutineAttribute   = "csolve_coroutine"
let foreachIterAttribute = "csolve_foreach_iter"
let foreachAttribute     = "csolve_foreach"

let block_has_attribute a b =
  hasAttribute a b.battrs

let stmt_has_attribute a = function
  | {skind = Block b} -> block_has_attribute a b
  | _                 -> false

let is_cobegin_block b =
  stmt_has_attribute cobeginAttribute b

let is_coroutine_block b =
  stmt_has_attribute coroutineAttribute b

let is_foreach_iter_block b =
  stmt_has_attribute foreachIterAttribute b

let is_foreach_block b =
  stmt_has_attribute foreachAttribute b

let block_has_fresh_effects b =
  is_coroutine_block b || is_foreach_iter_block b

let is_parallel_body_block b =
  is_coroutine_block b || is_foreach_iter_block b

let coroutines_of_block = function
  | {skind = Block ({bstmts = ss})} ->
    M.map_partial begin function
      | {skind = If (_, ({bstmts = [{sid = sid; skind = Block b}]}), _, _)}
          when block_has_attribute coroutineAttribute b ->
        Some sid
      | _ -> None
    end ss
  | _ -> E.s <| error "Malformed cobegin block@!@!"

let index_var_of_foreach b = match b.skind with
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

(*
let vm_of_list xs =
  List.fold_left (M.flip (M.uncurry VarMap.add)) VarMap.empty xs

let vm_to_list vm =
  VarMap.fold (fun v x xs -> (v, x) :: xs) vm []

let vm_union vm1 vm2 = VarMap.extend vm2 vm1 
*)


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


(* API *)
let source_files file : string list = 
  let t = Hashtbl.create 7 in
  iterGlobals file begin fun g -> 
    Hashtbl.replace t (get_globalLoc g).file ()
  end;
  Misc.hashtbl_keys t
  |> List.partition (Misc.is_suffix ".h")
  |> (fun (hs, cs) -> cs ++ hs)


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
let is_funptr = fun v -> match unrollType v.vtype with
  | TPtr (TFun (_,_,_,_),_) -> true
  | _ -> false

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
  
(******************************************************************************)
(**********************  Generalizing function pointers ***********************) 
(******************************************************************************)

class fptrAssgnVisitor (g:VarSet.t) = object(self)
  inherit nopCilVisitor
    
  val glob = ref g 
    
  method private update_glob v g = 
    if g then 
      glob := VarSet.add v !glob
    else 
      glob := VarSet.remove v !glob
        
  method private vinst_aux = function
    | v, (Cil.AddrOf (Cil.Var v', Cil.NoOffset)) 
      when is_fun v' -> self#update_glob v v'.Cil.vglob
    | v, (Cil.Lval (Cil.Mem (Cil.CastE (_, Cil.Lval (Cil.Var v', _))), _))
    | v, (Cil.CastE (_, Cil.Lval(Cil.Var v',_)))
    | v, (Cil.Lval (Cil.Mem (Cil.Lval (Cil.Var v', _)), _))
    | v, (Cil.Lval (Cil.Var v',_)) ->
      self#update_glob v (VarSet.mem v' !glob)
    | v, e -> ()
      (* Pretty.printf "%a%b := %a@!" *)
      (* Cil.d_plaintype v.Cil.vtype (is_funptr v) (Cil.printExp Cil.plainCilPrinter) e; assert false *)
      
  method vinst = function
    | Cil.Set ((Cil.Var v, _), e, _) when is_funptr v -> self#vinst_aux (v, e); DoChildren
    | _ -> DoChildren
      
  method get_glob = !glob
      
end
  
let top_level_fn_assgns cil = 
  let fav = new fptrAssgnVisitor VarSet.empty in
  let _   = visitCilFunction (fav :> cilVisitor) cil in
  let glob = fav#get_glob in
  let rec iterate glob = 
    let fav = new fptrAssgnVisitor glob in
    let _ = visitCilFunction (fav :> cilVisitor) cil in
    let glob' = fav#get_glob in
    if VarSet.equal glob glob' then glob else iterate glob' 
  in 
  iterate glob

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
      | (SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _ | AlignOfE _) as e ->
          ChangeTo (e |> constFold true >> fun e -> assert (isConstant e))
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
    v.vname ^ "__csolve_heapify__"

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
      "void * __attribute__ ((csolve_predicate (%p:outRef))) __attribute__ ((csolve_sloc (%p:outSloc))) () (int __attribute__ ((csolve_predicate (%p:inRef))) sz)"
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

(******************************************************************************)
(***************** Reconstructing Original Source from Temps ******************)
(******************************************************************************)

let fieldOfTypeIndex = 
  let t = Hashtbl.create 37 in
  begin fun (ty', n) ->
    match unrollTypeDeep ty' with
    | TPtr ((TComp (ci,_) as ty), _) ->
       if Hashtbl.mem t (ci.cname, 0) then 
         try Some (Hashtbl.find t (ci.cname, n)) with Not_found -> None
       else begin
         ci.cfields
         |> List.map  (fun fi -> (bytesOffset ty (Field (fi, NoOffset)), fi))
         (* >> List.iter (fun (n, fi) -> E.log "fieldsAt: %s at %d is %s \n" ci.cname n fi.fname) *)
         >> List.iter (fun (n, fi) -> Hashtbl.add t (ci.cname, n) fi)
         |> Misc.list_assoc_maybe n
       end
    | _ -> None
  end

let d_field () fi = 
  Pretty.dprintf "%s" fi.fname

let d_varExpr () (v, e) = 
  Pretty.dprintf "%a <- %a" d_var v Cil.d_exp e

let d_varExprMap () m = 
  d_many_brackets true d_varExpr () (VarMap.to_list m)


(* {{{
let fieldOfTypeIndex (ty, n) = 
  fieldOfTypeIndex (ty, n)
  >> E.log "fieldOfTypeIndex: ty = %a, n = %d, fio = %a \n" d_type ty n (d_opt d_field) 

let rec sexyDeref eTop = match eTop with 
  | Lval (Mem (CastE (_, ( BinOp (PlusPI, (CastE (_, e)), (Const (CInt64 (n, _, _))), _)))), NoOffset) (* *((T) e + N) *) 
  | Lval (Mem (CastE (_, ( BinOp (PlusPI, e, (Const (CInt64 (n, _, _))), _)))), NoOffset) (* *((T) e + N) *) 
  -> let e' =  sexyDeref e in
     begin match fieldOfTypeIndex (typeOf e, Int64.to_int n) with 
             | Some fld -> Lval (Mem e', Field (fld, NoOffset))
             | _        -> eTop  
     end
  | Lval (Mem (CastE (_, e)), NoOffset) (* *((T) e) *) 
  -> let e' =  sexyDeref e in
     begin match fieldOfTypeIndex (typeOf e, 0) with 
             | Some fld -> Lval (Mem e', Field (fld, NoOffset))
             | _        -> eTop  
     end

let prettyDerefVar su v = 
  match VarMap.maybe_find v su with
  | Some (BinOp (PlusPI, (CastE (_, e)), (Const (CInt64 (n, _, _))), _)) ->
      (* IF v ---> (_)e + n) THEN *v ===> e -> fld where fld = FLD(typOf(e), n) *)
      begin match fieldOfTypeIndex (typeOf e, Int64.to_int n) with 
             | Some fld -> Some (Mem e, Field (fld, NoOffset))
             | _        -> None
      end
  | None -> 
      (* IF v ---> ??? THEN *v ===> v -> fld where fld = FLD(typOf(v), 0) *)
      begin match fieldOfTypeIndex (v.vtype, 0) with 
             | Some fld -> Some (Mem (Lval (Var v, NoOffset)), Field (fld, NoOffset))
             | _        -> None
      end 

class prettyLvalMapVisitor su = object(self)
  inherit nopCilVisitor
  method vlval lv = match lv with 
    | Mem (CastE (_, (Lval (Var v, NoOffset)))), NoOffset (* *((T) x) *)
    -> begin match prettyDerefVar su v with 
             | Some lv' -> 
                 ChangeTo lv'
             | _        -> 
                 DoChildren 
       end
    | _ -> 
       DoChildren
end


let fieldDerefTx eTop = match eTop with 
  | Lval (Mem (CastE (_, ( BinOp (PlusPI, (CastE (_, e)), (Const (CInt64 (n, _, _))), _)))), NoOffset) (* *((T) e + N) *) 
  | Lval (Mem (CastE (_, ( BinOp (PlusPI, e, (Const (CInt64 (n, _, _))), _)))), NoOffset) (* *((T) e + N) *) 
  -> begin match fieldOfTypeIndex (typeOf e, Int64.to_int n) with 
             | Some fld -> Lval (Mem e, Field (fld, NoOffset))
             | _        -> eTop  
     end
  | Lval (Mem (CastE (_, e)), NoOffset) (* *((T) e) *) 
  -> begin match fieldOfTypeIndex (typeOf e, 0) with 
             | Some fld -> Lval (Mem e, Field (fld, NoOffset))
             | _        -> eTop  
     end
  | _ -> eTop

class fieldDerefVisitor = object(self)
  inherit nopCilVisitor
  (* method vexpr e  = ChangeDoChildrenPost (e, fieldDerefTx) *)
  method vlval lv = ChangeDoChildrenPost (lv, fieldDerefTx)

end

class substVisitor (su : Cil.exp VarMap.t) = object(self)
  inherit nopCilVisitor
  method vexpr = function
    | Lval (Var v, NoOffset) when VarMap.mem v su ->
        ChangeTo (VarMap.find v su)
    | _ -> 
        DoChildren
end

(* API *)
let reSugar_exp (su: Cil.exp VarMap.t) (e: Cil.exp) : Cil.exp =
  e |> visitCilExpr (new substVisitor su)
    |> visitCilExpr (new fieldDerefVisitor)

(* API*)
let reSugar_lval (su: Cil.exp VarMap.t) lv =
  lv |> visitCilLval (new substVisitor su)
     |> visitCilLval (new fieldDerefVisitor)

let updVarMap sur v e =
  e |> exprStripAttrs 
    (* |> exprStripCasts *) 
    |> reSugar_exp !sur
    |> (fun e -> sur := VarMap.add v e !sur)

class tmpVarVisitor (sur : (Cil.exp VarMap.t) ref) = object(self)
  inherit nopCilVisitor
  method vinst = function
    | Set ((Var v, NoOffset), e, _) when is_cil_tempvar v.vname ->
        updVarMap sur v e; 
        DoChildren
    | _ -> 
        DoChildren
end

}}} *)



let fieldDerefTx lv = match lv with 
  | (Mem (CastE (_, ( BinOp (PlusPI, (CastE (_, e)), (Const (CInt64 (n, _, _))), _)))), NoOffset) (* *((T) e + N) *) 
  | (Mem (CastE (_, ( BinOp (PlusPI, e, (Const (CInt64 (n, _, _))), _)))), NoOffset) (* *((T) e + N) *) 
  | (Mem (BinOp (PlusPI, (CastE (_, e)), (Const (CInt64 (n, _, _))), _)), NoOffset) (* *((T) e + N) *) 
  | (Mem (BinOp (PlusPI, e, (Const (CInt64 (n, _, _))), _)), NoOffset) (* *((T) e + N) *) 
  -> begin match fieldOfTypeIndex (typeOf e, Int64.to_int n) with 
             | Some fld -> (Mem e, Field (fld, NoOffset))
             | _        -> lv 
     end
  
  | (Mem (CastE (_, e)), NoOffset) (* *((T) e) *) 
  | (Mem e, NoOffset) (* *((T) e) *) 
  -> begin match fieldOfTypeIndex (typeOf e, 0) with 
             | Some fld -> (Mem e, Field (fld, NoOffset))
             | _        -> lv 
     end
  
  | _ -> lv 

class fieldDerefVisitor = object(self)
  inherit nopCilVisitor
  (* method vexpr e  = ChangeDoChildrenPost (e, fieldDerefTx) *)
  method vlval lv = ChangeDoChildrenPost (lv, fieldDerefTx)
end


class tmpVarVisitor2 (sur : (Cil.exp VarMap.t) ref) = object(self)
  inherit nopCilVisitor
  method vinst = function
    | Set ((Var v, NoOffset), e, _) when is_cil_tempvar v.vname ->
        sur :=  VarMap.add v (exprStripAttrs e) !sur;
        DoChildren
    | _ -> 
        DoChildren
end

class substVisitor (su : Cil.exp VarMap.t) = object(self)
  inherit nopCilVisitor
  
  method vexpr = function
    | Lval (Var v, NoOffset) 
    | StartOf (Var v, NoOffset) ->
        if VarMap.mem v su then
          ChangeTo (VarMap.find v su)
        else DoChildren
    | Lval (Var v, _) ->
       let _ = Pretty.printf "vexpr sees and skips var %s \n" v.vname in
        DoChildren
    | e' ->
       let _ = Pretty.printf "vexpr sees and skips %a \n" d_plainexp e' in
        DoChildren
end

class varUnlocalizeVisitor = object(self)
  inherit nopCilVisitor
  
  method vvrbl v = 
    ChangeTo (copyVarinfo v (unrename_local v.Cil.vname))
end

(*
class transitiveSubstVisitor (su: Cil.exp VarMap.t) = object(self)
  inherit nopCilVisitor
  method vexpr = function
    | Lval (Var v, NoOffset) when VarMap.mem v su ->
        ChangeDoChildrenPost (VarMap.find v su, fieldDeref_expr)
    | _ -> 
        DoChildren
end

let transitiveSubst su e = visitCilExpr (new transitiveSubstVisitor su) e
*)

exception SeenVar

let possibleSubst su e = 
  let s = ref VarSet.empty in
  iterExprVars e begin fun v -> 
    if VarMap.mem v su then s := VarSet.add v !s;
  end;
  !s

let checkSubst su e e' = 
  if e = e' then 
    let vars = possibleSubst su e in 
    let su'  = VarMap.filter (fun v _ -> VarSet.mem v vars) su in
    let _   = Pretty.printf "\nVAR EXPR MAP:\n%a" d_varExprMap su' in
    assertf "NO CHANGE!!!!"
 
let oneSubst su e = 
  visitCilExpr (new substVisitor su) e 
 >> (ignore <.> Pretty.printf "oneSubst e = %a e' = %a\n" d_exp e d_exp)
 (*  >> (checkSubst su e) 
 *)

let doDerefs = 
  visitCilExpr (new fieldDerefVisitor)

let rec transSubst su e = 
  let _ = Pretty.printf "transSubst %a\n" d_exp e in
  if not (VarSet.is_empty (possibleSubst su e))
  then let e' =  (oneSubst su e) in
       transSubst su e'
  else doDerefs e
  
(*
(* API *)
let varExprMap (f: Cil.file) : Cil.exp VarMap.t =
  let sur = ref VarMap.empty                                     in
  let _   = visitCilFile (new tmpVarVisitor2 sur) f              in
  let su' = VarMap.map (transitiveSubst !sur) !sur               in 
  let _   = Pretty.printf "\nVAR EXPR MAP:\n%a" d_varExprMap su' in
  su'

*)

(* API *)
let varExprMap (fs: Cil.fundec list) : Cil.exp VarMap.t =
  let sur = ref VarMap.empty                                     in
  let vis = new tmpVarVisitor2 sur                               in
  let _   = List.iter (ignore <.> visitCilFunction vis) fs       in
  let su' = VarMap.map (transSubst !sur) !sur               in 
  let _   = Pretty.printf "\nVAR EXPR MAP:\n%a" d_varExprMap su' in
  su'

let reSugarVisitors su = [ new substVisitor su
                         ; new fieldDerefVisitor 
                         ; new varUnlocalizeVisitor
                         ]

let visitCilInstrOne = fun vis -> visitCilInstr vis <+> Misc.safeHead "visitCilInstrOne"
let seqVisits        = fun f visitors x -> List.fold_left (fun acc vis -> f vis acc) x visitors 

(* API*)
let reSugar_lval  = seqVisits visitCilLval     <.> reSugarVisitors
let reSugar_exp   = seqVisits visitCilExpr     <.> reSugarVisitors
let reSugar_instr = seqVisits visitCilInstrOne <.> reSugarVisitors

(* 
let reSugar_lval su lv = 
   lv |> visitCilLval (new substVisitor su)
      |> visitCilLval (new fieldDerefVisitor)

(* API *)
let reSugar_exp (su: Cil.exp VarMap.t) (e: Cil.exp) : Cil.exp =
  e |> visitCilExpr (new substVisitor su)
    |> visitCilExpr (new fieldDerefVisitor)
  
(* API *)
let reSugar_instr (su: Cil.exp VarMap.t) i = 
  i |> visitCilInstrOne (new substVisitor su)
    |> visitCilInstrOne (new fieldDerefVisitor)
    (* >> (fun i' -> ignore <| Pretty.printf "reSugar: i = %a ; i' = %a \n" d_instr i d_instr i')
    *)
 *)
(***************************************************************************)
(******************** Source Location Information **************************)
(***************************************************************************)

type srcinfo 
  = Raw of string
  | Lvl of lval     * location 
  | Typ of typ      * location
  | Exp of exp      * location
  | Con of constant * location
  | Var of varinfo  * location
  | Ins of instr    * location 

let d_srcinfo () = function
  | Raw s        -> Pretty.dprintf "%s" s
  | Lvl (l, loc) -> Pretty.dprintf "%a at %a" d_lval l  d_loc loc
  | Typ (t, loc) -> Pretty.dprintf "%a at %a" d_type t  d_loc loc
  | Exp (e, loc) -> Pretty.dprintf "%a at %a" d_exp  e  d_loc loc
  | Con (c, loc) -> Pretty.dprintf "%a at %a" d_const c d_loc loc
  | Var (v, loc) -> Pretty.dprintf "%a at %a" d_var v   d_loc loc
  | Ins (i, loc) -> Pretty.dprintf "%a at %a" d_instr i d_loc loc

let srcinfo_of_lval     = fun l lo -> Lvl (l, Misc.get_option !currentLoc lo)
let srcinfo_of_type     = fun t lo -> Typ (t, Misc.get_option !currentLoc lo)
let srcinfo_of_constant = fun c lo -> Con (c, Misc.get_option !currentLoc lo)
let srcinfo_of_instr    = fun i lo -> Ins (i, Misc.get_option !currentLoc lo)
let srcinfo_of_var      = fun v lo -> Var (v, Misc.get_option !currentLoc lo)
let srcinfo_of_expr     = fun e lo -> Exp (e, Misc.get_option !currentLoc lo)
let srcinfo_of_string   = fun s    -> Raw s 

(*
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
*)

let srcInfot : ((location * string), srcinfo) Hashtbl.t = Hashtbl.create 37 

let lvalKey loc lv = loc, pretty_to_string d_lval lv
let exprKey loc e  = loc, pretty_to_string d_exp e

let setSrcLval loc lv lvSrc = 
  Hashtbl.add srcInfot (lvalKey loc lv) <| srcinfo_of_lval lvSrc (Some loc) 

let setSrcExpr loc e eSrc =
  Hashtbl.add srcInfot (exprKey loc e) <| srcinfo_of_expr eSrc (Some loc) 

let lval_of_srcinfos = Misc.list_first_maybe (function Lvl (x, _) -> Some x | _ -> None)
let exp_of_srcinfos  = Misc.list_first_maybe (function Exp (x, _) -> Some x | _ -> None)
let typ_of_srcinfos  = Misc.list_first_maybe (function Typ (x, _) -> Some x | _ -> None)

let getSrcLval loc lv =
  lvalKey loc lv 
  |> Hashtbl.find_all srcInfot 
  |> lval_of_srcinfos

let getSrcExpr loc e = 
  exprKey loc e 
  |> Hashtbl.find_all srcInfot 
  |> exp_of_srcinfos

