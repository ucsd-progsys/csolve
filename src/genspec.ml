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

(********************************************************************************)
(*************** Generating Specifications **************************************)  
(********************************************************************************)

module E   = Errormsg 
module F   = Format
module Ct  = Ctypes
module N   = Ct.Index
module SM  = Misc.StringMap
module CM  = CilMisc
module Cs  = Ct.RefCTypes.Spec

open Cil
open Misc.Ops

let mydebug = false

exception CantConvert

type period = 
  | Nop
  | Unb of int          (* Unb m    == forall 0 <= i: m * i     *)
  | Bnd of int * int    (* Bnd m, k == forall 0 <= i < k: m * i *)

(*************************************************************************************)
(* {{{ DO NOT DELETE
 * Unused code to determine if a type is recursive
 
let rec unroll_typinfo ti tm =
  if SM.mem ti.tname tm then tm else 
    tm |> Misc.sm_adds ti.tname (name_of_ciltype ti.ttype) 
       |> unroll_type ti.ttype 

and unroll_type t tm  = 
  let tid = name_of_ciltype t in
  if SM.mem tid tm then tm else 
    match t with
    | TVoid _ | TInt (_,_) | TFloat (_,_) ->
        Misc.sm_adds tid "base" tm
    
    | TPtr (t',_) | TArray (t',_,_) ->
        tm |> Misc.sm_adds tid (name_of_ciltype t')
           |> unroll_type t'
    
    | TComp (ci, _) ->
        List.fold_left begin fun tm f -> 
          Misc.sm_adds tid (name_of_ciltype f.ftype) tm 
          |> unroll_type f.ftype
        end tm ci.cfields
    
    | _ -> assertf "TBD: unroll_type %s" (name_of_ciltype t)

let mk_type_graph cil =
  SM.empty
  |> foldGlobals cil begin fun tm g -> match g with 
       | GType (ti,_)  -> unroll_type ti.ttype tm
       | GCompTag (ci,_) when not ci.cstruct -> assertf "mk_type_graph"
       | _ -> tm 
     end 
  >> SM.iter (fun k vs -> Printf.printf "%s :=> %s \n" k (String.concat " , " vs))

let print_type_graph cil = 
  Printf.printf "Type Graph \n";
  cil |> mk_type_graph 
      |> SM.iter (fun k vs -> Printf.printf "%s :=> %s \n" k (String.concat " , " vs))

let is_cyclic =
  let rec reach g src trg vism = 
    if SM.mem src vism then (false, vism) else
      (try SM.find src g with Not_found -> [])
      |> List.fold_left begin fun (ok, vism) v ->
           if ok then (ok, vism) else 
             if v = trg then (true, vism) else
               reach g v trg vism
         end (false, SM.add src () vism) in
  let memo = Hashtbl.create 17 in 
  fun g trg -> 
    let rv = Misc.do_memo memo (fun trg -> reach g trg trg SM.empty |> fst) trg trg in
    let _  = Printf.printf "is_cyclic %s = %b \n" trg rv in
    rv

 }}} *)

let period_of_ciltype = function
  | TArray (c',_,_) as c ->
      let m = CM.bytesSizeOf c' in
      let k = (CM.bytesSizeOf c) / m in
      Bnd (m, k)
  | _                   -> assertf "period_of_ciltype: non-array argument"

let id_of_period = function
  | Nop        -> ""
  | Unb m      -> Printf.sprintf "[%d]" m
  | Bnd (m, k) -> Printf.sprintf "[%d < %d]" m k 

let id_of_ciltype t pd =  
  Pretty.dprintf "%a ### %a ### %s" 
    d_typsig (typeSig t) 
    d_attrlist (typeAttrs t)
    (id_of_period pd)
  |> Pretty.sprint ~width:80
(* Cil.typeSig <+> Cil.d_typsig () <+> Pretty.sprint ~width:80 *)

let mk_idx pd i =
  match pd with 
  | Nop        -> N.IInt i
  | Unb n      -> N.mk_sequence i n (Some i) None
  | Bnd (n, k) -> N.mk_sequence i n (Some i) (Some (i + (k - 1) * n))

let unroll_ciltype off t =
  match Cil.unrollType t with
  | TComp (ci, _) ->
      asserti ci.cstruct "TBD unroll_ciltype: unions";
      List.map begin fun x ->
        (x.ftype, N.plus off (mk_idx Nop (fst (bitsOffset t (Field (x, NoOffset))) / 8)))
      end ci.cfields
  | t -> [(t, off)]

let add_off off c =
  let i = CM.bytesSizeOf c in
  match off with 
  | N.IInt i'   -> N.IInt (i+i')
  | N.ICClass _ -> E.s <| E.error "add_off %d to periodic offset %a" i N.d_index off

let adj_period pd idx = 
  match pd, idx with
  | Nop  , _             -> idx
  | Unb n,      N.IInt i -> N.mk_sequence i n (Some i) None
  | Bnd (n, k), N.IInt i -> N.mk_sequence i n (Some i) (Some (i + (k - 1) * n))
  | _,          _        -> assertf "adjust_period: adjusting a periodic index"

let ldesc_of_index_ctypes loc ts =
(* {{{ *) let _ = if mydebug then List.iter begin fun (i,t) -> 
            Pretty.printf "LDESC ON: %a : %a \n" N.d_index i Ct.I.CType.d_ctype t |> ignore
          end ts in (* }}} *)
     ts
  |> List.map (fun (i, t) -> (i, Ct.I.Field.create Ct.Nonfinal t))
  |> Ct.I.LDesc.create loc

(* match ts with 
  | [(Ct.ISeq (0,_), t)] -> Ct.LDesc.create [(Ct.ITop, t)] 
  | [(Ct.ISeq (0,n), t)] -> Ct.LDesc.create [(Ct.ISeq (0,n), t)] 
  | _                    -> Ct.LDesc.create ts 
*)


type type_level =
  | TopLevel
  | InStruct

let argsToList xtso = 
     xtso 
  |> Cil.argsToList 
  |> Misc.mapi (fun i -> Misc.app_fst3 (function "" | " " -> "x"^(string_of_int i) | s -> s))
  |> Misc.map (fun (x,y,_) -> (x,y))

let rec conv_ciltype_aux loc tlev (th, st, off) (c, a) =
  try
    match c with
      | TVoid _ | TInt (_,_) | TFloat _ | TEnum _ ->
          (th, st, add_off off c), [(off, CilInterface.ctype_of_cilbasetype c)]
      | TPtr (TFun (t, xtso, _, _), _) ->
          let (th, st), t = conv_funptr loc (th, st) t (argsToList xtso) in
            (th, st, add_off off c), [(off, t)]
      | TPtr (c',a') ->
          let pd = if CM.has_array_attr (a' ++ a) then Unb (CM.bytesSizeOf c') else Nop in
          let (th', st'), t = conv_ptr loc (th, st) pd c' in
          (th', st', add_off off c), [(off, t)] 
      | TArray (c',_,_) when tlev = InStruct ->
          conv_cilblock loc (th, st, off) (period_of_ciltype c) c'
      | TArray (c',_,_) when tlev = TopLevel ->
          let (th', st'), t = conv_ptr loc (th, st) (period_of_ciltype c) c' in
          (th', st', add_off off c), [(off, t)] 
      | TNamed (ti, a') ->
          conv_ciltype_aux loc tlev (th, st, off) (ti.ttype, a' ++ a)
      | TComp (_, _) ->
          conv_cilblock loc (th, st, off) Nop c
     | _ -> 
          halt <| errorLoc loc "TBD: conv_ciltype_aux: %a \n\n" d_type c
  with Ct.I.LDesc.TypeDoesntFit (i, ct, ld) ->
    let _ = errorLoc loc "Failed converting CIL type %a\n" d_type c in
    let _ = errorLoc loc "Can't fit %a -> %a in location %a\n" N.d_index i Ct.I.CType.d_ctype ct Ct.I.LDesc.d_ldesc ld in
      raise CantConvert

and conv_funptr loc (th, st) t xts =
  match cfun_of_args_ret "(function pointer)" (loc, t, xts) with
    | None    -> raise CantConvert
    | Some cf ->
        let l = Sloc.fresh_abstract () in
          ((th, Ct.I.Store.Function.add st l cf), Ct.Ref (l, N.IInt 0))

and conv_ptr loc (th, st) pd c =
  let tid = id_of_ciltype c pd in
  let _   = if mydebug then Format.printf "GENSPEC: id_of_ciltype: %s \n" tid in
  if SM.mem tid th then
    let l, idx           = SM.find tid th in 
    (th, st), Ct.Ref (l, idx) 
  else
    let l                = Sloc.fresh_abstract () in
    let idx              = mk_idx pd 0 in
    let th'              = SM.add tid (l, idx) th in
    let (th'', st', _), its = conv_cilblock loc (th', st, N.IInt 0) pd c in
    let b                = ldesc_of_index_ctypes loc its in
    let st''             = Ct.I.Store.Data.add st' l b in
    (th'', st''), Ct.Ref (l, idx)

and conv_cilblock loc (th, st, off) pd c =
(* {{{  *)let _  =
    if mydebug then 
      (let cs = unroll_ciltype off c in
       ignore <| Pretty.printf "conv_cilblock: unroll %a \n" d_type c;
       List.iter (fun (c', _) -> ignore <| Pretty.printf "conv_cilblock: into %a \n" d_type c') cs) in (* }}} *)
  c |> unroll_ciltype off
    |> Misc.mapfold (fun (th, st, _) (c', off) -> conv_ciltype_aux loc InStruct (th, st, off) (c', [])) (th, st, off)
    |> Misc.app_snd Misc.flatten
    |> Misc.app_snd (Misc.map (Misc.app_fst (adj_period pd)))

and conv_ciltype y tlev z c =
    let _ = if mydebug then ignore <| Pretty.printf "conv_ciltype: %a \n" d_type c in
      conv_ciltype_aux y tlev z (c, [])

and conv_arg loc z (name, c) =
  try
    conv_ciltype loc InStruct z c
  with CantConvert ->
    E.s <| errorLoc loc "Failed to generate type for argument %s\n" name

and conv_ret fn loc z c =
  try
    conv_ciltype loc InStruct z c
  with CantConvert ->
    E.s <| errorLoc loc "Failed to generate type for return value of function %s\n" fn

and cfun_of_args_ret fn (loc, t, xts) =
  let _ = if mydebug then ignore <| Format.printf "GENSPEC: process %s \n" fn in
  try
    let res   = Misc.mapfold (conv_arg loc) (SM.empty, Ct.I.Store.empty, N.IInt 0) xts in
    let ist   = res |> fst |> snd3 in
    let th    = res |> fst |> fst3 in
    let ts    = res |> snd |> Misc.flatsingles |> Misc.map snd in  
    let args  = Misc.map2 (fun (x,_) t -> (x,t)) xts ts in
    let res'  = conv_ret fn loc (th, ist, N.IInt 0) t in
    let ost   = res' |> fst |> snd3 in
    let ret   = res' |> snd |> function [(_,t)] -> t | _ -> E.s <| errorLoc loc "Fun %s has multi-outs (record) %s" fn in
    Some (Ct.I.CFun.make args ret ist ost)
  with ex -> 
    let _ = E.warn "Genspec fails on (%s) with exception (%s) \n" fn (Printexc.to_string ex) in
    None

let is_bltn = Misc.is_prefix "__builtin"

let upd_funm spec funm loc fn = function
  | _ when SM.mem fn spec -> funm
  | _ when is_bltn fn     -> funm
  | TFun (t,xtso,_,_)     -> Misc.sm_protected_add false fn (cfun_of_args_ret fn (loc, t, argsToList xtso)) funm
  | _                     -> funm 

let fundefs_of_file cil = 
  foldGlobals cil begin fun acc -> function
    | GFun (fd,_) as g -> SM.add fd.svar.vname g acc
    | _                -> acc
  end SM.empty

let fundecs_of_file cil = 
  let fdefm = fundefs_of_file cil in
  foldGlobals cil begin fun acc -> function
    | GVarDecl (v,_) as g -> if SM.mem v.vname fdefm then acc else SM.add v.vname g acc
    | _                   -> acc
  end SM.empty 

let funspecs_of_funm funspec funm =
  SM.empty
  |> SM.fold begin fun _ d funm -> match d with 
     | GFun (fd, loc)    -> upd_funm funspec funm loc fd.svar.vname fd.svar.vtype
     | GVarDecl (v, loc) -> upd_funm funspec funm loc v.vname v.vtype
     end funm 
  |> Misc.sm_bindings
  |> Misc.map_partial (function (x, Some y) -> Some (x,y) | _ -> None)

let upd_varm spec (st, varm) loc vn = function
  | _ when SM.mem vn spec         -> (st, varm)
  | t when not (isFunctionType t) ->
      begin try
        begin match conv_ciltype loc TopLevel (SM.empty, st, N.IInt 0) t with
          | (_, st, _), [(_, ct)] ->
              (st, Misc.sm_protected_add false vn ct varm)
          | _ -> halt <| errorLoc loc "Cannot specify globals of record type (%a)\n" d_type t
        end
      with CantConvert ->
        E.s <| errorLoc loc "Failed to generate type for global %s\n" vn
      end
  | _ -> (st, varm)

let vars_of_file cil =
  foldGlobals cil begin fun acc g -> match g with
    | GVarDecl (v, _) | GVar (v, _, _) when not (isFunctionType v.vtype) -> SM.add v.vname g acc
    | _                                                                  -> acc
  end SM.empty

let globalspecs_of_varm varspec varm =
  (Ct.I.Store.empty, SM.empty)
  |> SM.fold begin fun _ t (st, varm) -> match t with
       | GVarDecl (v, loc) | GVar (v, _, loc) -> upd_varm varspec (st, varm) loc v.vname v.vtype
       | _                                    -> (st, varm)
     end varm
  |> fun (st, varm) -> (st, Misc.sm_bindings varm)

(***************************************************************************)
(********************************* API *************************************)
(***************************************************************************)

let specs_of_file_all spec cil =
  let st, vr = vars_of_file cil 
               |> globalspecs_of_varm (Cs.varspec spec) in
  let fn     = Misc.sm_extend (fundefs_of_file cil) (fundecs_of_file cil) 
               |> funspecs_of_funm (Cs.funspec spec) in
  (fn, vr, st)

let specs_of_file_dec spec cil =
  let st, vr = vars_of_file cil    
               |> globalspecs_of_varm (Cs.varspec spec) in
  let fn     = fundecs_of_file cil 
               |> funspecs_of_funm (Cs.funspec spec) in 
  (fn, vr, st)

let spec_of_type loc t =
  try match conv_ciltype loc TopLevel (SM.empty, Ct.I.Store.empty, N.IInt 0) t with
      | (_, st, _), [(_, ct)] -> ct, st
      | _ -> raise CantConvert 
  with CantConvert -> 
    (warnLoc loc "Genspec.spec_of_type fails on: %a \n" d_type t; raise CantConvert)

