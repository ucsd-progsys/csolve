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
module SM  = Misc.StringMap
module SLM = Sloc.SlocMap
module CM  = CilMisc

open Cil
open Misc.Ops

let mydebug = false 

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

let mk_idx po i =
  match po with 
  | None   -> Ct.IInt i
  | Some n -> Ct.ISeq (i, n, Ct.Pos)

let unroll_ciltype t =
  match Cil.unrollType t with
  | TComp (ci, _) -> asserti ci.cstruct "TBD unroll_ciltype: unions";
                     List.map (fun x -> x.ftype) ci.cfields
  | t             -> [t]

let add_off off c =
  let i = CilMisc.bytesSizeOf c in
  match off with 
  | Ct.IInt i' -> Ct.IInt (i+i')
  | Ct.ISeq _  -> E.s <| E.error "add_off %d to periodic offset %a" i Ct.d_index off

let adj_period po idx = 
  match po, idx with
  | None  , _         -> idx
  | Some n, Ct.IInt i -> Ct.ISeq (i, n, Ct.Pos)
  | _, _              -> assertf "adjust_period: adjusting a periodic index"

let ldesc_of_index_ctypes ts =
(* {{{ *) let _ = if mydebug then List.iter begin fun (i,t) -> 
            Pretty.printf "LDESC ON: %a : %a \n" Ct.d_index i Ct.d_ctype t |> ignore
          end ts in (* }}} *)
Ct.LDesc.create ts
(* match ts with 
  | [(Ct.ISeq (0,_), t)] -> Ct.LDesc.create [(Ct.ITop, t)] 
  | [(Ct.ISeq (0,n), t)] -> Ct.LDesc.create [(Ct.ISeq (0,n), t)] 
  | _                    -> Ct.LDesc.create ts 
*)

let index_of_attrs = fun ats -> if CM.has_pos_attr ats then Ct.index_nonneg else Ct.index_top

let conv_cilbasetype = function 
  | TVoid ats        -> Ct.CTInt (0, index_of_attrs ats)
  | TInt (ik, ats)   -> Ct.CTInt (bytesSizeOfInt ik, index_of_attrs ats)
  | TFloat (fk, ats) -> Ct.CTInt (CM.bytesSizeOfFloat fk, index_of_attrs ats)
  | TEnum (ei, ats)  -> Ct.CTInt (bytesSizeOfInt ei.ekind, index_of_attrs ats)
  | _                -> assertf "ctype_of_cilbasetype: non-base!"

type type_level =
  | TopLevel
  | InStruct

let rec conv_ciltype loc tlev (th, st, off) (c, a) =
  match c with
  | TVoid _ | TInt (_,_) | TFloat _ | TEnum _ ->
      (th, st, add_off off c), [(off, conv_cilbasetype c)]
  | TPtr (c',a') ->
      let po = if CM.has_array_attr (a' ++ a) 
               then Some (CilMisc.bytesSizeOf c') else None in
      let (th', st'), t = conv_ptr loc (th, st) po c' in
      (th', st', add_off off c), [(off, t)] 
  | TArray (c',_,_) when tlev = InStruct ->
      conv_cilblock loc (th, st, off) (Some (CilMisc.bytesSizeOf c')) c'
  | TArray (c',_,_) when tlev = TopLevel ->
      let (th', st'), t = conv_ptr loc (th, st) (Some (CilMisc.bytesSizeOf c')) c' in
      (th', st', add_off off c), [(off, t)] 
  | TNamed (ti, a') ->
      conv_ciltype loc tlev (th, st, off) (ti.ttype, a' ++ a)
  | TComp (_, _) ->
      conv_cilblock loc (th, st, off) None c
  | _          -> 
      let _ = errorLoc loc "TBD: conv_ciltype: %a \n\n" d_type c in
      assertf "TBD: conv_ciltype" 

and conv_ptr loc (th, st) po c =
  let tid = CM.id_of_ciltype c po in
  let _   = if mydebug then Format.printf "GENSPEC: id_of_ciltype: %s \n" tid in
  if SM.mem tid th then
    let l, idx           = SM.find tid th in 
    (th, st), Ct.CTRef (l, idx) 
  else
    let l                = Sloc.fresh Sloc.Abstract in
    let idx              = mk_idx po 0 in
    let th'              = SM.add tid (l, idx) th in
    let (th'', st', _), its = conv_cilblock loc (th', st, Ct.IInt 0) po c in
    let b                = ldesc_of_index_ctypes its in
    let st''             = SLM.add l b st' in
    (th'', st''), Ct.CTRef (l, idx)

and conv_cilblock loc (th, st, off) po c =
(* {{{  *)let _  =
    if mydebug then 
      (let cs = unroll_ciltype c in
       ignore <| Pretty.printf "conv_cilblock: unroll %a \n" d_type c;
       List.iter (fun c' -> ignore <| Pretty.printf "conv_cilblock: into %a \n" d_type c') cs) in (* }}} *)
  c |> unroll_ciltype
    |> Misc.map (fun c' -> (c', []))
    |> Misc.mapfold (conv_ciltype loc InStruct) (th, st, off)
    |> Misc.app_snd Misc.flatten
    |> Misc.app_snd (Misc.map (Misc.app_fst (adj_period po)))

let conv_ciltype y tlev z c =
  let _ = if mydebug then ignore <| Pretty.printf "conv_ciltype: %a \n" d_type c in
  conv_ciltype y tlev z (c, [])

let cfun_of_args_ret fn (loc, t, xts) =
  let _ = if mydebug then ignore <| Format.printf "GENSPEC: process %s \n" fn in
  try
    let res   = xts |> Misc.map snd |> Misc.mapfold (conv_ciltype loc InStruct) (SM.empty, SLM.empty, Ct.IInt 0) in
    let ist   = res |> fst |> snd3 in
    let th    = res |> fst |> fst3 in
    let ts    = res |> snd |> Misc.flatsingles |> Misc.map snd in  
    let args  = Misc.map2 (fun (x,_) t -> (x,t)) xts ts in
    let res'  = conv_ciltype loc InStruct (th, ist, Ct.IInt 0) t in
    let ost   = res' |> fst |> snd3 in
    let ret   = res' |> snd |> function [(_,t)] -> t | _ -> E.s <| errorLoc loc "Fun %s has multi-outs (record) %s" fn in
    let qlocs = SLM.fold (fun l _ locs -> l :: locs) ost [] in
    Some (Ct.mk_cfun qlocs args ret ist ost)
  with ex -> 
    let _ = E.warn "Genspec fails on (%s) with exception (%s) \n" fn (Printexc.to_string ex) in
    None

let is_bltn = Misc.is_prefix "__builtin"

let argsToList xtso = 
  xtso 
  |> Cil.argsToList 
  |> Misc.mapi (fun i -> Misc.app_fst3 (function "" | " " -> "x"^(string_of_int i) | s -> s))
  |> Misc.map (fun (x,y,_) -> (x,y))

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
      begin match conv_ciltype loc TopLevel (SM.empty, st, Ct.IInt 0) t with
        | (_, st, _), [(_, ct)] ->
            (st, Misc.sm_protected_add false vn ct varm)
        | _ -> halt <| errorLoc loc "Cannot specify globals of record type (%a)\n" d_type t
      end
  | _ -> (st, varm)

let vars_of_file cil =
  foldGlobals cil begin fun acc g -> match g with
    | GVarDecl (v, _) | GVar (v, _, _) when not (isFunctionType v.vtype) -> SM.add v.vname g acc
    | _                                                                  -> acc
  end SM.empty

let globalspecs_of_varm varspec varm =
     (SLM.empty, SM.empty)
  |> SM.fold begin fun _ t (st, varm) -> match t with
       | GVarDecl (v, loc) | GVar (v, _, loc) -> upd_varm varspec (st, varm) loc v.vname v.vtype
       | _                                    -> (st, varm)
     end varm
  |> fun (st, varm) -> (st, Misc.sm_bindings varm)

(***************************************************************************)
(********************************* API *************************************)
(***************************************************************************)

let specs_of_file_all (funspec, varspec, storespec) cil =
  let storespec, varspec = vars_of_file cil |> globalspecs_of_varm varspec in
    (Misc.sm_extend (fundefs_of_file cil) (fundecs_of_file cil) |> funspecs_of_funm funspec,
     varspec, storespec)

let specs_of_file_dec (funspec, varspec, storespec) cil =
  let storespec, varspec = vars_of_file cil |> globalspecs_of_varm varspec in
    (fundecs_of_file cil |> funspecs_of_funm funspec, varspec, storespec)

