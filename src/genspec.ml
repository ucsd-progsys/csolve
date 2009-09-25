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

open Cil
open Misc.Ops

let mydebug = false

let id_of_ciltype   = fun t -> t |> Cil.typeSig |> Cil.d_typsig () |> Pretty.sprint ~width:80
let name_of_ciltype = id_of_ciltype (* fun t -> t |> Cil.d_type () |> Pretty.sprint ~width:80 *)

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
  | None -> Ct.IInt i
  | Some n -> Ct.ISeq (i, n)

let unroll_ciltype t =
  match Cil.unrollType t with
  | TComp (ci, _) -> asserti ci.cstruct "TBD unroll_ciltype: unions";
                     List.map (fun x -> x.ftype) ci.cfields
  | t             -> [t]


let is_array_attr = function Attr ("array",_) -> true | _ -> false
let is_pos_attr   = function Attr ("pos",_) -> true | _ -> false


let add_off off c =
  let i = CilMisc.bytesSizeOf c in
  match off with 
  | Ct.IInt i'    -> Ct.IInt (i+i')
  | Ct.ISeq (_,_) -> E.s <| E.error "add_off %d to periodic offset %a" i Ct.d_index off

let adj_period po idx = 
  match po, idx with
  | None  , _         -> idx
  | Some n, Ct.IInt i -> Ct.ISeq (i, n)
  | _, _              -> assertf "adjust_period: adjusting a periodic index"

let ldesc_of_index_ctypes ts =
(* {{{ *) let _ = if mydebug then List.iter begin fun (i,t) -> 
            Pretty.printf "LDESC ON: %a : %a \n" Ct.d_index i Ct.d_ctype t |> ignore
          end ts in (* }}} *)
  match ts with 
  | [(Ct.ISeq (0,_), t)] -> Ct.LDesc.create [(Ct.ITop, t)]
  | _                    -> Ct.LDesc.create ts 

let index_of_attrs = fun ats -> if List.exists is_pos_attr ats then Ct.ISeq (0, 1) else Ct.ITop 

let conv_cilbasetype = function 
  | TVoid ats      -> Ct.CTInt (0, index_of_attrs ats)
  | TInt (ik, ats) -> Ct.CTInt (bytesSizeOfInt ik, index_of_attrs ats)
  | _              -> assertf "ctype_of_cilbasetype: non-base!"

let rec conv_ciltype me loc (th, st, off) c = 
  match c with
  | TVoid _ | TInt (_,_) ->
      (th, st, add_off off c), [(off, conv_cilbasetype c)]
  | TPtr (c',a) ->
      let po = if List.exists is_array_attr a 
               then Some (CilMisc.bytesSizeOf c') else None in
      let (th', st'), t = conv_ptr me loc (th, st) po c' in
      (th', st', add_off off c), [(off, t)] 
  | TArray (c',_,_) -> 
      conv_cilblock me loc (th, st, off) (Some (CilMisc.bytesSizeOf c')) c'

  | TNamed (ti, _) ->
      conv_ciltype me loc (th, st, off) ti.ttype
  | TComp (_, _) ->
      conv_cilblock me loc (th, st, off) None c
  | _          -> 
      let _ = errorLoc loc "TBD: conv_ciltype: %a \n\n" d_type c in
      assertf "TBD: conv_ciltype" 

and conv_ptr me loc (th, st) po c =
  let tid = id_of_ciltype c in
  let idx = mk_idx po 0 in
  if SM.mem tid th then
    (th, st), Ct.CTRef (SM.find tid th, idx) 
  else
    let l                = Sloc.fresh Sloc.Abstract in
    let th'              = SM.add tid l th in
    let (th'', st', _), its = conv_cilblock me loc (th', st, Ct.IInt 0) po c in
    let b                = ldesc_of_index_ctypes its in
    let st''             = SLM.add l b st' in
    (th'', st''), Ct.CTRef (l, idx)

and conv_cilblock me loc (th, st, off) po c =
(* {{{  *)let _  =
    if mydebug then 
      (let cs = unroll_ciltype c in
       ignore <| Pretty.printf "conv_cilblock: unroll %a \n" d_type c;
       List.iter (fun c' -> ignore <| Pretty.printf "conv_cilblock: into %a \n" d_type c') cs) in (* }}} *)
  c |> unroll_ciltype
    |> Misc.mapfold (conv_ciltype me loc) (th, st, off)
    |> Misc.app_snd Misc.flatten
    |> Misc.app_snd (Misc.map (Misc.app_fst (adj_period po)))

let conv_ciltype x y z c = 
  let _ = if mydebug then ignore <| Pretty.printf "conv_ciltype: %a \n" d_type c in
  conv_ciltype x y z c

let cfun_of_args_ret me fn (loc, t, xts) =
  let _ = Pretty.printf "%a GENSPEC for %s \n" d_loc loc fn in
  try
    let res   = xts |> Misc.map snd3 |> Misc.mapfold (conv_ciltype me loc) (SM.empty, SLM.empty, Ct.IInt 0) in
    let ist   = res |> fst |> snd3 in
    let th    = res |> fst |> fst3 in
    let ts    = res |> snd |> Misc.flatsingles |> Misc.map snd in  
    let args  = Misc.map2 (fun (x,_,_) t -> (x,t)) xts ts in
    let res'  = conv_ciltype me loc (th, ist, Ct.IInt 0) t in 
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

let upd_funm spec funm loc fn = function
  | _ when SM.mem fn spec -> funm
  | _ when is_bltn fn     -> funm
  | TFun (t,xtso,_,_)     -> Misc.sm_protected_add false fn (loc, t, argsToList xtso) funm 
  | _                     -> funm 

let specs_of_file spec cil =
  SM.iter (fun fn _ -> Printf.printf "specs_of_file spec has %s \n" fn) spec;
  SM.empty 
  |> foldGlobals cil begin fun funm -> function
     | GFun (fd, loc)    -> upd_funm spec funm loc fd.svar.vname fd.svar.vtype
     | _                 -> funm 
     end 
  |> foldGlobals cil begin fun funm -> function
     | GVarDecl (v, loc) -> upd_funm spec funm loc v.vname v.vtype
     | _                 -> funm 
     end
  |> SM.mapi (cfun_of_args_ret ())
  |> Misc.sm_bindings
  |> Misc.map_partial (function (x, Some y) -> Some (x,y) | _ -> None)

