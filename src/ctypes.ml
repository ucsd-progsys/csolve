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

module Misc = FixMisc 
module P   = Pretty
module E   = Errormsg
module S   = Sloc
module SS  = S.SlocSet
module N   = Index
module C   = Cil
module CM  = CilMisc
module FC  = FixConstraint
module SM  = Misc.StringMap
module SLM = S.SlocMap
module Sv  = Svar
module SVM = Sv.SvarMap
module SVS = Sv.SvarSet
  
module SLMPrinter = P.MakeMapPrinter(SLM)

open Misc.Ops

let mydebug = false

(******************************************************************************)
(*********************************** Indices **********************************)
(******************************************************************************)


module IndexSetPrinter = P.MakeSetPrinter (N.IndexSet)

(******************************************************************************)
(****************************** Type Refinements ******************************)
(******************************************************************************)

type refVar = string

module type CTYPE_REFINEMENT = sig
  type t
  val is_subref    : t -> t -> bool
  val of_const     : C.constant -> t
  val top          : t
  val d_refinement : unit -> t -> P.doc
end

module IndexRefinement = struct
  type t = Index.t

  let top          = Index.top
  let is_subref    = Index.is_subindex
  let d_refinement = Index.d_index
  
  let of_const = function
    | C.CInt64 (v, ik, _) -> Index.of_int (Int64.to_int v)
    | C.CChr c            -> Index.IInt (Char.code c)
    | C.CReal (_, fk, _)  -> Index.top
    | C.CStr _            -> Index.IInt 0
    | c                   -> halt <| E.bug "Unimplemented ctype_of_const: %a@!@!" C.d_const c
end

module VarRefinement = struct
  module IR = IndexRefinement
  type t = refVar * IR.t
  let du                = "None"
  let top               = (du, IR.top)
  let is_subref x y     = IR.is_subref (snd x) (snd y)
  let of_const x        = (du, IR.of_const x)
  let d_refinement () x = P.dprintf "(%s, %a)" (fst x) IR.d_refinement (snd x)
end


(******************************************************************************)
(************************* Binders ********************************************)
(******************************************************************************)

type binder  = N of Ast.Symbol.t
             | S of string 
             | I of Index.t 
             | Nil

let d_binder () = function 
  | N n -> P.text (Ast.Symbol.to_string n)
  | S s -> P.text s
  | I i -> P.dprintf "@@%a" Index.d_index i
  | Nil -> P.nil




(******************************************************************************)
(************************* Refctypes and Friends ******************************)
(******************************************************************************)

let reft_of_top = 
  let so = Ast.Sort.t_obj in
  let vv = Ast.Symbol.value_variable so in
  FC.make_reft vv so []

let d_reft () r =
  CM.doc_of_formatter (FC.print_reft_pred None) r
  (* WORKS: P.dprintf "@[%s@]" (Misc.fsprintf (FC.print_reft_pred None) r) *)

let d_index_reft () (i,r) = 
  P.dprintf "%a , %a" Index.d_index i d_reft r
  (*let di = Index.d_index () i in
  let dc = P.text " , " in
  let dr = d_reft () r in
  P.concat (P.concat di dc) dr
  *)

module Reft = struct
  type t           = Index.t * FC.reft
  let d_refinement = d_index_reft
  let is_subref    = fun ir1 ir2 -> assert false
  let of_const     = fun c -> assert false
  let top          = Index.top, reft_of_top 
  let ref_of_any   = Index.ind_of_any, reft_of_top
end

(******************************************************************************)
(***************************** Parameterized Types ****************************)
(******************************************************************************)

type finality =
  | Final
  | Nonfinal

type fieldinfo  = {fname : string option; ftype : Cil.typ option} 
type ldinfo     = {stype : Cil.typ option; any: bool} 

let dummy_fieldinfo  = {fname = None; ftype = None}
let dummy_ldinfo = {stype = None; any = false}
let any_ldinfo   = {stype = None; any = true }
let any_fldinfo  = {fname = None; ftype = None}

type tvar = int
    
let tvar_prefix = "t"
let d_tvar () n = Pretty.dprintf "%s%d" tvar_prefix n
    
let fresh_tvar = Misc.mk_int_factory () |> fst

type 'a prectype =
  | Int  of int * 'a           (* fixed-width integer *)
  | Ref  of Sloc.t * 'a        (* reference *)
  | FRef of ('a precfun) * 'a  (* function reference *)
  | ARef                       (* a dynamic "blackhole" reference *)
  | Any                        (* the variable-width type of a "blackhole" *)
  | TVar of tvar               (* type variables *)


and 'a prefield = {  pftype     : 'a prectype
                   ; pffinal    : finality
                   ; pfloc      : C.location
                   ; pfinfo     : fieldinfo }

and effectptr  = Reft.t prectype

and effectset = effectptr SLM.t

and 'a preldesc = { plfields   : (Index.t * 'a prefield) list
                  ; plinfo     : ldinfo 
                  }

and 'a prestore = 'a preldesc Sloc.SlocMap.t
                * Sv.t list
                * 'a hf_appl list
               
and 'a hf_appl  = string * Sloc.t list * 'a list

and ref_hf_appl = FixConstraint.reft hf_appl
and ind_hf_appl = Index.t hf_appl

and 'a precfun =
    { args        : (string * 'a prectype) list;  (* arguments *)
      ret         : 'a prectype;                  (* return *)
      globlocs    : S.t list;                     (* unquantified locations *)
      quant_svars : Sv.t list;
      quant_tvars : tvar list;
      sto_in      : 'a prestore;                  (* in store *)
      sto_out     : 'a prestore;                  (* out store *)
      effects     : effectset;                    (* heap effects *)
    }

type specType =
  | HasShape
  | IsSubtype
  | HasType

type 'a prespec = ('a precfun * specType) Misc.StringMap.t 
                * ('a prectype * specType) Misc.StringMap.t 
                * 'a prestore
                * specType SLM.t
  
let d_specTypeRel () = function
  | HasShape  -> P.text "::"
  | IsSubtype -> P.text "<:"
  | HasType   -> P.text "|-"

let specTypeMax st1 st2 = match st1, st2 with
  | HasShape, st | st, HasShape -> st
  | HasType, _   | _, HasType   -> HasType
  | _                           -> IsSubtype

let d_fieldinfo () = function
  | { fname = Some fn; ftype = Some t } -> 
      P.dprintf "/* FIELDINFO %s %a */" fn Cil.d_type t 
  | { ftype = Some t } -> 
      P.dprintf "/* FIELDINFO %a */" Cil.d_type t 
  | _ ->
      P.nil (* RJ: screws up the autospec printer. P.dprintf "/* FIELDINFO ??? */" *)

let d_ldinfo () = function
  | { stype = Some t } -> 
      P.dprintf "/* %a */" Cil.d_type t
  | _ -> 
      P.nil
	
let rec d_prectype d_refinement () = function
      | Int (n, r)  -> P.dprintf "int(%d, %a)" n d_refinement r
      | Ref (s, r)  -> P.dprintf "ref(%a, %a)" S.d_sloc s d_refinement r
      | FRef (f, r) -> P.dprintf "fref(<TBD>, %a)" d_refinement r
      | ARef        -> P.dprintf "aref(%a)" S.d_sloc Sloc.sloc_of_any
      | Any         -> P.dprintf "any"
      | TVar t      -> P.dprintf "%s%d" tvar_prefix t

let d_lst fn () lst = Pretty.seq ~sep:(Pretty.text " ") ~doit:fn ~elements:lst
let d_varstore = d_lst (fun h -> Sv.d_svar () h)
let d_tvars    = d_lst (fun t -> Pretty.dprintf "%s%d" tvar_prefix t)

let d_refctype = d_prectype Reft.d_refinement

let d_effectinfo = d_refctype
  
let d_storelike d_binding =
  SLMPrinter.docMap ~sep:(P.dprintf ";@!") (fun l d -> P.dprintf "%a |-> %a" S.d_sloc l d_binding d)

let prectype_subs subs = function
  | Ref  (s, i) -> Ref (S.Subst.apply subs s, i)
  | pct         -> pct

let fieldinfo_of_cilfield prefix v = 
  { fname = Some (prefix ^ v.Cil.fname)
  ; ftype = Some v.Cil.ftype 
  }

let rec unfold_compinfo prefix ci = 
  let _  = asserti ci.Cil.cstruct "TBD: unfold_compinfo: unions" in
  ci.Cil.cfields |> Misc.flap (unfold_fieldinfo prefix)

and unfold_fieldinfo prefix fi = 
  match Cil.unrollType fi.Cil.ftype with
  | Cil.TComp (ci, _) -> 
     unfold_compinfo (prefix ^  fi.Cil.fname ^ ".") ci
  | _ -> [fieldinfo_of_cilfield prefix fi]

let unfold_compinfo pfx ci = 
  unfold_compinfo pfx ci 
  >> wwhen mydebug (E.log "unfold_compinfo: pfx = <%s> result = %a\n"  pfx (CM.d_many_braces false d_fieldinfo))

let unfold_ciltyp = function 
  | Cil.TComp (ci,_) ->
      unfold_compinfo "" ci
      |> Misc.index_from 0 
      |> Misc.IntMap.of_list 
      |> (fun im _ i -> Misc.IntMap.find i im)
  | Cil.TArray (t',_,_) ->
      (fun _ i -> { fname = None ; ftype = Some t'})
  | t -> 
      (fun _ i -> { fname = None; ftype = Some t}) 

let hf_appl_restrict f hfs =
  List.filter (function (_, l :: _, _) -> f l) hfs

let hf_appl_binds l = function
  | (_, k :: _, _) -> Sloc.eq k l 
  | _ -> false

let hf_appls_bind l =
  List.exists (hf_appl_binds l)

let hf_appl_binding_of l hfs =
  try Some (List.find (hf_appl_binds l) hfs)
  with Not_found -> None

let hf_appl_arg_of l =
  List.partition (fun (_, ls, _) -> List.mem l ls)

let hf_appl_sub sub (hf, ls, rs) =
  hf, List.map (Sloc.Subst.apply sub) ls, rs

let hf_appls_sub sub hfs =
  List.map (hf_appl_sub sub) hfs 

let hf_appl_fakemap f (hf, ls, rs) =
  let rs = List.map (fun r -> f (Int (0, r))) rs  
        |> List.map (function Int (_, r) -> r) in
  (hf, ls, rs)

let hf_appls_fakemap f =
  List.map (hf_appl_fakemap f)

let hf_appl_bloc = function
  (_, l :: _, _) -> l

let hf_appls_domain hfs =
  hfs |>: hf_appl_bloc

module EffectSet = struct
  type t = effectset

  let empty = SLM.empty

  let apply f effs =
    SLM.map f effs

  let maplisti f effs =
    effs |> SLM.to_list |>: Misc.uncurry f

  let subs sub effs =
    effs 
 |> SLM.to_list 
 |> List.map (S.Subst.apply sub <**> prectype_subs sub)
 |> SLM.of_list
    (* apply (prectype_subs sub) effs *)

  let find effs l =
    SLM.find l effs

  let mem effs l =
    SLM.mem l effs

  let add effs l eff =
    SLM.add l eff effs

  let domain effs = SLM.domain effs

  let d_effect () eptr =
    d_refctype () eptr

  let d_effectset () effs =
    P.dprintf "{@[%a@]}" (d_storelike d_effect) effs
end
      
module TVarElt = struct
  type t = tvar
  let refresh _ = fresh_tvar ()
  let d_t () t = Pretty.dprintf "%s%d" tvar_prefix t
end
      
module TVarSubst = Substitution.Make(TVarElt)
module TS = TVarSubst
  
module type INST_ELT = sig
  type t
  val d_t : unit -> t prectype -> Pretty.doc
end
  
module type INST_TYPE = sig
  type e
  type t = (tvar * e prectype) list
  val empty     : t
  val mem       : tvar -> t -> bool
  val lookup    : tvar -> t -> e prectype
  val apply     : t -> e prectype -> e prectype
  val map_binds : (e prectype -> e prectype) -> t -> t
  val extend    : tvar -> e prectype -> t -> t
  val d_inst    : unit -> t -> Pretty.doc
end
  
module VarInst (T : INST_ELT) : INST_TYPE with type e = T.t = struct
  type e = T.t
  type t = (tvar * e prectype) list
      
  let d_inst () lst =
    P.dprintf "[@[%a@]]" (P.d_list ", " (fun () (f,t) ->
      P.dprintf "%s%d -> %a" tvar_prefix f T.d_t t)) lst
      
  let empty  = []
  let mem    = List.mem_assoc
  let lookup = List.assoc
    
  let apply inst = function
    | TVar t when mem t inst -> lookup t inst
    | ct -> ct
      
  let map_binds f = List.map (Misc.app_snd f)
    (* note here that extending applies the instantiation *)
  let extend t ct sub =
    (t, ct) :: List.remove_assoc t (map_binds (apply [(t,ct)]) sub)
end
  
module StoreSubst = struct
    open SVM
    type elt = SS.t * SVS.t
	  
    type t = elt SVM.t
	
    let (empty:t) = empty

    let lookup v sub =
      try find v sub with Not_found -> (SS.empty, SVS.empty)
	
    let extend_sset v ss sub =
      let (slocs, svars) = lookup v sub in
      add v (SS.union slocs ss, svars) sub
	
    let extend_vset v vs sub =
      let (slocs, svars) = lookup v sub in
      add v (slocs, SVS.union svars vs) sub
	
    let extend v sub sub' =
      let (slocs, svars) = lookup v sub in
      let (slocs', svars') = lookup v sub' in
      add v (SS.union slocs slocs', SVS.union svars svars') sub'
	
    let mem = mem
	
    let subs sub t =
      let sub_ss sub ss = ss |> SS.elements |> List.map (S.Subst.apply sub)
        |> List.fold_left (Misc.flip SS.add) SS.empty
      in
      SVM.map (fun (ss,vs) -> (sub_ss sub ss, vs)) t
	
    let to_list = to_list

    module SMP = P.MakeMapPrinter(SVM)
      
    let d_value () (ss,vs) =
      P.dprintf "(%a, %a)" S.d_slocset ss Sv.d_svarset vs
	
    let d_subst () svm =
      SMP.d_map "; " Sv.d_svar d_value () svm
end
  
module StS = StoreSubst

module type CTYPE_DEFS = sig
  module R : CTYPE_REFINEMENT
  type refinement = R.t
      
  module TVarInst : INST_TYPE with type e = refinement
    
  type ctype = refinement prectype
  type field = refinement prefield
  type ldesc = refinement preldesc
  type store = refinement prestore
  type cfun  = refinement precfun
  type spec  = refinement prespec
  type tvinst = TVarInst.t
end

module MakeTypes (R : CTYPE_REFINEMENT): CTYPE_DEFS with module R = R = struct
  module R = R

  type refinement = R.t
    
  module TVarElt : INST_ELT with type t = refinement = struct
    type t = refinement
    let d_t = d_prectype R.d_refinement
  end
      
  module TVarInst : INST_TYPE with type e = refinement = VarInst(TVarElt)

  type ctype = refinement prectype
  type field = refinement prefield
  type ldesc = refinement preldesc
  type store = refinement prestore
  type cfun  = refinement precfun
  type spec  = refinement prespec
  type tvinst = TVarInst.t
end

module IndexTypes  = MakeTypes (IndexRefinement)
module RefVarTypes = MakeTypes (VarRefinement)
module ReftTypes   = MakeTypes (Reft)

module SIGS (T : CTYPE_DEFS) = struct
  module type CTYPE = sig
    type t = T.ctype
        
    exception NoLUB of t * t

    val refinement  : t -> T.refinement
    val set_refinement : t -> T.refinement -> t
    val map         : ('a -> 'b) -> 'a prectype -> 'b prectype
    val map_func    : ('a -> 'b) -> 'a precfun -> 'b precfun
    val d_ctype     : unit -> t -> P.doc
    val of_const    : Cil.constant -> t
    val is_subctype : t -> t -> bool
    val width       : t -> int
    val sloc        : t -> Sloc.t option
    val subs        : Sloc.Subst.t -> t -> t
    val subs_store_var : StoreSubst.t -> Sloc.Subst.t -> T.store -> t -> t
    val subs_tvar   : TVarSubst.t -> t -> t
    val inst_tvar   : TVarSubst.t -> T.tvinst -> t -> t
    val eq          : t -> t -> bool
    val collide     : Index.t -> t -> Index.t -> t -> bool
    val is_void     : t -> bool
    val same_shape   : t -> t -> bool
  end

  module type FIELD = sig
    type t = T.field

    val get_finality  : t -> finality
    val set_finality  : t -> finality -> t
    val get_fieldinfo : t -> fieldinfo
    val set_fieldinfo : t -> fieldinfo -> t
    val is_final      : t -> bool
    val type_of       : t -> T.ctype
    val sloc_of       : t -> Sloc.t option
    val create        : finality -> fieldinfo -> T.ctype -> t
    val subs          : Sloc.Subst.t -> t -> t
    val map_type      : ('a prectype -> 'b prectype) -> 'a prefield -> 'b prefield
    val map2_type     : ('a prectype -> 'a prectype -> 'b prectype) ->
                         'a prefield -> 'a prefield -> 'b prefield
    val d_field       : unit -> t -> P.doc
  end

  module type LDESC = sig
    type t = T.ldesc

    exception TypeDoesntFit of Index.t * T.ctype * t

    val empty         : t
    val any           : t
    val eq            : t -> t -> bool
    val is_empty      : t -> bool
    val is_any        : t -> bool
    val is_read_only  : t -> bool
    val add           : Index.t -> T.field -> t -> t
    val create        : ldinfo -> (Index.t * T.field) list -> t
    val remove        : Index.t -> t -> t
    val mem           : Index.t -> t -> bool
    val referenced_slocs : t -> Sloc.t list
    val find          : Index.t -> t -> (Index.t * T.field) list
    val foldn         : (int -> 'a -> Index.t -> T.field -> 'a) -> 'a -> t -> 'a
    val fold          : ('a -> Index.t -> T.field -> 'a) -> 'a -> t -> 'a
    val subs          : Sloc.Subst.t -> t -> t
    val map           : ('a prefield -> 'b prefield) -> 'a preldesc -> 'b preldesc
    val map2          : ('a prefield -> 'a prefield -> 'b prefield) ->
                         'a preldesc -> 'a preldesc -> 'b preldesc
    val mapn          : (int -> Index.t -> 'a prefield -> 'b prefield) -> 'a preldesc -> 'b preldesc
    val iter          : (Index.t -> T.field -> unit) -> t -> unit
    val indices       : t -> Index.t list
    val bindings      : t -> (Index.t * T.field) list

    val set_ldinfo    : t -> ldinfo -> t
    val get_ldinfo    : t -> ldinfo
    val set_stype     : t -> Cil.typ option -> t
    val d_ldesc       : unit -> t -> P.doc
    val decorate      : Sloc.t -> Cil.typ -> t -> t
    
    val d_vbind       : unit -> (binder * (T.ctype * Cil.typ)) -> P.doc
    val d_sloc_ldesc  : unit -> (Sloc.t * t) -> P.doc
  end

  module type STORE = sig
    type t = T.store

    val empty        : t
    val bindings     : 'a prestore -> (Sloc.t * 'a preldesc) list
    val abstract     : t -> t
    val join_effects :
      t ->
      effectset ->
      (Sloc.t * (T.ldesc * effectptr)) list
    val domain       : t -> Sloc.t list
    val mem          : t -> Sloc.t -> bool
    val sto_of_hfs   : T.R.t hf_appl list -> t
    val closed       : t -> t -> bool
    val reachable    : t -> Sloc.t -> Sloc.t list
    val restrict     : t -> Sloc.t list -> t
    val map          : ('a prectype -> 'b prectype) -> 'a prestore -> 'b prestore
    val map_variances : ('a prectype -> 'b prectype) ->
                        ('a prectype -> 'b prectype) ->
                        'a prestore ->
                        'b prestore
    val map_ldesc    : (Sloc.t -> 'a preldesc -> 'a preldesc) -> 'a prestore -> 'a prestore
    val partition    : (Sloc.t -> bool) -> t -> t * t
    val remove       : t -> Sloc.t -> t
    val sh_upd       : t -> t -> t
    val upd          : t -> t -> t
  (** [upd st1 st2] returns the store obtained by adding the locations from st2 to st1,
      overwriting the common locations of st1 and st2 with the blocks appearing in st2 *)
    val subs         : Sloc.Subst.t -> t -> t
    val subs_store_var : StoreSubst.t -> Sloc.Subst.t -> t -> t -> t
    val subs_tvar    : TVarSubst.t -> t -> t
    val inst_tvar    : TVarSubst.t -> (tvar * T.refinement prectype) list -> t -> t
    val ctype_closed : T.ctype -> t -> bool
    val indices      : t -> Index.t list
    val abstract_empty_slocs : t -> t
    val add_var      : t -> Sv.t -> t
    val add_app      : t -> T.refinement hf_appl -> t
    val rem_app      : t -> string -> S.t -> t
    val vars         : t -> Sv.t list
    val filter_vars  : (Sv.t -> bool) -> t -> t
    val concrete_part : t -> t
    val hfuns        : t -> T.refinement hf_appl list
    val cnc          : t -> T.ldesc SLM.t
        
    val d_store_addrs: unit -> t -> P.doc
    val d_store      : unit -> t -> P.doc
    val d_store_hfs  : unit -> t -> P.doc

    val add           : t -> Sloc.t -> T.ldesc -> t
      (* val bindings      : 'a prestore -> (Sloc.t * 'a preldesc) list *)
      (* val domain        : t -> Sloc.t list *)
      (* val mem           : t -> Sloc.t -> bool *)
    val ensure_sloc   : t -> Sloc.t -> t
    val ensure_var    : Sv.t -> t -> t
    val ensure_skeleton: t -> t -> t
    val find          : t -> Sloc.t -> T.ldesc
    val find_or_empty : t -> Sloc.t -> T.ldesc
      (* val map           : (T.ctype -> T.ctype) -> t -> t *)
    val fold_fields   : ('a -> Sloc.t -> Index.t -> T.field -> 'a) -> 'a -> t -> 'a
    val fold_locs     : (Sloc.t -> T.ldesc -> 'a -> 'a) -> 'a -> t -> 'a

    module Unify: sig
      exception UnifyFailure of Sloc.Subst.t * t

      val unify_ctype_locs : t -> Sloc.Subst.t -> T.TVarInst.t -> T.ctype -> T.ctype -> t * Sloc.Subst.t * T.TVarInst.t
      val unify_overlap    : t -> Sloc.Subst.t -> Sloc.t -> Index.t -> t * Sloc.Subst.t
      val add_field        : t -> Sloc.Subst.t -> Sloc.t -> Index.t -> T.field -> t * Sloc.Subst.t
    end
  end

  module type CFUN = sig
    type t = T.cfun
        
    (* module TVarSubst : Substitution.S with type e = tvar *)
        
    val sto_in          : t -> T.R.t prestore
    val sto_out         : t -> T.R.t prestore
    val d_cfun          : unit -> t -> P.doc
    val map             : ('a prectype -> 'b prectype) -> 'a precfun -> 'b precfun
    val map_variances   : ('a prectype -> 'b prectype) ->
                          ('a prectype -> 'b prectype) ->
                          'a precfun ->
                          'b precfun
    val map_ldesc       : (Sloc.t -> 'a preldesc -> 'a preldesc) -> 'a precfun -> 'a precfun
    val apply_effects   : (effectptr -> effectptr) -> t -> t
    val well_formed     : T.store -> t -> bool
    val normalize_names : t -> t ->
      (T.store -> Sloc.Subst.t -> (string * string) list -> T.ctype -> T.ctype) ->
      (T.store -> Sloc.Subst.t -> (string * string) list -> effectptr -> effectptr) ->
      t * t
    val same_shape      : t -> t -> bool
    (* val quantify_svars  : t -> t *)
    val quantified_locs : t -> Sloc.t list
    val quantified_svars : t -> Sv.t list
    val quantified_tvars : t -> tvar list
    val generalize      : t -> t
    val free_svars      : t -> Sv.t list
    val instantiate     : CilMisc.srcinfo -> t -> T.ctype list -> T.store -> (t * Sloc.Subst.t * TVarSubst.t * (tvar * T.ctype) list * StoreSubst.t)
    val make            : (string * T.ctype) list -> S.t list -> Sv.t list -> tvar list -> T.store -> T.ctype -> T.store -> effectset -> t
    val subs            : t -> Sloc.Subst.t -> t
    val subs_store_var : StoreSubst.t -> Sloc.Subst.t -> T.store -> t -> t
    val subs_tvar   : TVarSubst.t -> t -> t
    val inst_tvar   : TVarSubst.t -> T.tvinst -> t -> t
    val indices         : t -> Index.t list
    val map_stores  : (T.store -> T.store) -> t -> t
  end

  module type SPEC = sig
    type t      = T.spec

    val empty   : t

    val map : ('a prectype -> 'b prectype) -> 'a prespec -> 'b prespec
    val map_stores    : (T.store -> T.store) -> t -> t
    val map_stores_fn : (string option -> T.store -> T.store) -> t -> t
    val add_fun : bool -> string -> T.cfun * specType -> t -> t
    val add_var : bool -> string -> T.ctype * specType -> t -> t
    val add_data_loc : Sloc.t -> T.ldesc * specType -> t -> t
    val find_cf : t -> string -> T.cfun * specType
    (* val add_fun_loc  : Sloc.t -> T.cfun * specType -> t -> t *)
    val store   : t -> T.store
    val funspec : t -> (T.cfun * specType) Misc.StringMap.t
    val varspec : t -> (T.ctype * specType) Misc.StringMap.t
    val locspectypes : t -> specType SLM.t

    val make    : (T.cfun * specType) Misc.StringMap.t ->
                  (T.ctype * specType) Misc.StringMap.t ->
                  T.store ->
                  specType SLM.t ->
                  t
    val add     : t -> t -> t
    val d_spec  : unit -> t -> P.doc
  end
end

module type S = sig
  module T : CTYPE_DEFS
    
  module CType : SIGS (T).CTYPE
  module Field : SIGS (T).FIELD
  module LDesc : SIGS (T).LDESC
  module Store : SIGS (T).STORE
  module CFun  : SIGS (T).CFUN
  module Spec  : SIGS (T).SPEC

  module ExpKey: sig
    type t = Cil.exp
    val compare: t -> t -> int
  end

  module ExpMap: Map.S with type key = ExpKey.t

  module ExpMapPrinter: sig
    val d_map:
      ?dmaplet:(P.doc -> P.doc -> P.doc) ->
      string ->
      (unit -> ExpMap.key -> P.doc) ->
      (unit -> 'a -> P.doc) -> unit -> 'a ExpMap.t -> P.doc
  end

  type ctemap = CType.t ExpMap.t

  val d_ctemap: unit -> ctemap -> P.doc
end

module Make (T: CTYPE_DEFS): S with module T = T = struct
  module T   = T
  module SIG = SIGS (T)

  (***********************************************************************)
  (***************************** Types ***********************************)
  (***********************************************************************)

  module rec CType: SIG.CTYPE = struct
    type t = T.ctype
        
    let refinement = function
      | Int (_, r) | Ref (_, r) | FRef (_, r) -> r
      | ARef | Any | TVar _ -> T.R.top

    let set_refinement ct r = match ct with
      | Int (w, _)  -> Int (w, r)
      | Ref (s, _)  -> Ref (s, r)
      | FRef (f, _) -> FRef (f, r)
      | ARef        -> ARef
      | Any         -> Any

    let rec map f = function
      | Int (i, x) -> Int (i, f x)
      | Ref (l, x) -> Ref (l, f x)
      | FRef(g, x) -> FRef (map_func f g, f x)
      | ARef      -> ARef
      | Any       -> Any    
      | TVar t    -> TVar t
    and map_field f ({pftype = typ} as pfld) =
      {pfld with pftype = map f typ}
    and map_desc f {plfields = flds; plinfo = info} =
      {plfields = List.map (id <**> map_field f) flds; plinfo = info}
    and map_sto f (desc, v, _) = (Sloc.SlocMap.map (map_desc f) desc, v, [])
    and map_func f ({args=args; ret=ret; sto_in=stin; sto_out=stout} as g) =
	{g with args    = List.map (id <**> (map f)) args;
	        ret     = map f ret;
	        sto_in  = map_sto f stin;
	        sto_out = map_sto f stout}

    let d_ctype () = function
      | Int (n, i)  -> P.dprintf "int(%d, %a)" n T.R.d_refinement i
      | Ref (s, i)  -> P.dprintf "ref(%a, %a)" S.d_sloc s T.R.d_refinement i
      | FRef (g, i) -> P.dprintf "fref(@[%a,@!%a@])" CFun.d_cfun g T.R.d_refinement i
      | ARef        -> P.dprintf "aref(%a)" S.d_sloc S.sloc_of_any
      | Any         -> P.dprintf "any"  
      | TVar t      -> P.dprintf "%s%d" tvar_prefix t

    let width = function
      | Int (n, _) -> n
      | Any        -> 0
      | TVar t     -> 0
      | _          -> CM.int_width
    
    let sloc = function
      | Ref (s, _) -> Some s
      | ARef       -> Some S.sloc_of_any
      | TVar t     -> Some S.none
      | _          -> None

    let subs subs = function
      | Ref (s, i) -> Ref (S.Subst.apply subs s, i)
      | pct        -> pct
        
    let subs_store_var subs lsubs sto = function
      | FRef (f, i) -> FRef (CFun.subs_store_var subs lsubs sto f, i)
      | pct -> pct
        
    let subs_tvar rename = function
      | TVar t -> TVar (TVarSubst.apply rename t)
      | FRef (f, r) -> FRef (CFun.subs_tvar rename f, r)
      | ct -> ct
        
    let inst_tvar rename inst = function 
      | TVar t -> TVar (TVarSubst.apply rename t)
                  |> T.TVarInst.apply inst 
      | FRef (f, r) -> FRef (CFun.inst_tvar rename inst f, r)
      | ct -> ct

    exception NoLUB of t * t

    let is_subctype pct1 pct2 =                        
      match pct1, pct2 with
        | Int (n1, r1), Int (n2, r2) when n1 = n2    -> T.R.is_subref r1 r2
        | Ref (s1, r1), Ref (s2, r2) when S.eq s1 s2 -> T.R.is_subref r1 r2
	  (* not sure what the semantics are here
	     should is_subctype be called on the arguments of f1/f2 etc? *)
	      | FRef (f1, r1), FRef (f2, r2) when f1 = f2  -> T.R.is_subref r1 r2
        | ARef, ARef                                 -> true
        | Any, Any                                   -> true
        | Int _, Any                                 -> true
        | Any, Int _                                 -> true
	| TVar t1, TVar t2                           -> t1 = t2
        | _                                          -> false

    let of_const c =
      let r = T.R.of_const c in
        match c with
          | C.CInt64 (v, ik, _) -> Int (C.bytesSizeOfInt ik, r)
          | C.CChr c            -> Int (CM.int_width, r)
          | C.CReal (_, fk, _)  -> Int (CM.bytesSizeOfFloat fk, r)
          | C.CStr s            -> Ref (S.fresh_abstract (CM.srcinfo_of_constant c None) , r)
          | _                   -> halt <| E.bug "Unimplemented ctype_of_const: %a@!@!" C.d_const c

    let eq pct1 pct2 =
      match (pct1, pct2) with
        | Ref (l1, i1), Ref (l2, i2) -> S.eq l1 l2 && i1 = i2
        | _                          -> pct1 = pct2

    let index_overlaps_type i i2 pct =
      Misc.foldn (fun b n -> b || N.overlaps i (N.offset n i2)) (width pct) false

    let extrema_in i1 pct1 i2 pct2 =
      index_overlaps_type i1 i2 pct2 || index_overlaps_type (N.offset (width pct1 - 1) i1) i2 pct2

    let collide i1 pct1 i2 pct2 =
      extrema_in i1 pct1 i2 pct2 || extrema_in i2 pct2 i1 pct1

    let is_void = function
      | Int (0, _) | Any     -> true
      | _                    -> false

    let same_shape = begin function
      | Int _, Int _ | Ref _, Ref _ | FRef _, FRef _
      | ARef _, ARef _ | Any _, Any _ | TVar _, TVar _ -> true
      | _ -> false end
        |> Misc.curry
  end

  (******************************************************************************)
  (*********************************** Stores ***********************************)
  (******************************************************************************)

  and Field: SIG.FIELD = struct
    type t = T.field

    let get_finality {pffinal = fnl} =
      fnl

    let set_finality fld fnl =
      {fld with pffinal = fnl}

    let get_fieldinfo {pfinfo = fi} =
      fi

    let set_fieldinfo fld fi =
      {fld with pfinfo = fi}

    let type_of {pftype = ty} =
      ty

    let create fnl fi t =
      (* pmr: Change location from locUnknown *)
      {pftype = t; pffinal = fnl; pfloc = C.locUnknown; pfinfo = fi}

    let is_final fld =
      get_finality fld = Final

    let sloc_of fld =
      fld |> type_of |> CType.sloc

    let map_type f fld =
      {fld with pftype = fld |> type_of |> f}

    let map2_type f fld1 fld2 =
      {fld1 with pftype = f <| (type_of <| fld1)
                            <| (type_of <| fld2)}

    let subs sub =
      map_type (CType.subs sub)

    let d_finality () = function
      | Final -> P.text "final "
      | _     -> P.nil

    (* ORIG *)
    let d_field () fld =
      P.dprintf "%a%a%a" 
        d_finality (get_finality fld) 
        CType.d_ctype (type_of fld)
        d_fieldinfo (get_fieldinfo fld)

  end

  and LDesc: SIG.LDESC = struct
    type t = T.ldesc

    exception TypeDoesntFit of Index.t * CType.t * t

    let empty =
      { plfields = [] ; plinfo = dummy_ldinfo }

    let any =
      { plfields = [] ; plinfo = any_ldinfo   }

    let eq {plfields = cs1} {plfields = cs2} =
      Misc.same_length cs1 cs2 &&
        List.for_all2
          (fun (i1, f1) (i2, f2) -> i1 = i2 && Field.type_of f1 = Field.type_of f2)
          cs1 cs2

    let is_any {plinfo = inf} =
      inf.any

    let is_empty ld =
      ld.plfields = [] && not(is_any ld)

    let is_read_only {plfields = flds} =
      List.for_all (fun (_, {pffinal = fnl}) -> fnl = Final) flds

    let fits i fld {plfields = cs} =
      let t = Field.type_of fld in
      let w = CType.width t in
        Misc.get_option w (N.period i) >= w &&
          not (List.exists (fun (i2, fld2) -> CType.collide i t i2 (Field.type_of fld2)) cs)

    let rec insert_field ((i, _) as fld) = function
      | []                      -> [fld]
      | (i2, _) as fld2 :: flds -> if i < i2 then fld :: fld2 :: flds else fld2 :: insert_field fld flds

    let add i fld ld =
      if fits i fld ld then
        {ld with plfields = insert_field (i, fld) ld.plfields}
      else raise (TypeDoesntFit (i, Field.type_of fld, ld))

    let remove i ld =
      {ld with plfields = List.filter (fun (i2, _) -> not (i = i2)) ld.plfields}

    let create si flds =
      List.fold_right (Misc.uncurry add) flds {empty with plinfo = si}

    let mem i {plfields = flds} =
      List.exists (fun (i2, _) -> N.is_subindex i i2) flds

    let find i ld =
      if is_any ld then
        [i, {pftype = Any;          pffinal = Nonfinal;
             pfloc  = C.locUnknown; pfinfo  = any_fldinfo}]
      else
        List.filter (fun (i2, _) -> N.overlaps i i2) ld.plfields

    let rec foldn_aux f n b = function
      | []               -> b
      | (i, fld) :: flds -> foldn_aux f (n + 1) (f n b i fld) flds

    let foldn f b ld =
      foldn_aux f 0 b ld.plfields

    let fold f b flds =
      foldn (fun _ b i fld -> f b i fld) b flds

    let mapn f ld =
      {ld with plfields = Misc.mapi (fun n (i, fld) -> (i, f n i fld)) ld.plfields}

    let map2n f ld1 ld2 =
      let ld1p, ld2p = ld1.plfields, ld2.plfields in
      {ld1 with plfields =
        Misc.map2i (fun n (i, fld1) (_, fld2) -> (i, f n i fld1 fld2)) ld1p ld2p}

    let map f flds =
      mapn (fun _ _ fld -> f fld) flds

    let map2 f flds1 flds2 =
      map2n (fun _ _ fld1 fld2 -> f fld1 fld2) flds1 flds2

    let subs sub ld =
      map (Field.subs sub) ld

    let iter f ld =
      fold (fun _ i fld -> f i fld) () ld

    let referenced_slocs ld =
      fold begin fun rls _ fld -> match Field.sloc_of fld with 
        | None   -> rls
        | Some l -> l :: rls
      end [] ld

    let bindings {plfields = flds} =
      flds

    let indices ld =
      ld |> bindings |>: fst

    let get_ldinfo {plinfo = si} =
      si

    let set_ldinfo ld si =
      {ld with plinfo = si}

    let set_stype ld st =
      {ld with plinfo = {ld.plinfo with stype = st}}

    let d_ldesc () {plfields = flds} =
      P.dprintf "@[%t@]"
        begin fun () ->
          P.seq
            (P.dprintf ",@!")
            (fun (i, fld) -> P.dprintf "%a: %a" Index.d_index i Field.d_field fld)
            flds
        end

    let decorate l ty ld =
      let fldm = unfold_ciltyp ty in
      ld |> Misc.flip set_stype (Some ty)
         |> mapn begin fun i _ pf -> 
              try Field.set_fieldinfo pf (fldm ld i) with Not_found -> 
                let _ = E.warn "WARNING: decorate : %a bad idx %d, ld=%a, t=%a \n" 
                        Sloc.d_sloc l i d_ldesc ld Cil.d_type ty
                in pf
            end

    let d_ref () = function
      | Ref (l,_) -> P.dprintf "REF(%a)" Sloc.d_sloc l
      | _         -> P.nil

    let d_vbind () (b, (ct, t)) =
      P.dprintf "%a %a %a %a" 
        Cil.d_type (CM.typStripAttrs t)
        d_ref ct
        d_binder b 
        T.R.d_refinement (CType.refinement ct) 

    let d_ann_field () (i, fld) = 
      match Field.get_fieldinfo fld with
      | { fname = Some fldname; ftype = Some t } ->
          d_vbind () (S fldname, (Field.type_of fld, t))
      | { ftype = Some t } ->
          d_vbind () (I i, (Field.type_of fld, t))
      | _ -> P.dprintf "%a ??? %a" d_binder (I i) d_ref (Field.type_of fld)

    let d_ldinfo () = function
      | {stype = Some t} -> Cil.d_type () (CM.typStripAttrs t)
      | _                -> P.nil

    let d_sloc_ldesc () (l, ld) =
      P.dprintf "%a %a |-> %a" 
        d_ldinfo ld.plinfo
        Sloc.d_sloc l 
        (CM.d_many_braces true d_ann_field) (bindings ld)
    
    (* API *)
    let d_sloc_ldesc () ((l : Sloc.t), (ld: t)) =
      let ld = match ld.plinfo.stype, Sloc.to_ciltyp l with 
               | None, Some ty -> decorate l ty ld 
               | _             -> ld 
      in d_sloc_ldesc () (l, ld)
        
  end

  and Store: SIG.STORE = struct
    type t = T.store

    let empty = (SLM.empty, [], [])

    (* let map_data f = *)
    (*   f |> Field.map_type |> LDesc.map |> SLM.map *)

    let sto_of_hfs hfs = (SLM.empty, [], hfs)

    let map_ldesc f (ds, vs, hf) = SLM.mapi f ds, vs, hf

    let map2_data f =
      f |> Field.map2_type
        |> LDesc.map2
        |> begin fun f -> function Some a, Some b -> f a b
            | _ -> assertf "map2_data: shapes differ" end
        |> Misc.curry
        |> SLM.map2

    let map_data f =
      f |> Field.map_type |> LDesc.map |> SLM.map

    let map_function f =
      SLM.map (CFun.map f)

    let restrict_slm_abstract m =
      SLM.filter (fun l -> const <| S.is_abstract l) m

    let hfuns (_, _, hfs) = hfs

    let cnc (ds, _, _) = ds

    let vars (_, vs, _) = vs

    let concrete_part (sto, _, _) = (sto, [], [])

    let add (ds, vs, hf) l ld =
      (* let _ = assert ((l = Sloc.sloc_of_any) || SLM.mem l ds) in *)
      if not (l = Sloc.sloc_of_any) then
        (SLM.add l ld ds, vs, hf)
      else
        (ds, vs, hf)

    let bindings (ds, _, _) =
      SLM.to_list ds

    let domain (ds, _, hfs) =
         SLM.domain ds ++ hf_appls_domain hfs
      |> Misc.sort_and_compact

    let mem (ds, _, hfs) l =
      Sloc.eq l Sloc.sloc_of_any
      or SLM.mem l ds
      or hf_appls_bind l hfs

    let find (ds, _, _) l =
      if (Sloc.eq l Sloc.sloc_of_any) then
        LDesc.any 
      else
        SLM.find l ds

    let find_or_empty sto l =
      try find sto l with Not_found -> LDesc.empty

    let add_app (s, vs, hfs) hf = s, vs,
      if not (List.mem hf hfs) then hf :: hfs else hfs

    let rem_app (s, vs, hfs) hf l = s, vs,
      List.filter begin function | (hfk, k :: _, _) ->
                                    hfk != hf || not (Sloc.eq l k)
                                 | _ -> true end hfs
 
    let ensure_sloc sto l =
      if hfuns sto |> hf_appls_bind l then
        sto
      else 
        find_or_empty sto l |> add sto l

    let ensure_slocs sto1 sto2 =
         concrete_part sto2
      |> domain
      |> List.fold_left ensure_sloc sto1

    let ensure_fun sto1 hf =
      add_app sto1 hf

    let ensure_funs sto1 sto2 =
         hfuns sto2
      |> List.fold_left ensure_fun sto1

    let ensure_skeleton sto1 sto2 =
         ensure_slocs sto1 sto2
      |> Misc.flip ensure_funs sto2

    let fold_fields f b (ds, _, _) =
      SLM.fold (fun l ld b -> LDesc.fold (fun b i pct -> f b l i pct) b ld) ds b

    let fold_locs f b (ds, _, _) =
      SLM.fold f ds b

    let ensure_var v ((ds, vs, hf) as sto) = 
      if List.mem v vs then sto else 
        (ds, Misc.sort_and_compact (v::vs), hf)

    let map f (ds, vs, hfs) = (f 
                         |> Field.map_type 
                         |> LDesc.map 
                         |> Misc.flip SLM.map ds,
                         vs, hf_appls_fakemap f hfs)
      
    let map_variances f_co f_contra sto = map f_co sto

    let bindings (ds, _, _) = SLM.to_list ds

    let abstract (ds, vs, hfs) =
      (restrict_slm_abstract ds, vs, hf_appl_restrict Sloc.is_abstract hfs)

    let join_effects sto effs =
      sto 
      |> bindings 
      |>: fun (l, ld) -> (l, (ld, EffectSet.find effs (S.canonical l)))

    let subs_addrs_ds subs m =
      SLM.fold (fun l d m -> SLM.add (S.Subst.apply subs l) d m) m SLM.empty

    let subs subs (ds, vs, hfs) =
      (SLM.map (LDesc.subs subs) ds |> subs_addrs_ds subs, vs, hf_appls_sub subs hfs)
        
    let d_store_addrs () st =
      P.seq (P.text ",") (Sloc.d_sloc ()) (domain st)

    let d_hf_ls () ls =
      P.seq (P.text ",") (Sloc.d_sloc ()) ls

    let d_hf_rs () rs =
      P.seq (P.text ",") (T.R.d_refinement ()) rs

    let d_hf_appl () (hf, ls, rs) =
      P.dprintf "@[%s(%a;%a)@]" hf d_hf_ls ls d_hf_rs rs

    let d_hfs () hfs =
      P.seq (P.text ",") (d_hf_appl ()) hfs

    let d_store () (ds, vs, hfs) =
      if vs <> [] && hfs <> [] then
        P.dprintf "[@[%a@]]*@[%a@]*@[%a@]"
        (d_storelike LDesc.d_ldesc) ds d_varstore vs d_hfs hfs
      else if vs = [] then
        P.dprintf "[@[%a@]]*@[%a@]"
        (d_storelike LDesc.d_ldesc) ds d_hfs hfs
      else if hfs = [] then
        P.dprintf "[@[%a@]]*@[%a@]"
        (d_storelike LDesc.d_ldesc) ds d_varstore vs
      else
        P.dprintf "[@[%a@]]" (d_storelike LDesc.d_ldesc) ds

    let d_store_hfs () (_, _, hfs) =
      P.dprintf "@[%a@]" d_hfs hfs

    let remove ((ds, vs, hfs) as sto) l =
      if (l = Sloc.sloc_of_any) then sto
      else hfs
        |> List.filter (function (_, k :: _, _) -> not (Sloc.eq l k) | _ -> true)
        |> fun x -> SLM.remove l ds, vs, x

    let sh_upd (ds1, vs1, hf1) (ds2, vs2, hf2) =
      (SLM.fold SLM.add ds2 ds1, vs1 ++ vs2, hf1 ++ hf2)
      |> Misc.app_snd3 Misc.sort_and_compact
      |> Misc.app_thd3 Misc.sort_and_compact

    let upd s1 s2 = sh_upd s1 s2
      >> begin fun x -> asserts (domain x |> Misc.is_unique)
           "Store.upd: duplicate heap function binds" end
            
    (* this may very well not be the right thing to do with vs *)
    let partition f (ds, vs, hfs) =
      let (ds', ds'') = 
        SLM.fold begin fun l d (m1, m2) ->
          if f l then (SLM.add l d m1, m2) else (m1, SLM.add l d m2)
        end ds (SLM.empty, SLM.empty) in
      let (hfs', hfs'') =
        List.partition (function (_, l :: _, _) -> f l | _ -> false) hfs in
      ((ds', vs, hfs'), (ds'', vs, hfs''))
      
    let ctype_closed t sto = match t with
      | Ref (l, _) -> mem sto l
      | Int _ | Any | FRef _ | ARef -> true

    (* with the addition of hfuns this is quite conservative now, but
     * good enough as long as l is bound by an hfun *)
    let rec reachable_aux sto visited l =
      if SS.mem l visited then
        visited
      else begin try
           l
        |> find sto
        |> LDesc.referenced_slocs
        |> List.fold_left (reachable_aux sto) (SS.add l visited)
        with Not_found -> SS.add l visited
      end

    let reachable sto l =
      l |> reachable_aux sto SS.empty |> SS.elements

    let restrict sto ls =
         sto
      |> partition (ls |> Misc.flap (reachable sto) |> Misc.sort_and_compact |> Misc.flip List.mem)
      |> fst

    (* MK: suspicious *)
    let rec closed globstore sto = 
      fold_fields
        (fun c _ _ fld -> c && ctype_closed (Field.type_of fld) (upd globstore sto)) true sto
          
    let abstract_empty_slocs sto = 
      let v = Sv.fresh_svar () in
      fold_locs
        (fun s ld sto -> 
          if S.is_abstract s && LDesc.is_empty ld then 
            remove sto s |> ensure_var v
          else
            sto) sto sto
        
    let add_var (s, vs, hf) v =
    (* Should we check here for two variables? OR check later when we
       get two mappings in the heap? Deferred for now... *)
      (s, v::vs, hf)

      
      
    let filter_vars p sto = Misc.app_snd3 (List.filter p) sto
      
    let slm_acc_list f m =
      SLM.fold (fun _ d acc -> f d ++ acc) m []

    let indices (ds, _, _) = slm_acc_list LDesc.indices ds
          
    let subs_store_var subs lsubs fromsto (insto, vs, hf) = 
      List.fold_left begin fun (sto, vs, hf) v -> 
             if StS.mem v subs then
      	       let (slocs, vars) = StS.lookup v subs in
	             let (conc, _, _)  = SS.elements slocs |> restrict fromsto in
                 (upd (sto, vs, hf) (conc, SVS.elements vars, hf))
             else
               (sto, v :: vs, hf) end (insto, [], hf) vs
      |> Misc.app_snd3 Misc.sort_and_compact
          
    let subs_tvar rename sto =
      map (CType.subs_tvar rename) sto
          
    let inst_tvar rename tinst sto =
      map (CType.inst_tvar rename tinst) sto

    
    module Unify = struct
      exception UnifyFailure of S.Subst.t * t

      let fail sub sto _ =
        raise (UnifyFailure (sub, sto))
          
      let lift_third (x,y) z = (x, y, z)
        
      let all_subs sub tsub ct = ct|> T.TVarInst.apply tsub |> CType.subs sub 

      let rec unify_ctype_locs sto sub tsub ct1 ct2 = 
        match all_subs sub tsub ct1, all_subs sub tsub ct2 with
        (* match CType.subs sub ct1, CType.subs sub ct2 with *)
        | Int (n1, _), Int (n2, _) when n1 = n2 -> (sto, sub, tsub)
        | Ref (s1, _), Ref (s2, _)              -> lift_third (unify_locations sto sub s1 s2) tsub
        | FRef (f1,_), FRef(f2,_)  when CFun.same_shape f1 f2 -> (sto, sub, tsub)
        | ARef, ARef                            -> (sto, sub, tsub)
        | Ref (s, _), ARef                      -> lift_third (anyfy_location sto sub s) tsub
        | ARef, Ref (s, _)                      -> lift_third (anyfy_location sto sub s) tsub
        | Any, Any                              -> (sto, sub, tsub)
        | Any, Int _                            -> (sto, sub, tsub)
        | Int _, Any                            -> (sto, sub, tsub)
        | TVar t1, TVar t2 when t1 = t2         -> (sto, sub, tsub)
        (* | TVar _ as tv, ct'                     -> unify_tvars sub sto tsub tv ct' *)
        (* | ct', (TVar _ as tv)                   -> unify_tvars sub sto tsub ct' tv *)
        | ct1, ct2                              -> 
          fail sub sto <| C.error "Cannot unify locations of %a and %a@!" CType.d_ctype ct1 CType.d_ctype ct2
              
      and unify_tvars sub sto tsub t1 t2 = 
        match all_subs sub tsub t1, all_subs sub tsub t2 with
          | (Ref (s1, r1) as t1), (Ref (s2, r2) as t2) (* when r1 = r2 *)-> 
            unify_ctype_locs sto sub tsub t1 t2
          | TVar t1', TVar t2'       -> sto, sub, T.TVarInst.extend t1' (TVar t2') tsub
          | TVar t', ((Ref _) as r) 
          | ((Ref _) as r), TVar t'  -> sto, sub, T.TVarInst.extend t' r tsub
          | ct1, ct2 -> fail sub sto <| C.error "Cannot unify (tvar-inst'd) %a and %a@!" CType.d_ctype ct1 CType.d_ctype ct2
              
      and unify_data_locations sto sub s1 s2 =
        let hf1, hf2 = Misc.map_pair begin fun x ->
                         hfuns sto |> hf_appl_binding_of x end (s1, s2)
                    |> (function (Some hf, None) -> (Some hf, Some hf)
                               | hfs             -> hfs)
                    |> Misc.map_pair (fun x -> x |>> ((hf_appl_sub sub) <+> some)) in
        let ld1, ld2 = Misc.map_pair (find_or_empty sto <+> LDesc.subs sub) (s1, s2) in
        let sto      = remove sto s1 in
        let sto      = ld2 |> add sto s2 |> subs sub in
        let sto      = match hf2 with Some hf -> add_app sto hf | _ -> sto in
        LDesc.fold (fun (sto, sub) i f -> add_field sto sub s2 i f) (sto, sub) ld1
       

      and anyfy_location sto sub s =
        if s = S.sloc_of_any then
          (sto, sub)
        else
          let sub = S.Subst.extend s S.sloc_of_any sub in
          if mem sto s then
            (subs sub (remove sto s), sub)
          else
            (subs sub sto, sub)

      and unify_locations sto sub s1 s2 =
        if not (S.eq s1 s2) then
          let sub  = S.Subst.extend s1 s2 sub in
          let dsto = concrete_part sto in
          if mem dsto s1 || mem dsto s2 then
            unify_data_locations sto sub s1 s2
          else (subs sub sto, sub)
        else (sto, sub)

      and unify_fields sto sub fld1 fld2 = match Misc.map_pair (Field.type_of <+> CType.subs sub) (fld1, fld2) with
        | ct1, ct2                   when ct1 = ct2 -> (sto, sub)
        | Ref (s1, i1), Ref (s2, i2) when i1 = i2   -> unify_locations sto sub s1 s2
        | Ref (s, _), ARef | ARef, Ref(s, _)        -> anyfy_location sto sub s
        | Any , Int _ | Int _, Any                  -> (sto, sub)
        | ct1, ct2                                  ->
          fail sub sto <| C.error "Cannot unify %a and %a@!" CType.d_ctype ct1 CType.d_ctype ct2

      and unify_overlap sto sub s i =
        let s  = S.Subst.apply sub s in
        let ld = find_or_empty sto s in
        match LDesc.find i ld with
          | []                         -> (sto, sub)
          | ((_, fstfld) :: _) as olap ->
            let i = olap |>: fst |> List.fold_left Index.lub i in
            ld
          |> List.fold_right (fst <+> LDesc.remove) olap
          |> LDesc.add i fstfld
          |> add sto s
          |> fun sto ->
            List.fold_left
              (fun (sto, sub) (_, olfld) -> unify_fields sto sub fstfld olfld)
              (sto, sub)
              olap

      and add_field sto sub s i fld =
        try
          begin match i with
            | N.IBot                 -> (sto, sub)
            | N.ICClass _ | N.IInt _ ->
              let sto, sub = unify_overlap sto sub s i in
              let s        = S.Subst.apply sub s in
              let fld      = Field.subs sub fld in
              let ld       = find_or_empty sto s in
              begin match LDesc.find i ld with
                | []          -> (ld |> LDesc.add i fld |> add sto s, sub)
                | [(_, fld2)] -> unify_fields sto sub fld fld2
                | _           -> assert false
              end
          end
        with e ->
          C.error "Can't fit @!%a: %a@!  in location@!%a |-> %a@!"
            Index.d_index i 
            Field.d_field fld 
            S.d_sloc_info s 
            LDesc.d_ldesc (find_or_empty sto s) 
          |> ignore;
          raise e
    end
  end

  (******************************************************************************)
  (******************************* Function Types *******************************)
  (******************************************************************************)
  and CFun: SIG.CFUN = struct
    type t = T.cfun
        
    (* API *)
    let make args globs svars tvars sin reto sout effs =
      { args     = args;
        ret      = reto;
        globlocs = globs;
        quant_svars = List.sort Sv.compare svars; 
	quant_tvars = List.sort compare tvars;
        sto_in   = sin;
        sto_out  = sout;
        effects  = effs;
      }

    let map_variances f_co f_contra ft =
      { args     = List.map (Misc.app_snd f_contra) ft.args;
        ret      = f_co ft.ret;
        globlocs = ft.globlocs;
        quant_svars = ft.quant_svars;
	quant_tvars = ft.quant_tvars;
        sto_in   = Store.map_variances f_contra f_co ft.sto_in;
        sto_out  = Store.map_variances f_co f_contra ft.sto_out;
        effects  = ft.effects;
      }

    let sto_in  t = t.sto_in
    let sto_out t = t.sto_out

    let map f ft =
      map_variances f f ft

    let map_ldesc f ft =
      { ft with 
        sto_in = Store.map_ldesc f ft.sto_in
      ; sto_out = Store.map_ldesc f ft.sto_out }

    let apply_effects f ft =
      {ft with effects = EffectSet.apply f ft.effects}
        
    let quantified_tvars {quant_tvars = tvars} = tvars

    let quantified_locs {globlocs = g; sto_out = sto; args = args; ret = ret} =
      Store.domain sto
     |> Misc.flap (Store.reachable sto)
     |> List.fold_left (Misc.flip SS.add) SS.empty
     (* Grab any locations that perhaps weren't in the store
        Does this make the previous step redundant? *)
     |> begin fun ss -> List.fold_left 
        (fun s ct -> match CType.sloc ct with 
                       | Some l -> SS.add l s  
                       | _ -> s) ss (ret::List.map snd args)
     end
     |> SS.filter (not <.> Misc.flip List.mem g)
     |> SS.elements

    let d_slocs () slocs = P.dprintf "[%t]" (fun _ -> P.seq (P.text ";") (S.d_sloc ()) slocs)
    let d_arg () (x, ct) = P.dprintf "%s : %a" x CType.d_ctype ct
    let d_args () args   = P.seq (P.dprintf ",@!") (d_arg ()) args

    let d_argret () ft =
      P.dprintf "arg       (@[%a@])\nret       %a\n"
        d_args ft.args
        CType.d_ctype ft.ret

    let d_spec_globlocs () ft =
      P.dprintf "global    %a\n" d_slocs ft.globlocs

    let d_globlocs () ft =
      d_spec_globlocs () ft

    let d_stores () ft =
      P.dprintf "store_in  %a\nstore_out %a\n"
        Store.d_store ft.sto_in
        Store.d_store ft.sto_out

    let d_effectset () ft =
      P.dprintf "effects   %a" EffectSet.d_effectset ft.effects
        
    let d_qsvars () ft =
      if ft.quant_svars <> [] then
        (* P.dprintf "forall %a.\n" d_varstore ft.quant_svars *)
        P.dprintf "∀ %a.\n" d_varstore ft.quant_svars
      else
        P.dprintf ""
	  
    let d_qtvars () ft = 
      if ft.quant_tvars <> [] then
	(* P.dprintf "forall %a.\n" d_tvars ft.quant_tvars *)
	P.dprintf "∀ %a.\n" d_tvars ft.quant_tvars
      else
	P.dprintf ""

    let d_cfun () ft  =
      P.dprintf "@[%a%a%a%a%a%a@]" 
	d_qsvars ft d_qtvars ft d_argret ft d_globlocs ft d_stores ft d_effectset ft

    let rec capturing_subs cf sub =
      (* let apply_sub = CType.subs sub in *)
      let apply_sub = subs_frefs sub in
        make (List.map (Misc.app_snd apply_sub) cf.args)
             (List.map (S.Subst.apply sub) cf.globlocs)
             cf.quant_svars
	     cf.quant_tvars
             (Store.subs sub cf.sto_in)
             (apply_sub cf.ret)
             (Store.subs sub cf.sto_out)
             (EffectSet.subs sub cf.effects)
	  
    and subs_frefs sub = function
      | FRef (f, r) -> FRef (capturing_subs f sub, r)
      | x -> CType.subs sub x

    let subs cf sub =
      cf |> quantified_locs |> S.Subst.avoid sub |> capturing_subs cf
          
    let subs_tvar rename cf = 
      let apply_sub = CType.subs_tvar rename in
      make (List.map (Misc.app_snd apply_sub) cf.args)
           cf.globlocs
           cf.quant_svars
          (cf.quant_tvars |>: TVarSubst.apply rename)
          (Store.subs_tvar rename cf.sto_in)
          (apply_sub cf.ret)
          (Store.subs_tvar rename cf.sto_out)
           cf.effects

    let rec order_locs_aux sto ord = function
      | []      -> ord
      | l :: ls ->
          if not (List.mem l ord) then
            let ls = if Store.mem sto l then ls @ (l |> Store.find sto |> LDesc.referenced_slocs) else ls in
              order_locs_aux sto (l :: ord) ls
          else order_locs_aux sto ord ls

    let ordered_locs ({args = args; ret = ret; sto_out = sto} as cf) =
      let ord = (CType.sloc ret :: List.map (snd <+> CType.sloc) args)
             |> Misc.maybe_list
             |> order_locs_aux sto []
             |> Misc.mapi (fun i x -> (x, i)) in
      cf |> quantified_locs |> Misc.fsort (Misc.flip List.assoc ord)

    let replace_arg_names anames cf =
      {cf with args = List.map2 (fun an (_, t) -> (an, t)) anames cf.args}
        
    let rec pad_names = function
      | ([], [])          -> []
      | (l::ls1, [])      -> (l, Sloc.copy_abstract [] l)::pad_names (ls1, [])
      | ([], l::ls2)      -> (Sloc.copy_abstract [] l, l)::pad_names ([], ls2)
      | (l::ls1, l'::ls2) -> (l,l')::pad_names(ls1, ls2)
        
    let normalize_names cf1 cf2 f fe =
      let ls1, ls2    = Misc.map_pair ordered_locs (cf1, cf2) 
                      |> pad_names 
                      |> List.split in
      let fresh_locs   = List.map (Sloc.copy_abstract []) ls1 in
      let lsub1, lsub2 = Misc.map_pair (Misc.flip List.combine fresh_locs) (ls1, ls2) in
      let fresh_args   = List.map (fun _ -> CM.fresh_arg_name ()) cf1.args in
      let asub1, asub2 = Misc.map_pair (List.map fst <+> Misc.flip List.combine fresh_args) (cf1.args, cf2.args) in
      let cf1, cf2     = Misc.map_pair (replace_arg_names fresh_args) (cf1, cf2) in
        (capturing_subs cf1 lsub1 |> map (f cf1.sto_out lsub1 asub1) |> apply_effects (fe cf1.sto_out lsub1 asub1),
         capturing_subs cf2 lsub2 |> map (f cf2.sto_out lsub2 asub2) |> apply_effects (fe cf2.sto_out lsub2 asub2))

    let rec same_shape cf1 cf2 =
      Misc.same_length (quantified_locs cf1) (quantified_locs cf2) && Misc.same_length cf1.args cf2.args &&
        let cf1, cf2 = normalize_names cf1 cf2 (fun _ _ _ ct -> ct) (fun _ _ _ ct -> ct) in
          List.for_all2 (fun (_, a) (_, b) -> a = b) cf1.args cf2.args
       && cf1.ret = cf2.ret
       && Store.fold_locs begin fun l ld b ->
            b && Store.mem cf2.sto_out l && LDesc.eq ld (Store.find cf2.sto_out l)
          end true cf1.sto_out

    let well_formed globstore cf =
      (* pmr: also need to check sto_out includes sto_in, possibly subtyping *)
      let whole_instore  = Store.upd cf.sto_in globstore in
      let whole_outstore = Store.upd cf.sto_out globstore in
             Store.closed globstore cf.sto_in
          && Store.closed globstore cf.sto_out
          && List.for_all (Store.mem globstore) cf.globlocs
          && not (cf.sto_out |> Store.domain |> List.exists (Misc.flip List.mem cf.globlocs))
          (* && List.for_all (fun (_, ct) -> Store.ctype_closed ct whole_instore) cf.args *)
          (* && Store.ctype_closed cf.ret whole_outstore *)

    let indices cf =
      Store.indices cf.sto_out
	
    let subs_store_var subs lsubs sto cf =
      (* Only sub the locations that are in the store substitution *)
      let lsubs = SVM.fold (fun _ (ss,_) s -> SS.union ss s) subs SS.empty 
               |> SS.elements
	       |> (fun ss -> List.filter (snd <+>Misc.flip List.mem ss) lsubs)
      in
      let cf = capturing_subs cf lsubs in
      let sto_in = Store.subs_store_var subs lsubs sto cf.sto_in in
      let sto_out = Store.subs_store_var subs lsubs sto cf.sto_out in
      let top_of_l l = 
        let so = Ast.Sort.t_ptr (Ast.Sort.Loc (S.to_string l)) in
        let vv = Ast.Symbol.value_variable so in
        (Index.top, FC.make_reft vv so [FC.Conc Ast.pTrue])
      in
      let effects = Store.fold_locs begin fun s _ fx ->
        if (not (SLM.mem s fx)) && S.is_abstract s then 
          SLM.add s (Ref (s, (top_of_l S.none))) fx
        else
          fx
      end (EffectSet.subs lsubs cf.effects) sto_out
      in
      {cf with
        args = CType.subs_store_var subs lsubs sto
            |>Misc.app_snd
            |>Misc.flip List.map cf.args;
        quant_svars = List.filter (not <.>Misc.flip SVM.mem subs) cf.quant_svars;
        sto_in = sto_in;
        sto_out = sto_out;
        effects = effects;
      }
        
    let inst_tvar rename tinst cf = 
      let sub = CType.inst_tvar rename tinst in
      {cf with ret   = sub cf.ret;
              args   = cf.args |>:Misc.app_snd sub;
              sto_in = Store.map sub cf.sto_in;
              sto_out = Store.map sub cf.sto_out;
              quant_tvars = List.filter (not <.> Misc.flip T.TVarInst.mem tinst) cf.quant_tvars;
      }
           
    let instantiate_locs srcinf cf = 
      let qslocs    = quantified_locs cf in
      let instslocs = List.map (S.copy_abstract [srcinf]) qslocs in
      List.combine qslocs instslocs
	
    let rec sto_of_args srcinfo cf args gsto sub tsub =
      List.fold_left2 begin fun (sub,sto) t t' ->
	begin match CType.subs sub t', CType.subs sub t with
	  | Ref (lsub, _), Ref (lsuper, _) -> 
	    let sub' = S.Subst.extend lsuper lsub sub in
	    let usto, usub, tsub = Store.Unify.unify_ctype_locs gsto sub tsub t t' in
	    let sto'  = Store.restrict usto [lsub] 
	             |> Store.upd sto in 
	    (sub', sto')
	  | FRef (f,_), FRef (g,_) ->
	    let sub   = S.Subst.compose (instantiate_locs srcinfo f) sub in
	    let f     = subs f sub in
	    let f     = 
	      {f with args = List.map (Misc.app_snd <| subs_frefs sub) f.args;
	              ret  = subs_frefs sub f.ret} 
	    in
	    let args = List.map snd g.args in
	    let (sub', sto') = 
	      sto_of_args srcinfo f args (Store.upd gsto f.sto_out) sub tsub
	    in
            (sub', Store.subs sub' sto')
	    (* (sub', Store.upd sto (Store.subs sub' sto')) *)
	  | _ -> (sub, sto)
	end
      end (sub, Store.empty) (List.map snd cf.args) args
	
    let instantiate_store srcinfo cf args sto =
      if cf.quant_svars = [] then (cf, S.Subst.empty, StS.empty) else
      	let non_poly_locs = cf.sto_out |> Store.domain in
	let v = List.hd cf.quant_svars in
	let rest = List.tl cf.quant_svars in
	let cf   = {cf with quant_svars = rest} in
	let (sub,sto)  = sto_of_args srcinfo cf args sto S.Subst.empty T.TVarInst.empty in
	let ssub = Store.domain sto
	        |> List.filter (not <.>Misc.flip List.mem non_poly_locs)
                |> List.fold_left (Misc.flip SS.add) SS.empty
		|> fun sset -> StS.extend_sset v sset StS.empty in
	let cf = subs_store_var ssub sub sto cf in
	(cf, sub, ssub)
 
    let is_quant_tvar t cf = List.mem t cf.quant_tvars
      
    let assert_ok_tvar_subst = function
      | FRef _
      | Int _  -> assert false
      | _ -> ()
        
    let tvarinst_of_args cf args = 
      let rec inst_of_args fromas toas = 
        List.fold_left2 begin fun sub formal actual ->
          begin match formal, actual with
            | a, TVar t
            | TVar t, a when is_quant_tvar t cf ->
              let _ = assert_ok_tvar_subst a in
              T.TVarInst.extend t a sub
            | FRef (g, _), FRef (g', _) ->
              inst_of_args (List.map snd g'.args) (List.map snd g.args)
              |> (List.fold_left begin fun sub' (f, t) ->
                T.TVarInst.extend f t sub'
              end sub)
            | _ -> sub
          end
        end [] fromas toas
      in
      inst_of_args (List.map snd cf.args) args
        
    let instantiate_tvars srcinf cf args =
      (* I think any return value should have to share the same variable
	 as some input argument *)
      (* Make sure that function pointers do not have quantified tvars *)
      let rename = cf.quant_tvars 
                   |>: (fun _ -> fresh_tvar ())
                   |> List.combine cf.quant_tvars 
      in
      let inst = tvarinst_of_args (subs_tvar rename cf) (args |>: CType.subs_tvar rename) in
      (cf |> inst_tvar rename inst, rename, inst)

    let instantiate srcinf cf args sto =
      let sub = instantiate_locs srcinf cf in
      let cf = subs cf sub in
      let cf = {cf with args = List.map (Misc.app_snd <| subs_frefs sub) cf.args;
	                 ret = subs_frefs sub cf.ret} in
      let (cf, tsub, tinst) = instantiate_tvars srcinf cf args in
      let (cf, isub, hsub) = instantiate_store srcinf cf args sto in
      (cf, S.Subst.compose isub sub, tsub, tinst, hsub)
        
    let quantified_svars cf = cf.quant_svars
      
    let rec free_svars cf = 
      let vs = Store.vars cf.sto_out
      |> List.filter (not <.> Misc.flip List.mem cf.quant_svars)
      in
      let free  = function
        | FRef (f, _) -> free_svars f
        | _ -> []
      in cf.args |> Misc.flap (free <.> snd) |> List.append vs |> Misc.sort_and_compact
	  
    let quantify_svars cf = 
      let svars = free_svars cf
      |> List.append (snd3 cf.sto_out ++ snd3 cf.sto_in)
      |>Misc.sort_and_compact
      in
      {cf with quant_svars = svars }
	
    let rec free_tvars cf = 
      let vars = function 
        | TVar t -> [t]
        | FRef (f, _) -> free_tvars f
        | _ -> [] in
      cf.ret::List.map snd cf.args 
      |>Misc.flap vars
      |>Misc.sort_and_compact
      |>Misc.filter (not <.> Misc.flip List.mem cf.quant_tvars)
	  
    let quantify_tvars cf = {cf with quant_tvars = free_tvars cf}
      
    let generalize cf = cf |> quantify_svars |> quantify_tvars
        
    let lsubs_of_args f args sub = 
      let flocs, alocs = 
        (List.map snd f.args, args)
      |> Misc.map_pair (Misc.map_partial (CType.subs sub <+> CType.sloc)) in
      List.fold_left2 begin fun sub f a ->
        S.Subst.extend a f sub
      end sub flocs alocs
        
    let safe_update_hsub v sto hsub sub = 
      if SVM.mem v hsub then
        let st' = Store.fold_fields begin fun (sto, sub') s i fld ->
          Store.Unify.add_field sto sub' s i fld
        end (SVM.find v hsub, sub) sto
        in
        SVM.add v (fst st') hsub
      else
        SVM.add v sto hsub

     let map_stores f ({sto_in = sin; sto_out = sout} as t) =
       {t with sto_in = f sin; sto_out = f sout}      
  end

  (******************************************************************************)
  (************************************ Specs ***********************************)
  (******************************************************************************)
  and Spec: SIG.SPEC = struct
    type t = T.spec

    let empty = (SM.empty, SM.empty, Store.empty, SLM.empty)

    let map f (funspec, varspec, storespec, storetypes) =
      (SM.map (f |> CFun.map |> Misc.app_fst) funspec,
       SM.map (f |> Misc.app_fst) varspec,
       Store.map f storespec,
       storetypes)

    let add_fun b fn sp (funspec, varspec, storespec, storetypes) =
      (Misc.sm_protected_add b fn sp funspec, varspec, storespec, storetypes)

    let add_var b vn sp (funspec, varspec, storespec, storetypes) =
      (funspec, Misc.sm_protected_add b vn sp varspec, storespec, storetypes)

    let add_data_loc l (ld, st) (funspec, varspec, storespec, storetypes) =
      (funspec, varspec, Store.add storespec l ld, SLM.add l st storetypes)


    let funspec (fs, _, _, _)        = fs
    let varspec (_, vs, _, _)        = vs
    let store (_, _, sto, _)         = sto
    let locspectypes (_, _, _, lsts) = lsts
    let make w x y z                 = (w, x, y, z)

    let find_cf t fn = funspec t |> SM.find fn 

    let add (funspec, varspec, storespec, storetypes) spec =  
          spec
       |> SM.fold (fun fn sp spec -> add_fun false fn sp spec) funspec
       |> SM.fold (fun vn sp spec -> add_var false vn sp spec) varspec
       |> (fun (w, x, y, z) -> (w, x, Store.upd y storespec, z))

    let d_spec () sp =
      let lspecs = locspectypes sp in
      [ (Store.fold_locs (fun l ld acc ->
          P.concat acc (P.dprintf "loc %a %a %a\n\n"
                          Sloc.d_sloc l d_specTypeRel (SLM.find l lspecs) LDesc.d_ldesc ld)
         ) P.nil (store sp))
      ; (P.seq (P.text "\n\n") (fun (vn, (ct, _)) -> 
          P.dprintf "%s :: @[%a@]" vn CType.d_ctype ct
         ) (varspec sp |> SM.to_list))
      ; (P.seq (P.text "\n\n") (fun (fn, (cf, _)) -> 
          P.dprintf "%s ::@!  @[%a@]\n\n" fn CFun.d_cfun cf
         ) (funspec sp |> SM.to_list)) ]
      |> List.fold_left P.concat P.nil

    let map_stores f (cfs, ctv, gl, s) = 
      let g   = CFun.map_stores f |> Misc.app_fst in
      let cfs = Misc.StringMap.fold begin
        fun fn cf fs -> Misc.StringMap.add fn (g cf) fs end
          cfs Misc.StringMap.empty in
      (cfs, ctv, (f gl), s)

    let map_stores_fn f (cfs, ctv, gl, s) = 
      let g fn = CFun.map_stores (f (Some (fn))) |> Misc.app_fst in
      let cfs  = Misc.StringMap.fold begin
        fun fn cf fs -> Misc.StringMap.add fn (g fn cf) fs end
          cfs Misc.StringMap.empty in
      (cfs, ctv, (f None gl), s)

  end

  (******************************************************************************)
  (******************************* Expression Maps ******************************)
  (******************************************************************************)
  module ExpKey = struct
    type t      = Cil.exp
    let compare = compare
    let print   = CilMisc.pretty_to_format Cil.d_exp
  end

  module ExpMap = Misc.EMap (ExpKey)

  module ExpMapPrinter = P.MakeMapPrinter(ExpMap)

  type ctemap = CType.t ExpMap.t

  let d_ctemap () (em: ctemap): P.doc =
    ExpMapPrinter.d_map "\n" Cil.d_exp CType.d_ctype () em
end

module I    = Make (IndexTypes)

type ctype  = I.CType.t
type cfun   = I.CFun.t
type store  = I.Store.t
type cspec  = I.Spec.t
type ctemap = I.ctemap
type tvinst = IndexTypes.tvinst

let null_fun      = {args = [];
                     ret  = Int (0, N.top);
                     globlocs = [];
                     quant_svars = [];
		                 quant_tvars = [];
                     sto_in = I.Store.empty;
                     sto_out = I.Store.empty;
                     effects = SLM.empty}
  
let void_ctype   = Int  (0, N.top)
let ptr_ctype_of = (fun s -> Ref (s, N.top))
let ptr_ctype    = Ref  (S.none, N.top)

let scalar_ctype = Int  (0, N.top)
let int_ctype    = Int  (4, N.top)
let fptr_ctype   = FRef (null_fun, N.top)

let rec vtype_to_ctype v = if Cil.isArithmeticType v
  then scalar_ctype
  else match v with
    | Cil.TNamed ({C.ttype = v'},_) -> vtype_to_ctype v'
    (* | Cil.TPtr (Cil.TVoid _, _) -> TVar (fresh_tvar ()) *)
    | Cil.TPtr (Cil.TFun _, _) -> fptr_ctype
    | _ -> ptr_ctype


let d_ctype        = I.CType.d_ctype
let index_of_ctype = I.CType.refinement

(*******************************************************************)
(********************* Refined Types and Stores ********************)
(*******************************************************************)

module RefCTypes   = Make (ReftTypes)
module RCt         = RefCTypes

type refctype      = RCt.CType.t
type refcfun       = RCt.CFun.t
type reffield      = RCt.Field.t
type refldesc      = RCt.LDesc.t
type refstore      = RCt.Store.t
type refspec       = RCt.Spec.t

let d_refstore     = RCt.Store.d_store
let d_refcfun      = RCt.CFun.d_cfun

let refstore_partition = RCt.Store.partition

let refstore_set sto l rd =
  try RCt.Store.add sto l rd with Not_found -> 
    assertf "refstore_set"

let refstore_get sto l =
  try RCt.Store.find sto l with Not_found ->
    (Errormsg.error "Cannot find location %a in store %a\n" Sloc.d_sloc l RCt.Store.d_store sto;
     asserti false "refstore_get"; assert false)

let refldesc_subs rd f =
  RCt.LDesc.mapn (fun i pl fld -> RCt.Field.map_type (f i pl) fld) rd

(*******************************************************************)
(******************** Operations on Refined Stores *****************)
(*******************************************************************)

let refdesc_find i rd = 
  match RCt.LDesc.find i rd with
  | [(i', rfld)] -> (rfld, Index.is_periodic i')
  | _            -> assertf "refdesc_find"

let addr_of_refctype loc = function
  | Ref (cl, (i,_)) when not (Sloc.is_abstract cl) ->
      (cl, i)
  | ARef -> (Sloc.sloc_of_any, Index.ind_of_any)
  | cr   ->
      let s = cr  |> d_refctype () |> P.sprint ~width:80 in
      let l = loc |> Cil.d_loc () |> P.sprint ~width:80 in
      let _ = asserti false "addr_of_refctype: bad arg %s at %s \n" s l in
      assert false

let ac_refstore_read loc sto cr = 
  let (l, ix) = addr_of_refctype loc cr in 
     l
  |> RCt.Store.find sto 
  |> refdesc_find ix

(* API *)
let refstore_read loc sto cr = 
  ac_refstore_read loc sto cr |> fst

(* API *)
let is_soft_ptr loc sto cr = 
  ac_refstore_read loc sto cr |> snd

(* API *)
let refstore_write loc sto rct rct' = 
  let (cl, ix) = addr_of_refctype loc rct in
  let _  = assert (not (Sloc.is_abstract cl)) in
  let ld = RCt.Store.find sto cl in
  let ld = RCt.LDesc.remove ix ld in
  let ld = RCt.LDesc.add ix (RCt.Field.create Nonfinal dummy_fieldinfo rct') ld in
  RCt.Store.add sto cl ld

(* API *)
let ctype_of_refctype = function
  | Int (x, (y, _))  -> Int (x, y) 
  | Ref (x, (y, _))  -> Ref (x, y)
  | ARef             -> ARef
  | Any              -> Any  
  | TVar t           -> TVar t
  | f -> RCt.CType.map fst f
    
(* API *)
let free_svars = function
  | FRef (f, _) -> RCt.CFun.free_svars f
  | _           -> []

(* API *)
let cfun_of_refcfun   = I.CFun.map ctype_of_refctype 
let cspec_of_refspec  = I.Spec.map (RCt.CType.map (fun (i,_) -> i))
let store_of_refstore = I.Store.map ctype_of_refctype
let args_of_refcfun   = fun ft -> ft.args
let ret_of_refcfun    = fun ft -> ft.ret
let stores_of_refcfun = fun ft -> (ft.sto_in, ft.sto_out)

let reft_of_refctype = function
  | Int (_,(_,r)) 
  | Ref (_,(_,r))
  | FRef (_,(_,r)) -> r
  | Any | ARef | TVar _ -> reft_of_top

(**********************************************************************)


