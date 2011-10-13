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

module M   = Misc
module P   = Pretty
module E   = Errormsg
module S   = Sloc
module SS  = S.SlocSet
module N   = Index
module C   = Cil
module CM  = CilMisc
module FC  = FixConstraint
module SM  = M.StringMap
module SLM = S.SlocMap

module SLMPrinter = P.MakeMapPrinter(SLM)

open M.Ops

(******************************************************************************)
(*********************************** Indices **********************************)
(******************************************************************************)


module IndexSetPrinter = P.MakeSetPrinter (N.IndexSet)

(******************************************************************************)
(****************************** Type Refinements ******************************)
(******************************************************************************)

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

(******************************************************************************)
(************************* Refctypes and Friends ******************************)
(******************************************************************************)

let reft_of_top = 
  let so = Ast.Sort.t_obj in
  let vv = Ast.Symbol.value_variable so in
  FC.make_reft vv so []

let d_reft () r = 
  Misc.fsprintf (FC.print_reft_pred None) r |> P.text

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
end

(******************************************************************************)
(***************************** Parameterized Types ****************************)
(******************************************************************************)

type finality =
  | Final
  | Nonfinal

type fieldinfo  = {fname : string option; ftype : Cil.typ option} 
type structinfo = {stype : Cil.typ option} 

let dummy_fieldinfo  = {fname = None; ftype = None}
let dummy_structinfo = {stype = None}

type 'a prectype =
  | Int  of int * 'a        (* fixed-width integer *)
  | Ref  of Sloc.t * 'a     (* reference *)

type 'a prefield = { pftype     : 'a prectype
                   ; pffinal    : finality
                   ; pfloc      : C.location
                   ; pfinfo     : fieldinfo }

type effectinfo = Reft.t prectype

type 'a preldesc = { plfields   : (Index.t * 'a prefield) list
                   ; plwrite    : effectinfo
                   ; plinfo     : structinfo }

type 'a prestore = 'a preldesc Sloc.SlocMap.t * 'a precfun Sloc.SlocMap.t

and 'a precfun =
    { args        : (string * 'a prectype) list;  (* arguments *)
      ret         : 'a prectype;                  (* return *)
      globlocs    : S.t list;                     (* unquantified locations *)
      sto_in      : 'a prestore;                  (* in store *)
      sto_out     : 'a prestore;                  (* out store *)
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
      P.nil

let d_structinfo () = function
  | { stype = Some t } -> 
      P.dprintf "/* %a */" Cil.d_type t
  | _ -> 
      P.nil

let d_prectype d_refinement () = function
      | Int (n, r) -> P.dprintf "int(%d, %a)" n d_refinement r
      | Ref (s, r) -> P.dprintf "ref(%a, %a)" S.d_sloc s d_refinement r

let d_refctype = d_prectype Reft.d_refinement

let d_effectinfo = d_refctype

module type CTYPE_DEFS = sig
  module R : CTYPE_REFINEMENT

  type refinement = R.t

  type ctype = refinement prectype
  type field = refinement prefield
  type ldesc = refinement preldesc
  type store = refinement prestore
  type cfun  = refinement precfun
  type spec  = refinement prespec
end

module MakeTypes (R : CTYPE_REFINEMENT): CTYPE_DEFS with module R = R = struct
  module R = R

  type refinement = R.t

  type ctype = refinement prectype
  type field = refinement prefield
  type ldesc = refinement preldesc
  type store = refinement prestore
  type cfun  = refinement precfun
  type spec  = refinement prespec
end

module IndexTypes = MakeTypes (IndexRefinement)
module ReftTypes  = MakeTypes (Reft)

module SIGS (T : CTYPE_DEFS) = struct
  module type CTYPE = sig
    type t = T.ctype

    exception NoLUB of t * t

    val refinement  : t -> T.refinement
    val map         : ('a -> 'b) -> 'a prectype -> 'b prectype
    val d_ctype     : unit -> t -> P.doc
    val of_const    : Cil.constant -> t
    val is_subctype : t -> t -> bool
    val width       : t -> int
    val sloc        : t -> Sloc.t option
    val subs        : Sloc.Subst.t -> t -> t
    val eq          : t -> t -> bool
    val collide     : Index.t -> t -> Index.t -> t -> bool
    val is_void     : t -> bool
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
      
    val d_field       : unit -> t -> P.doc
  end

  module type LDESC = sig
    type t = T.ldesc

    exception TypeDoesntFit of Index.t * T.ctype * t

    val empty         : S.t -> t
    val eq            : t -> t -> bool
    val is_empty      : t -> bool
    val is_read_only  : t -> bool
    val add           : Index.t -> T.field -> t -> t
    val create        : S.t -> structinfo -> (Index.t * T.field) list -> t
    val remove        : Index.t -> t -> t
    val mem           : Index.t -> t -> bool
    val referenced_slocs : t -> Sloc.t list
    val find          : Index.t -> t -> (Index.t * T.field) list
    val foldn         : (int -> 'a -> Index.t -> T.field -> 'a) -> 'a -> t -> 'a
    val fold          : ('a -> Index.t -> T.field -> 'a) -> 'a -> t -> 'a
    val subs          : Sloc.Subst.t -> t -> t
    val map           : ('a prefield -> 'b prefield) -> 'a preldesc -> 'b preldesc
    val mapn          : (int -> Index.t -> 'a prefield -> 'b prefield) -> 'a preldesc -> 'b preldesc
    val map_effects   : (effectinfo -> effectinfo) -> t -> t
    val iter          : (Index.t -> T.field -> unit) -> t -> unit
    val indices       : t -> Index.t list
    val bindings      : t -> (Index.t * T.field) list

    val set_structinfo : t -> structinfo -> t
    val get_structinfo : t -> structinfo

    val get_write_effect : t -> effectinfo
    val set_write_effect : t -> effectinfo -> t

    val d_ldesc       : unit -> t -> P.doc
  end

  module type STORE = sig
    type t = T.store

    val empty        : t
    val bindings     : 'a prestore -> (Sloc.t * 'a preldesc) list * (Sloc.t * 'a precfun) list
    val domain       : t -> Sloc.t list
    val mem          : t -> Sloc.t -> bool
    val closed       : t -> t -> bool
    val reachable    : t -> Sloc.t -> Sloc.t list
    val restrict     : t -> Sloc.t list -> t
    val map          : ('a prectype -> 'b prectype) -> 'a prestore -> 'b prestore
    val map_effects  : (effectinfo -> effectinfo) -> t -> t
    val map_variances : ('a prectype -> 'b prectype) ->
                        ('a prectype -> 'b prectype) ->
                        'a prestore ->
                        'b prestore
    val map_ldesc    : (Sloc.t -> 'a preldesc -> 'a preldesc) -> 'a prestore -> 'a prestore
    val partition    : (Sloc.t -> bool) -> t -> t * t
    val remove       : t -> Sloc.t -> t
    val upd          : t -> t -> t
  (** [upd st1 st2] returns the store obtained by adding the locations from st2 to st1,
      overwriting the common locations of st1 and st2 with the blocks appearing in st2 *)
    val subs         : Sloc.Subst.t -> t -> t
    val ctype_closed : T.ctype -> t -> bool
    val indices      : t -> Index.t list

    val data         : t -> t

    val d_store_addrs: unit -> t -> P.doc
    val d_store      : unit -> t -> P.doc

    module Data: sig
      val add           : t -> Sloc.t -> T.ldesc -> t
      val bindings      : t -> (Sloc.t * T.ldesc) list
      val domain        : t -> Sloc.t list
      val mem           : t -> Sloc.t -> bool
      val ensure_sloc   : t -> Sloc.t -> t
      val find          : t -> Sloc.t -> T.ldesc
      val find_or_empty : t -> Sloc.t -> T.ldesc
      val map           : (T.ctype -> T.ctype) -> t -> t
      val fold_fields   : ('a -> Sloc.t -> Index.t -> T.field -> 'a) -> 'a -> t -> 'a
      val fold_locs     : (Sloc.t -> T.ldesc -> 'a -> 'a) -> 'a -> t -> 'a
    end

    module Function: sig
      val add       : 'a prestore -> Sloc.t -> 'a precfun -> 'a prestore
      val bindings  : 'a prestore -> (Sloc.t * 'a precfun) list
      val domain    : t -> Sloc.t list
      val mem       : 'a prestore -> Sloc.t -> bool
      val find      : 'a prestore -> Sloc.t -> 'a precfun
      val fold_locs : (Sloc.t -> 'b precfun -> 'a -> 'a) -> 'a -> 'b prestore -> 'a
    end

    module Unify: sig
      exception UnifyFailure of Sloc.Subst.t * t

      val unify_ctype_locs : t -> Sloc.Subst.t -> T.ctype -> T.ctype -> t * Sloc.Subst.t
      val unify_overlap    : t -> Sloc.Subst.t -> Sloc.t -> Index.t -> t * Sloc.Subst.t
      val add_field        : t -> Sloc.Subst.t -> Sloc.t -> Index.t -> T.field -> t * Sloc.Subst.t
      val add_fun          : t -> Sloc.Subst.t -> Sloc.t -> T.cfun -> t * Sloc.Subst.t
    end
  end

  module type CFUN = sig
    type t = T.cfun

    val d_cfun          : unit -> t -> P.doc
    val map             : ('a prectype -> 'b prectype) -> 'a precfun -> 'b precfun
    val map_variances   : ('a prectype -> 'b prectype) ->
                          ('a prectype -> 'b prectype) ->
                          'a precfun ->
                          'b precfun
    val map_ldesc       : (Sloc.t -> 'a preldesc -> 'a preldesc) -> 'a precfun -> 'a precfun
    val map_effects     : (effectinfo -> effectinfo) -> t -> t
    val well_formed     : T.store -> t -> bool
    val normalize_names :
      t ->
      t ->
      (T.store -> Sloc.Subst.t -> (string * string) list -> T.ctype -> T.ctype) ->
      (T.store -> Sloc.Subst.t -> (string * string) list -> effectinfo -> effectinfo) ->
      t * t
    val same_shape      : t -> t -> bool
    val quantified_locs : t -> Sloc.t list
    val instantiate     : CM.srcinfo -> t -> t * S.Subst.t
    val make            : (string * T.ctype) list -> S.t list -> T.store -> T.ctype -> T.store -> t
    val subs            : t -> Sloc.Subst.t -> t
    val indices         : t -> Index.t list
  end

  module type SPEC = sig
    type t      = T.spec

    val empty   : t

    val map : ('a prectype -> 'b prectype) -> 'a prespec -> 'b prespec
    val add_fun : bool -> string -> T.cfun * specType -> t -> t
    val add_var : bool -> string -> T.ctype * specType -> t -> t
    val add_data_loc : Sloc.t -> T.ldesc * specType -> t -> t
    val add_fun_loc  : Sloc.t -> T.cfun * specType -> t -> t
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

let prectype_subs subs = function
  | Ref (s, i) -> Ref (S.Subst.apply subs s, i)
  | pct        -> pct

module Make (T: CTYPE_DEFS): S with module T = T = struct
  module T   = T
  module SIG = SIGS (T)

  (***********************************************************************)
  (***************************** Types ***********************************)
  (***********************************************************************)

  module rec CType: SIG.CTYPE = struct
    type t = T.ctype

    let refinement = function
      | Int (_, r) | Ref (_, r) -> r

    let map f = function
      | Int (i, x) -> Int (i, f x)
      | Ref (l, x) -> Ref (l, f x)

    let d_ctype () = function
      | Int (n, i) -> P.dprintf "int(%d, %a)" n T.R.d_refinement i
      | Ref (s, i) -> P.dprintf "ref(%a, %a)" S.d_sloc s T.R.d_refinement i

    let width = function
      | Int (n, _) -> n
      | Ref _      -> CM.int_width
    
    let sloc = function
      | Ref (s, _) -> Some s
      | _          -> None

    let subs subs = function
      | Ref (s, i) -> Ref (S.Subst.apply subs s, i)
      | pct        -> pct

    exception NoLUB of t * t

    let is_subctype pct1 pct2 =
      match pct1, pct2 with
        | Int (n1, r1), Int (n2, r2) when n1 = n2    -> T.R.is_subref r1 r2
        | Ref (s1, r1), Ref (s2, r2) when S.eq s1 s2 -> T.R.is_subref r1 r2
        | _                                          -> false

    let of_const c =
      let r = T.R.of_const c in
        match c with
          | C.CInt64 (v, ik, _) -> Int (C.bytesSizeOfInt ik, r)
          | C.CChr c            -> Int (CM.int_width, r)
          | C.CReal (_, fk, _)  -> Int (CM.bytesSizeOfFloat fk, r)
          | C.CStr s            -> Ref (S.fresh_abstract [CM.srcinfo_of_constant c None] , r)
          | _                   -> halt <| E.bug "Unimplemented ctype_of_const: %a@!@!" C.d_const c

    let eq pct1 pct2 =
      match (pct1, pct2) with
        | Ref (l1, i1), Ref (l2, i2) -> S.eq l1 l2 && i1 = i2
        | _                          -> pct1 = pct2

    let index_overlaps_type i i2 pct =
      M.foldn (fun b n -> b || N.overlaps i (N.offset n i2)) (width pct) false

    let extrema_in i1 pct1 i2 pct2 =
      index_overlaps_type i1 i2 pct2 || index_overlaps_type (N.offset (width pct1 - 1) i1) i2 pct2

    let collide i1 pct1 i2 pct2 =
      extrema_in i1 pct1 i2 pct2 || extrema_in i2 pct2 i1 pct1

    let is_void = function
      | Int (0, _) -> true
      | _          -> false
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

    let empty l =
      let dummy_effectinfo = Ref (l, Reft.top) in
        {plfields = []; plinfo = dummy_structinfo; plwrite = dummy_effectinfo}

    let eq {plfields = cs1} {plfields = cs2} =
      Misc.same_length cs1 cs2 &&
        List.for_all2
          (fun (i1, f1) (i2, f2) -> i1 = i2 && Field.type_of f1 = Field.type_of f2)
          cs1 cs2

    let is_empty {plfields = flds} =
      flds = []

    let is_read_only {plfields = flds} =
      List.for_all (fun (_, {pffinal = fnl}) -> fnl = Final) flds

    let fits i fld {plfields = cs} =
      let t = Field.type_of fld in
      let w = CType.width t in
        M.get_option w (N.period i) >= w &&
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

    let create l si flds =
      List.fold_right (M.uncurry add) flds {(empty l) with plinfo = si}

    let mem i {plfields = flds} =
      List.exists (fun (i2, _) -> N.is_subindex i i2) flds

    let find i {plfields = flds} =
      List.filter (fun (i2, _) -> N.overlaps i i2) flds

    let rec foldn_aux f n b = function
      | []               -> b
      | (i, fld) :: flds -> foldn_aux f (n + 1) (f n b i fld) flds

    let foldn f b ld =
      foldn_aux f 0 b ld.plfields

    let fold f b flds =
      foldn (fun _ b i fld -> f b i fld) b flds

    let mapn f ld =
      {ld with plfields = M.mapi (fun n (i, fld) -> (i, f n i fld)) ld.plfields}

    let map f flds =
      mapn (fun _ _ fld -> f fld) flds

    let map_effects f ld =
      {ld with plwrite = f ld.plwrite}

    let subs sub ld =
         ld
      |> map (Field.map_type (CType.subs sub))
      |> map_effects (prectype_subs sub)

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

    let get_structinfo {plinfo = si} =
      si

    let set_structinfo ld si =
      {ld with plinfo = si}

    let get_write_effect {plwrite = w} =
      w

    let set_write_effect ld w =
      {ld with plwrite = w}

    let d_ldesc () {plfields = flds; plwrite = w} =
      P.dprintf "@[%t <*write: %a>@]"
        begin fun () ->
          P.seq
            (P.dprintf ",@!")
            (fun (i, fld) -> P.dprintf "%a: %a" Index.d_index i Field.d_field fld)
            flds
        end
        d_effectinfo w
  end

  and Store: SIG.STORE = struct
    type t = T.store

    let empty = (SLM.empty, SLM.empty)

    let map_data f =
      f |> Field.map_type |> LDesc.map |> SLM.map

    let map_function f =
      SLM.map (CFun.map f)

    let map_ldesc f (ds, fs) =
      (SLM.mapi f ds, SLM.map (CFun.map_ldesc f) fs)

    let map_effects f sto =
      map_ldesc (const <| LDesc.map_effects f) sto

    module Data = struct
      let add (ds, fs) l ld =
        let _ = assert (not (SLM.mem l fs)) in
          (SLM.add l ld ds, fs)

      let bindings (ds, _) =
        SLM.to_list ds

      let domain (ds, _) =
        SLM.domain ds

      let mem (ds, _) l =
        SLM.mem l ds

      let find (ds, _) l =
        SLM.find l ds

      let find_or_empty sto l =
        try find sto l with Not_found -> LDesc.empty l

      let ensure_sloc sto l =
        l |> find_or_empty sto |> add sto l

      let map f (ds, fs) =
        (map_data f ds, fs)

      let fold_fields f b (ds, fs) =
        SLM.fold (fun l ld b -> LDesc.fold (fun b i pct -> f b l i pct) b ld) ds b

      let fold_locs f b (ds, fs) =
        SLM.fold f ds b
    end

    module Function = struct
      let add (ds, fs) l cf =
        let _ = assert (not (SLM.mem l ds)) in
        let _ = assert (Sloc.is_abstract l) in
          (ds, SLM.add l cf fs)

      let bindings (_, fs) =
        SLM.to_list fs

      let domain (_, fs) =
        SLM.domain fs

      let mem (_, fs) l =
        SLM.mem l fs

      let find (_, fs) l =
        SLM.find l fs

      let fold_locs f b (_, fs) =
        SLM.fold f fs b
    end

    let map_variances f_co f_contra (ds, fs) =
      (map_data f_co ds, SLM.map (CFun.map_variances f_co f_contra) fs)

    let map f (ds, fs) =
      (map_data f ds, map_function f fs)

    let bindings sto =
      (Data.bindings sto, Function.bindings sto)

    let domain sto =
      Data.domain sto ++ Function.domain sto

    let mem (ds, fs) s =
      SLM.mem s ds || SLM.mem s fs

    let subs_slm_dom subs m =
      SLM.fold (fun l d m -> SLM.add (S.Subst.apply subs l) d m) m SLM.empty

    let subs_addrs subs (ds, fs) =
      (subs_slm_dom subs ds, subs_slm_dom subs fs)

    let subs subs (ds, fs) =
      (SLM.map (LDesc.subs subs) ds, fs |> SLM.map (M.flip CFun.subs subs)) |> subs_addrs subs

    let remove (ds, fs) l =
      (SLM.remove l ds, SLM.remove l fs)

    let upd (ds1, fs1) (ds2, fs2) =
      (SLM.fold SLM.add ds2 ds1, SLM.fold SLM.add fs2 fs1)

    let partition_map f m =
      SLM.fold begin fun l d (m1, m2) ->
        if f l then (SLM.add l d m1, m2) else (m1, SLM.add l d m2)
      end m (SLM.empty, SLM.empty)

    let partition f (ds, fs) =
      let ds1, ds2 = partition_map f ds in
      let fs1, fs2 = partition_map f fs in
        ((ds1, fs1), (ds2, fs2))

    let ctype_closed t sto = match t with
      | Ref (l, _) -> mem sto l
      | _          -> true

    let rec reachable_aux sto visited l =
      if SS.mem l visited then
        visited
      else if Function.mem sto l then
        SS.add l visited
      else begin
           l
        |> Data.find sto
        |> LDesc.referenced_slocs
        |> List.fold_left (reachable_aux sto) (SS.add l visited)
      end

    let reachable sto l =
      l |> reachable_aux sto SS.empty |> SS.elements

    let restrict sto ls =
         sto
      |> partition (ls |> M.flap (reachable sto) |> M.sort_and_compact |> M.flip List.mem)
      |> fst

    let rec closed globstore ((_, fs) as sto) =
      Data.fold_fields
        (fun c _ _ fld -> c && ctype_closed (Field.type_of fld) (upd globstore sto)) true sto &&
        SLM.fold (fun _ cf c -> c && CFun.well_formed globstore cf) fs true

    let slm_acc_list f m =
      SLM.fold (fun _ d acc -> f d ++ acc) m []

    let indices (ds, fs) =
      slm_acc_list LDesc.indices ds ++ slm_acc_list CFun.indices fs

    let data (ds, _) =
      (ds, SLM.empty)

    let d_store_addrs () st =
      P.seq (P.text ",") (Sloc.d_sloc ()) (domain st)

    let d_slm d_binding =
      SLMPrinter.docMap ~sep:(P.dprintf ";@!") (fun l d -> P.dprintf "%a |-> %a" S.d_sloc l d_binding d)

    let d_store () (ds, fs) =
      if fs = SLM.empty then
        P.dprintf "[@[%a@]]" (d_slm LDesc.d_ldesc) ds
      else if ds = SLM.empty then
        P.dprintf "[@[%a@]]" (d_slm CFun.d_cfun) fs
      else
        P.dprintf "[@[%a;@!%a@]]" (d_slm LDesc.d_ldesc) ds (d_slm CFun.d_cfun) fs

    module Unify = struct
      exception UnifyFailure of S.Subst.t * t

      let fail sub sto _ =
        raise (UnifyFailure (sub, sto))

      let rec unify_ctype_locs sto sub ct1 ct2 = match CType.subs sub ct1, CType.subs sub ct2 with
        | Int (n1, _), Int (n2, _) when n1 = n2 -> (sto, sub)
        | Ref (s1, _), Ref (s2, _)              -> unify_locations sto sub s1 s2
        | ct1, ct2                              ->
          fail sub sto <| C.error "Cannot unify locations of %a and %a@!" CType.d_ctype ct1 CType.d_ctype ct2

      and unify_data_locations sto sub s1 s2 =
        let ld1, ld2 = M.map_pair (Data.find_or_empty sto <+> LDesc.subs sub) (s1, s2) in
        let sto      = remove sto s1 in
        let sto      = ld2 |> Data.add sto s2 |> subs sub in
          LDesc.fold (fun (sto, sub) i f -> add_field sto sub s2 i f) (sto, sub) ld1

      and unify_fun_locations sto sub s1 s2 =
        if Function.mem sto s1 then
          let cf1 = CFun.subs (Function.find sto s1) sub in
          let sto = s1 |> remove sto |> subs sub in
          if Function.mem sto s2 then
            let cf2 = CFun.subs (Function.find sto s2) sub in
            if CFun.same_shape cf1 cf2 then
              (sto, sub)
            else
              fail sub sto <|
                  C.error "Trying to unify locations %a, %a with different function types:@!@!%a: %a@!@!%a: %a@!"
                    S.d_sloc_info s1 S.d_sloc_info s2 S.d_sloc_info s1 CFun.d_cfun cf1 S.d_sloc_info s2 CFun.d_cfun cf2
          else (Function.add sto s2 cf1, sub)
        else (subs sub sto, sub)

      and assert_unifying_same_location_type sto sub s1 s2 =
        if (Function.mem sto s1 && Data.mem sto s2) ||
          (Data.mem sto s1 && Function.mem sto s2) then
            fail sub sto <| C.error "Trying to unify data and function locations (%a, %a) in store@!%a@!"
                S.d_sloc_info s1 S.d_sloc_info s2 d_store sto
        else ()

      and unify_locations sto sub s1 s2 =
        if not (S.eq s1 s2) then
          let _   = assert_unifying_same_location_type sto sub s1 s2 in
          let sub = S.Subst.extend s1 s2 sub in
            if Function.mem sto s1 || Function.mem sto s2 then
              unify_fun_locations sto sub s1 s2
            else if Data.mem sto s1 || Data.mem sto s2 then
              unify_data_locations sto sub s1 s2
            else (subs sub sto, sub)
            else (sto, sub)

      and unify_fields sto sub fld1 fld2 = match M.map_pair (Field.type_of <+> CType.subs sub) (fld1, fld2) with
        | ct1, ct2                   when ct1 = ct2 -> (sto, sub)
        | Ref (s1, i1), Ref (s2, i2) when i1 = i2   -> unify_locations sto sub s1 s2
        | ct1, ct2                                  ->
          fail sub sto <| C.error "Cannot unify %a and %a@!" CType.d_ctype ct1 CType.d_ctype ct2

      and unify_overlap sto sub s i =
        let s  = S.Subst.apply sub s in
        let ld = Data.find_or_empty sto s in
          match LDesc.find i ld with
            | []                         -> (sto, sub)
            | ((_, fstfld) :: _) as olap ->
              let i = olap |>: fst |> List.fold_left Index.lub i in
                   ld
                |> List.fold_right (fst <+> LDesc.remove) olap
                |> LDesc.add i fstfld
                |> Data.add sto s
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
              let ld       = Data.find_or_empty sto s in
                begin match LDesc.find i ld with
                  | []          -> (ld |> LDesc.add i fld |> Data.add sto s, sub)
                  | [(_, fld2)] -> unify_fields sto sub fld fld2
                  | _           -> assert false
                end
          end
        with e ->
          C.error "Can't fit @!%a: %a@!  in location@!%a |-> %a"
            Index.d_index i Field.d_field fld S.d_sloc_info s LDesc.d_ldesc (Data.find_or_empty sto s) |> ignore;
          raise e

      let add_fun sto sub l cf =
        let l = S.Subst.apply sub l in
          if not (Data.mem sto l) then
            if Function.mem sto l then
              let _ = assert (CFun.same_shape cf (Function.find sto l)) in
                (sto, sub)
            else (Function.add sto l cf, sub)
          else fail sub sto <| C.error "Attempting to store function in location %a, which contains: %a@!"
                 S.d_sloc_info l LDesc.d_ldesc (Data.find sto l)
    end
  end

  (******************************************************************************)
  (******************************* Function Types *******************************)
  (******************************************************************************)
  and CFun: SIG.CFUN = struct
    type t = T.cfun

    (* API *)
    let make args globs sin reto sout =
      { args     = args;
        ret      = reto;
        globlocs = globs;
        sto_in   = sin;
        sto_out  = sout;
      }

    let map_variances f_co f_contra ft =
      { args     = List.map (Misc.app_snd f_contra) ft.args;
        ret      = f_co ft.ret;
        globlocs = ft.globlocs;
        sto_in   = Store.map_variances f_contra f_co ft.sto_in;
        sto_out  = Store.map_variances f_co f_contra ft.sto_out;
      }

    let map f ft =
      map_variances f f ft

    let map_ldesc f ft =
      { ft with 
        sto_in = Store.map_ldesc f ft.sto_in
      ; sto_out = Store.map_ldesc f ft.sto_out }

    let map_effects f ft =
      map_ldesc (const <| LDesc.map_effects f) ft

    let quantified_locs {sto_out = sto} =
      Store.domain sto

    let d_slocs () slocs = P.dprintf "[%t]" (fun _ -> P.seq (P.text ";") (S.d_sloc ()) slocs)
    let d_arg () (x, ct) = P.dprintf "%s : %a" x CType.d_ctype ct
    let d_args () args   = P.seq (P.dprintf ",@!") (d_arg ()) args

    let d_argret () ft =
      P.dprintf "arg       (@[%a@])\nret       %a\n"
        d_args ft.args
        CType.d_ctype ft.ret

    let d_globlocs () ft =
      if ft.globlocs = [] then
        P.nil
      else
        P.dprintf "global    %a\n" d_slocs ft.globlocs

    let d_stores () ft =
      if ft.sto_in = ft.sto_out then
        P.dprintf "store     %a" Store.d_store ft.sto_in
      else
        P.dprintf "store_in  %a\nstore_out %a"
          Store.d_store ft.sto_in
          Store.d_store ft.sto_out

    let d_cfun () ft  =
      P.dprintf "@[%a%a%a@]" d_argret ft d_globlocs ft d_stores ft

    let capturing_subs cf sub =
      let apply_sub = CType.subs sub in
        make (List.map (M.app_snd apply_sub) cf.args)
             (List.map (S.Subst.apply sub) cf.globlocs)
             (Store.subs sub cf.sto_in)
             (apply_sub cf.ret)
             (Store.subs sub cf.sto_out)

    let subs cf sub =
      cf |> quantified_locs |> S.Subst.avoid sub |> capturing_subs cf

    let rec order_locs_aux sto ord = function
      | []      -> ord
      | l :: ls ->
          if not (List.mem l ord) then
            let ls = if Store.Data.mem sto l then ls @ (l |> Store.Data.find sto |> LDesc.referenced_slocs) else ls in
              order_locs_aux sto (l :: ord) ls
          else order_locs_aux sto ord ls

    let ordered_locs ({args = args; ret = ret; sto_out = sto} as cf) =
      let ord = (CType.sloc ret :: List.map (snd <+> CType.sloc) args)
             |> M.maybe_list
             |> order_locs_aux sto []
             |> M.mapi (fun i x -> (x, i)) in
      cf |> quantified_locs |> M.fsort (M.flip List.assoc ord)

    let replace_arg_names anames cf =
      {cf with args = List.map2 (fun an (_, t) -> (an, t)) anames cf.args}

    let normalize_names cf1 cf2 f fe =
      let ls1, ls2     = M.map_pair ordered_locs (cf1, cf2) in
      let fresh_locs   = List.map (Sloc.to_slocinfo <+> Sloc.fresh_abstract) ls1 in
      let lsub1, lsub2 = M.map_pair (M.flip List.combine fresh_locs) (ls1, ls2) in
      let fresh_args   = List.map (fun _ -> CM.fresh_arg_name ()) cf1.args in
      let asub1, asub2 = M.map_pair (List.map fst <+> M.flip List.combine fresh_args) (cf1.args, cf2.args) in
      let cf1, cf2     = M.map_pair (replace_arg_names fresh_args) (cf1, cf2) in
        (capturing_subs cf1 lsub1 |> map (f cf1.sto_out lsub1 asub1) |> map_effects (fe cf1.sto_out lsub1 asub1),
         capturing_subs cf2 lsub2 |> map (f cf2.sto_out lsub2 asub2) |> map_effects (fe cf2.sto_out lsub2 asub2))

    let rec same_shape cf1 cf2 =
      M.same_length (quantified_locs cf1) (quantified_locs cf2) && M.same_length cf1.args cf2.args &&
        let cf1, cf2 = normalize_names cf1 cf2 (fun _ _ _ ct -> ct) (fun _ _ _ ct -> ct) in
          List.for_all2 (fun (_, a) (_, b) -> a = b) cf1.args cf2.args
       && cf1.ret = cf2.ret
       && Store.Data.fold_locs begin fun l ld b ->
            b && Store.Data.mem cf2.sto_out l && LDesc.eq ld (Store.Data.find cf2.sto_out l)
          end true cf1.sto_out
       && Store.Function.fold_locs begin fun l cf b ->
              b && Store.Function.mem cf2.sto_out l && same_shape cf (Store.Function.find cf2.sto_out l)
          end true cf1.sto_out

    let well_formed globstore cf =
      (* pmr: also need to check sto_out includes sto_in, possibly subtyping *)
      let whole_instore  = Store.upd cf.sto_in globstore in
      let whole_outstore = Store.upd cf.sto_out globstore in
             Store.closed globstore cf.sto_in
          && Store.closed globstore cf.sto_out
          && List.for_all (Store.mem globstore) cf.globlocs
          && not (cf.sto_out |> Store.domain |> List.exists (M.flip List.mem cf.globlocs))
          && List.for_all (fun (_, ct) -> Store.ctype_closed ct whole_instore) cf.args
          && Store.ctype_closed cf.ret whole_outstore

    let indices cf =
      Store.indices cf.sto_out

    let instantiate srcinf cf =
      let qslocs    = quantified_locs cf in
      let instslocs = List.map (fun _ -> S.fresh_abstract [srcinf]) qslocs in
      let sub       = List.combine qslocs instslocs in
        (subs cf sub, sub)
  end

  (******************************************************************************)
  (************************************ Specs ***********************************)
  (******************************************************************************)
  and Spec: SIG.SPEC = struct
    type t = T.spec

    let empty = (SM.empty, SM.empty, Store.empty, SLM.empty)

    let map f (funspec, varspec, storespec, storetypes) =
      (SM.map (f |> CFun.map |> M.app_fst) funspec,
       SM.map (f |> M.app_fst) varspec,
       Store.map f storespec,
       storetypes)

    let add_fun b fn sp (funspec, varspec, storespec, storetypes) =
      (Misc.sm_protected_add b fn sp funspec, varspec, storespec, storetypes)

    let add_var b vn sp (funspec, varspec, storespec, storetypes) =
      (funspec, Misc.sm_protected_add b vn sp varspec, storespec, storetypes)

    let add_data_loc l (ld, st) (funspec, varspec, storespec, storetypes) =
      (funspec, varspec, Store.Data.add storespec l ld, SLM.add l st storetypes)

    let add_fun_loc l (cf, st) (funspec, varspec, storespec, storetypes) =
      (funspec, varspec, Store.Function.add storespec l cf, SLM.add l st storetypes)
      
    let funspec (fs, _, _, _)        = fs
    let varspec (_, vs, _, _)        = vs
    let store (_, _, sto, _)         = sto
    let locspectypes (_, _, _, lsts) = lsts
    let make w x y z                 = (w, x, y, z)

    let add (funspec, varspec, storespec, storetypes) spec =  
          spec
       |> SM.fold (fun fn sp spec -> add_fun false fn sp spec) funspec
       |> SM.fold (fun vn sp spec -> add_var false vn sp spec) varspec
       |> (fun (w, x, y, z) -> (w, x, Store.upd y storespec, z))

    let d_spec () sp =
      let lspecs = locspectypes sp in
      [ (Store.Data.fold_locs (fun l ld acc ->
          P.concat acc (P.dprintf "loc %a %a %a\n\n"
                          Sloc.d_sloc l d_specTypeRel (SLM.find l lspecs) LDesc.d_ldesc ld)
         ) P.nil (store sp))
      ; (Store.Function.fold_locs (fun l cf acc ->
          P.concat acc  (P.dprintf "loc %a %a@!  @[%a@]@!@!"
                           Sloc.d_sloc l d_specTypeRel (SLM.find l lspecs) CFun.d_cfun cf)
         ) P.nil (store sp))
      ; (P.seq (P.text "\n\n") (fun (vn, (ct, _)) -> 
          P.dprintf "%s :: @[%a@]" vn CType.d_ctype ct
         ) (varspec sp |> SM.to_list))
      ; (P.seq (P.text "\n\n") (fun (fn, (cf, _)) -> 
          P.dprintf "%s ::@!  @[%a@]\n\n" fn CFun.d_cfun cf
         ) (funspec sp |> SM.to_list)) ]
      |> List.fold_left P.concat P.nil
  end

  (******************************************************************************)
  (******************************* Expression Maps ******************************)
  (******************************************************************************)
  module ExpKey = struct
    type t      = Cil.exp
    let compare = compare
  end

  module ExpMap = Misc.EMap (ExpKey)

  module ExpMapPrinter = P.MakeMapPrinter(ExpMap)

  type ctemap = CType.t ExpMap.t

  let d_ctemap () (em: ctemap): P.doc =
    ExpMapPrinter.d_map "\n" Cil.d_exp CType.d_ctype () em
end

module I          = Make (IndexTypes)

type ctype  = I.CType.t
type cfun   = I.CFun.t
type store  = I.Store.t
type cspec  = I.Spec.t
type ctemap = I.ctemap

let void_ctype   = Int (0, N.top)
let ptr_ctype    = Ref (S.none, N.top)
let scalar_ctype = Int (0, N.top)

let vtype_to_ctype v = if Cil.isArithmeticType v
                         then scalar_ctype else ptr_ctype

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
  try RCt.Store.Data.add sto l rd with Not_found -> 
    assertf "refstore_set"

let refstore_get sto l =
  try RCt.Store.Data.find sto l with Not_found ->
    (Errormsg.error "Cannot find location %a in store\n" Sloc.d_sloc l;   
     asserti false "refstore_get"; assert false)

let refldesc_subs rd f =
  RCt.LDesc.mapn (fun i pl fld -> RCt.Field.map_type (f i pl) fld) rd

(*******************************************************************)
(******************** Operations on Refined Stores *****************)
(*******************************************************************)

let refdesc_find i rd = 
  match RCt.LDesc.find i rd with
  | [(i', rfld)] -> (RCt.Field.type_of rfld, Index.is_periodic i')
  | _            -> assertf "refdesc_find"

let addr_of_refctype loc = function
  | Ref (cl, (i,_)) when not (Sloc.is_abstract cl) ->
      (cl, i)
  | cr ->
      let s = cr  |> d_refctype () |> P.sprint ~width:80 in
      let l = loc |> Cil.d_loc () |> P.sprint ~width:80 in
      let _ = asserti false "addr_of_refctype: bad arg %s at %s \n" s l in
      assert false

let ac_refstore_read loc sto cr = 
  let (l, ix) = addr_of_refctype loc cr in 
     l
  |> RCt.Store.Data.find sto 
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
  let ld = RCt.Store.Data.find sto cl in
  let ld = RCt.LDesc.remove ix ld in
  let ld = RCt.LDesc.add ix (RCt.Field.create Nonfinal dummy_fieldinfo rct') ld in
  RCt.Store.Data.add sto cl ld

(* API *)
let ctype_of_refctype = function
  | Int (x, (y, _))  -> Int (x, y) 
  | Ref (x, (y, _))  -> Ref (x, y)

(* API *)
let cfun_of_refcfun   = I.CFun.map ctype_of_refctype 
let cspec_of_refspec  = I.Spec.map (RCt.CType.map (fun (i,_) -> i))
let store_of_refstore = I.Store.map ctype_of_refctype
let args_of_refcfun   = fun ft -> ft.args
let ret_of_refcfun    = fun ft -> ft.ret
let stores_of_refcfun = fun ft -> (ft.sto_in, ft.sto_out)

let reft_of_refctype = function
  | Int (_,(_,r)) 
  | Ref (_,(_,r)) -> r

(**********************************************************************)


