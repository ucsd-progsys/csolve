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
module C   = Cil
module CM  = CilMisc
module SM  = M.StringMap
module SLM = S.SlocMap

module SLMPrinter = P.MakeMapPrinter(SLM)

open M.Ops

(******************************************************************************)
(*********************************** Indices **********************************)
(******************************************************************************)

module Index = struct
  type class_bound = int option (* None = unbounded *)

  let scale_bound x b =
    M.map_opt (( * ) x) b

  let bound_offset x b =
    M.map_opt (( + ) x) b

  let bound_plus b1 b2 = match b1, b2 with
    | Some n, Some m -> Some (m + n)
    | _              -> None

  let lower_bound_le b1 b2 = match b1, b2 with
    | Some n, Some m -> n <= m
    | Some _, None   -> false
    | _              -> true

  let upper_bound_le b1 b2 = match b1, b2 with
    | Some n, Some m -> n <= m
    | None, Some _   -> false
    | _              -> true

  let bound_min le b1 b2 =
    if le b1 b2 then b1 else b2

  let bound_max le b1 b2 =
    if le b1 b2 then b2 else b1

  let lower_bound_min = bound_min lower_bound_le
  let lower_bound_max = bound_max lower_bound_le
  let upper_bound_min = bound_min upper_bound_le
  let upper_bound_max = bound_max upper_bound_le

  (* Describes the integers lb <= n <= ub s.t. n = c (mod m) *)
  type bounded_congruence_class = {
    lb : class_bound; (* Lower bound; lb mod m = c *)
    ub : class_bound; (* Upper bound; ub mod m = c; lb < ub *)
    c  : int;         (* Remainder; 0 <= c < m *)
    m  : int;         (* Modulus; 0 < m <= ub - lb *)
  }

  type t =
    | IBot                                 (* Empty *)
    | IInt    of int                       (* Single integer *)
    | ICClass of bounded_congruence_class  (* Subset of congruence class *)

  let top = ICClass {lb = None; ub = None; c = 0; m = 1}

  let nonneg = ICClass {lb = Some 0; ub = None; c = 0; m = 1}

  let d_index () = function
    | IBot                                         -> P.dprintf "_|_"
    | IInt n                                       -> P.num n
    | ICClass {lb = None; ub = None; c = c; m = m} -> P.dprintf "%d{%d}" c m
    | ICClass {lb = Some n; ub = None; m = m}      -> P.dprintf "%d[%d]" n m
    | ICClass {lb = Some l; ub = Some u; m = m}    -> P.dprintf "%d[%d < %d]" l m (u + m)
    | _                                            -> assert false

  let repr_suffix = function
    | IBot                                     -> "B"
    | IInt n                                   -> string_of_int n
    | ICClass {lb = lb; ub = ub; m = m; c = c} ->
      let lbs = match lb with Some n -> string_of_int n | None -> "-" in
      let ubs = match ub with Some n -> string_of_int n | None -> "+" in
        lbs ^ "#c" ^ string_of_int c ^ "#m" ^ string_of_int m ^ "#" ^ ubs

  let repr_prefix = "Ix#"

  let repr i =
    repr_prefix ^ repr_suffix i

  let compare i1 i2 = compare i1 i2

  let of_int i =
    (* pmr: loosen this? *)
    if i >= 0 then IInt i else top

  let mk_sequence start period lbound ubound = match lbound, ubound with
    | Some m, Some n when m = n -> IInt n
    | _                         -> ICClass {lb = lbound; ub = ubound; m = period; c = start mod period}

  let mk_singleton n =
    IInt n

  let mk_geq n =
    ICClass {lb = Some n; ub = None; m = 1; c = 0}

  let mk_leq n =
    ICClass {lb = None; ub = Some n; m = 1; c = 0}

  let mk_eq_mod c m =
    ICClass {lb = None; ub = None; m = m; c = c}

  let is_unbounded = function
    | ICClass {lb = None} | ICClass {ub = None} -> true
    | _                                         -> false

  let period = function
    | ICClass {m = m} -> Some m
    | IInt _ | IBot   -> None

  let is_periodic i =
    period i != None

  let is_subindex i1 i2 = match i1, i2 with
    | IBot, _                                          -> true
    | _, IBot                                          -> false
    | IInt n, IInt m                                   -> n = m
    | ICClass _, IInt _                                -> false
    | IInt n, ICClass {lb = lb; ub = ub; c = c; m = m} ->
      lower_bound_le lb (Some n) && upper_bound_le (Some n) ub && ((n mod m) = c)
    | ICClass {lb = lb1; ub = ub1; c = c1; m = m1},
      ICClass {lb = lb2; ub = ub2; c = c2; m = m2} ->
      lower_bound_le lb2 lb1 && upper_bound_le ub1 ub2 && (m1 mod m2) = 0 && (c1 mod m2) = c2

  let lub i1 i2 =
    if is_subindex i1 i2 then
      i2
    else if is_subindex i2 i1 then
      i1
    else match i1, i2 with
      | IBot, _ | _, IBot -> assert false
      | IInt m, IInt n    ->
        let d = abs (m - n) in
          ICClass {lb = Some (min m n); ub = Some (max m n); m = d; c = m mod d}
      | IInt n, ICClass {lb = lb; ub = ub; m = m; c = c}
      | ICClass {lb = lb; ub = ub; m = m; c = c}, IInt n ->
        let m = M.gcd m (abs (c - (n mod m))) in
          ICClass {lb = lower_bound_min lb (Some n);
                   ub = upper_bound_max ub (Some n);
                   m  = m;
                   c  = c mod m}
      | ICClass {lb = lb1; ub = ub1; m = m1; c = c1},
        ICClass {lb = lb2; ub = ub2; m = m2; c = c2} ->
        let m = M.gcd m1 (M.gcd m2 (abs (c1 - c2))) in
          ICClass {lb = lower_bound_min lb1 lb2;
                   ub = upper_bound_max ub1 ub2;
                   m  = m;
                   c  = c1 mod m}

  (* pmr: There is surely a closed form for this, to be sought
     later. *)
  let rec search_congruent c1 m1 c2 m2 n =
    if n < 0 then None else
      if (n mod m1) = c1 && (n mod m2) = c2 then
        Some n
      else
        search_congruent c1 m1 c2 m2 (n - 1)

  let glb i1 i2 =
    if is_subindex i1 i2 then
      i1
    else if is_subindex i2 i1 then
      i2
    else match i1, i2 with
      | IBot, _ | _, IBot         -> assert false
      | IInt m, IInt n when m = n -> IInt m
      | IInt _, IInt _            -> IBot
      | IInt n, ICClass {lb = lb; ub = ub; m = m; c = c}
      | ICClass {lb = lb; ub = ub; m = m; c = c}, IInt n ->
        if lower_bound_le lb (Some n) &&
           upper_bound_le (Some n) ub &&
           (n mod m) = c then
          IInt n
        else IBot
      | ICClass {lb = lb1; ub = ub1; m = m1; c = c1},
        ICClass {lb = lb2; ub = ub2; m = m2; c = c2} ->
        let m = M.lcm m1 m2 in
          match search_congruent c1 m1 c2 m2 (m - 1) with
            | None   -> IBot
            | Some c ->
              let lb = lower_bound_max lb1 lb2 |> M.map_opt (fun l -> m * ((l + m - c - 1) / m) + c) in
              let ub = upper_bound_min ub1 ub2 |> M.map_opt (fun l -> m * ((l - c) / m) + c) in
                match lb, ub with
                  | Some l, Some u when u = l -> IInt u
                  | Some l, Some u when u < l -> IBot
                  | _                         -> ICClass {lb = lb; ub = ub; m = m; c = c}

  let overlaps i1 i2 =
    glb i1 i2 <> IBot

  let widen i1 i2 =
    if is_subindex i1 i2 then
      i2
    else if is_subindex i2 i1 then
      i1
    else match i1, i2 with
      | IBot, _ | _, IBot -> assert false
      | IInt n, IInt m    ->
        let d = abs (n - m) in
          ICClass {lb = Some (min n m);
                   ub = None;
                   c  = n mod d;
                   m  = d}
      | IInt n, ICClass {lb = lb; ub = ub; c = c; m = m}
      | ICClass {lb = lb; ub = ub; c = c; m = m}, IInt n ->
        ICClass {lb = lower_bound_min (Some n) lb;
                 ub = upper_bound_max (Some n) ub;
                 c  = 0;
                 m  = M.gcd m (M.gcd c n)}
      | ICClass {lb = lb1; ub = ub1; c = c1; m = m1},
        ICClass {lb = lb2; ub = ub2; c = c2; m = m2} ->
        let m = M.gcd m1 (M.gcd m2 (abs (c1 - c2))) in
          ICClass {lb = if lb1 = lb2 then lb1 else None;
                   ub = if ub1 = ub2 then ub1 else None;
                   c  = c1 mod m;
                   m  = m}

  let offset n = function
    | IBot                                     -> IBot
    | IInt m                                   -> IInt (n + m)
    | ICClass {lb = lb; ub = ub; c = c; m = m} ->
      ICClass {lb = bound_offset n lb;
               ub = bound_offset n ub;
               c  = (c + n) mod m;
               m  = m}

  let plus i1 i2 = match i1, i2 with
    | IBot, _ | _, IBot     -> IBot
    | IInt n, i | i, IInt n -> offset n i
    | ICClass {lb = lb1; ub = ub1; c = c1; m = m1},
      ICClass {lb = lb2; ub = ub2; c = c2; m = m2} ->
      ICClass {lb = bound_plus lb1 lb2;
               ub = bound_plus ub1 ub2;
               c  = 0;
               m  = M.gcd m1 (M.gcd m2 (M.gcd c1 c2))}

  let minus i1 i2 = match i1, i2 with
    | IBot, _ | _, IBot                                                -> IBot
    | IInt n, IInt m                                                   -> IInt (n - m)
    | ICClass {lb = Some l; ub = ub; c = c; m = m}, IInt n when l >= n ->
      ICClass {lb = Some (l - n);
               ub = bound_offset (-n) ub;
               c = (c - n) mod m;
               m = m}
    | _ -> top

  let scale x = function
    | IBot   -> IBot
    | IInt n -> IInt (n * x)
    | ICClass {lb = lb; ub = ub; c = c; m = m} ->
      ICClass {lb = scale_bound x lb;
               ub = scale_bound x ub;
               c = c;
               m = x * m}

  let mult i1 i2 = match i1, i2 with
    | IBot, _ | _, IBot     -> IBot
    | IInt n, i | i, IInt n -> scale n i
    | _                     -> top

  let constop op i1 i2 = match i1, i2 with
    | IBot, _ | _, IBot -> IBot
    | IInt n, IInt m    -> IInt (op n m)
    | _                 -> top

  let div =
    constop (/)

  let unsign i = match i with
    | IBot                              -> IBot
    | IInt n when n >= 0                -> i
    | ICClass {lb = Some n} when n >= 0 -> i
    | _                                 -> nonneg
end

module IndexSet        = Set.Make (Index)
module IndexSetPrinter = P.MakeSetPrinter (IndexSet)

let d_indexset () is =
  IndexSetPrinter.d_set ", " Index.d_index () is

module N = Index

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
(***************************** Parameterized Types ****************************)
(******************************************************************************)

type finality =
  | Final
  | Nonfinal

type 'a prectype =
  | Int  of int * 'a        (* fixed-width integer *)
  | Ref  of Sloc.t * 'a     (* reference *)

and 'a prefield = 'a prectype * finality

and 'a preldesc = (Index.t * (C.location * 'a prefield)) list

and 'a prestore = 'a preldesc Sloc.SlocMap.t * 'a precfun Sloc.SlocMap.t

and 'a precfun =
    { args        : (string * 'a prectype) list;  (* arguments *)
      ret         : 'a prectype;                  (* return *)
      globlocs    : S.t list;                     (* unquantified locations *)
      sto_in      : 'a prestore;                  (* in store *)
      sto_out     : 'a prestore;                  (* out store *)
    }

type 'a prespec = ('a precfun * bool) Misc.StringMap.t 
                * ('a prectype * bool) Misc.StringMap.t 
                * 'a prestore

module SIGS (R : CTYPE_REFINEMENT) = struct
  type ctype = R.t prectype
  type field = R.t prefield
  type ldesc = R.t preldesc
  type store = R.t prestore
  type cfun  = R.t precfun
  type spec  = R.t prespec

  module type CTYPE = sig
    type t = ctype

    exception NoLUB of t * t

    val refinement  : t -> R.t
    val map         : ('a -> 'b) -> 'a prectype -> 'b prectype
    val d_ctype     : unit -> t -> Pretty.doc
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
    type t = field

    val get_finality : t -> finality
    val set_finality : finality -> t -> t
    val is_final     : t -> bool
    val type_of      : t -> ctype
    val sloc_of      : t -> Sloc.t option
    val create       : finality -> ctype -> t
    val subs         : Sloc.Subst.t -> t -> t
    val map_type     : ('a prectype -> 'b prectype) -> 'a prefield -> 'b prefield
      
    val d_field      : unit -> t -> Pretty.doc
  end

  module type LDESC = sig
    type t = ldesc

    exception TypeDoesntFit of Index.t * ctype * t

    val empty         : t
    val eq            : t -> t -> bool
    val add           : C.location -> Index.t -> field -> t -> t
    val create        : C.location -> (Index.t * field) list -> t
    val remove        : Index.t -> t -> t
    val mem           : Index.t -> t -> bool
    val referenced_slocs : t -> Sloc.t list
    val find          : Index.t -> t -> (Index.t * field) list
    val foldn         : (int -> 'a -> Index.t -> field -> 'a) -> 'a -> t -> 'a
    val fold          : ('a -> Index.t -> field -> 'a) -> 'a -> t -> 'a
    val subs          : Sloc.Subst.t -> t -> t
    val map           : ('a prefield -> 'b prefield) -> 'a preldesc -> 'b preldesc
    val mapn          : (int -> Index.t -> 'a prefield -> 'b prefield) -> 'a preldesc -> 'b preldesc
    val iter          : (Index.t -> field -> unit) -> t -> unit
    val indices       : t -> Index.t list
    val d_ldesc       : unit -> t -> Pretty.doc
  end

  module type STORE = sig
    type t = store

    val empty        : t
    val domain       : t -> Sloc.t list
    val mem          : t -> Sloc.t -> bool
    val closed       : t -> bool
    val reachable    : t -> Sloc.t -> Sloc.t list
    val map          : ('a prectype -> 'b prectype) -> 'a prestore -> 'b prestore
    val partition    : (Sloc.t -> bool) -> t -> t * t
    val remove       : t -> Sloc.t -> t
    val upd          : t -> t -> t
  (** [upd st1 st2] returns the store obtained by adding the locations from st2 to st1,
      overwriting the common locations of st1 and st2 with the blocks appearing in st2 *)
    val subs         : Sloc.Subst.t -> t -> t
    val ctype_closed : ctype -> t -> bool
    val indices      : t -> Index.t list

    val d_store_addrs: unit -> t -> Pretty.doc
    val d_store      : unit -> t -> Pretty.doc

    module Data: sig
      val add           : t -> Sloc.t -> ldesc -> t
      val mem           : t -> Sloc.t -> bool
      val find          : t -> Sloc.t -> ldesc
      val find_or_empty : t -> Sloc.t -> ldesc
      val map           : ('a prectype -> 'a prectype) -> 'a prestore -> 'a prestore
      val fold_fields   : ('a -> Sloc.t -> Index.t -> field -> 'a) -> 'a -> t -> 'a
      val fold_locs     : (Sloc.t -> ldesc -> 'a -> 'a) -> 'a -> t -> 'a
    end

    module Function: sig
      val add       : 'a prestore -> Sloc.t -> 'a precfun -> 'a prestore
      val mem       : 'a prestore -> Sloc.t -> bool
      val find      : 'a prestore -> Sloc.t -> 'a precfun
      val fold_locs : (Sloc.t -> 'b precfun -> 'a -> 'a) -> 'a -> 'b prestore -> 'a
    end
  end

  module type CFUN = sig
    type t = cfun

    val d_cfun          : unit -> t -> Pretty.doc
    val map             : ('a prectype -> 'b prectype) -> 'a precfun -> 'b precfun
    val well_formed     : store -> t -> bool
    val normalize_names : t -> t -> (store -> Sloc.Subst.t -> (string * string) list -> ctype -> ctype) -> t * t
    val same_shape      : t -> t -> bool
    val quantified_locs : t -> Sloc.t list
    val make            : (string * ctype) list -> ctype -> S.t list -> store -> store -> t
    val subs            : t -> Sloc.Subst.t -> t
    val indices         : t -> Index.t list
  end

  module type SPEC = sig
    type t      = spec

    val empty   : t

    val map : ('a prectype -> 'b prectype) -> 'a prespec -> 'b prespec
    val add_fun : bool -> string -> cfun * bool -> t -> t
    val add_var : bool -> string -> ctype * bool -> t -> t
    val add_data_loc : Sloc.t -> ldesc -> t -> t
    val add_fun_loc  : Sloc.t -> cfun -> t -> t
    val upd_store    : t -> store -> t
    val mem_fun : string -> t -> bool
    val mem_var : string -> t -> bool
    val get_fun : string -> t -> cfun * bool
    val get_var : string -> t -> ctype * bool
    val store   : t -> store
    val funspec : t -> (R.t precfun * bool) Misc.StringMap.t
    val varspec : t -> (R.t prectype * bool) Misc.StringMap.t

    val make    : (R.t precfun * bool) Misc.StringMap.t ->
                  (R.t prectype * bool) Misc.StringMap.t ->
                  store ->
                  t
    val add     : t -> t -> t
  end
end

module type S = sig
  module R : CTYPE_REFINEMENT

  module CType : SIGS (R).CTYPE
  module Field : SIGS (R).FIELD
  module LDesc : SIGS (R).LDESC
  module Store : SIGS (R).STORE
  module CFun  : SIGS (R).CFUN
  module Spec  : SIGS (R).SPEC

  module ExpKey: sig
    type t = Cil.exp
    val compare: t -> t -> int
  end

  module ExpMap: Map.S with type key = ExpKey.t

  module ExpMapPrinter: sig
    val d_map:
      ?dmaplet:(Pretty.doc -> Pretty.doc -> Pretty.doc) ->
      string ->
      (unit -> ExpMap.key -> Pretty.doc) ->
      (unit -> 'a -> Pretty.doc) -> unit -> 'a ExpMap.t -> Pretty.doc
  end

  type ctemap = CType.t ExpMap.t

  val d_ctemap: unit -> ctemap -> Pretty.doc
end

module Make (R: CTYPE_REFINEMENT): S with module R = R = struct
  module R   = R
  module SIG = SIGS (R)

  (***********************************************************************)
  (***************************** Types ***********************************)
  (***********************************************************************)

  module rec CType: SIG.CTYPE = struct
    type t = R.t prectype

    let refinement = function
      | Int (_, r) | Ref (_, r) -> r

    let map f = function
      | Int (i, x) -> Int (i, f x)
      | Ref (l, x) -> Ref (l, f x)

    let d_ctype () = function
      | Int (n, i) -> P.dprintf "int(%d, %a)" n R.d_refinement i
      | Ref (s, i) -> P.dprintf "ref(%a, %a)" S.d_sloc s R.d_refinement i

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
        | Int (n1, r1), Int (n2, r2) when n1 = n2    -> R.is_subref r1 r2
        | Ref (s1, r1), Ref (s2, r2) when S.eq s1 s2 -> R.is_subref r1 r2
        | _                                          -> false

    let of_const c =
      let r = R.of_const c in
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
    type t = R.t prefield

    let get_finality = snd

    let type_of = fst

    let create fnl t =
      (t, fnl)

    let set_finality fnl fld =
      fld |> type_of |> create fnl

    let is_final fld =
      get_finality fld = Final

    let sloc_of fld = fld |> type_of |> CType.sloc

    let map_type f fld =
      fld |> type_of |> f |> create (get_finality fld)

    let subs sub =
      map_type (CType.subs sub)

    let d_field () fld =
      let pct = type_of fld in
        if is_final fld then P.dprintf "final %a" CType.d_ctype pct else CType.d_ctype () pct
  end

  and LDesc: SIG.LDESC = struct
    type t = R.t preldesc

    exception TypeDoesntFit of Index.t * CType.t * t

    let empty = []

    let eq ld1 ld2 =
      Misc.same_length ld1 ld2 &&
        List.for_all2 (fun (i1, (_, f1)) (i2, (_, f2)) -> i1 = i2 && f1 = f2) ld1 ld2

    let fits i fld flds =
      let t = Field.type_of fld in
      let w = CType.width t in
        M.get_option w (N.period i) >= w &&
          not (List.exists (fun (i2, (_, fld2)) -> CType.collide i t i2 (Field.type_of fld2)) flds)

    let rec insert ((i, _) as fld) = function
      | []                      -> [fld]
      | (i2, _) as fld2 :: flds -> if i < i2 then fld :: fld2 :: flds else fld2 :: insert fld flds

    let add loc i fld flds =
      if fits i fld flds then insert (i, (loc, fld)) flds else
        raise (TypeDoesntFit (i, Field.type_of fld, flds))

    let remove i flds =
      List.filter (fun (i2, _) -> not (i = i2)) flds

    let create loc flds =
      List.fold_right (add loc |> M.uncurry) flds empty

    let mem i flds =
      List.exists (fun (i2, _) -> N.is_subindex i i2) flds

    let find i flds =
      flds |> List.filter (fun (i2, _) -> N.overlaps i i2)
           |> List.map (fun (i, (_, fld)) -> (i, fld))

    let rec foldn_aux f n b = function
      | []                    -> b
      | (i, (_, fld)) :: flds -> foldn_aux f (n + 1) (f n b i fld) flds

    let foldn f b flds =
      foldn_aux f 0 b flds

    let fold f b flds =
      foldn (fun _ b i fld -> f b i fld) b flds

    let mapn f flds =
      M.mapi (fun n (i, (loc, fld)) -> (i, (loc, f n i fld))) flds

    let map f flds =
      mapn (fun _ _ fld -> f fld) flds

    let subs sub =
      map (Field.map_type (CType.subs sub))

    let iter f ld =
      fold (fun _ i fld -> f i fld) () ld

    let referenced_slocs ld =
      fold begin fun rls _ fld -> match Field.sloc_of fld with 
        | None   -> rls
        | Some l -> l :: rls
      end [] ld

    let indices flds =
      List.map fst flds

    let d_ldesc () flds =
      P.dprintf "@[%t@]"
        begin fun () ->
          P.seq
            (P.dprintf ",@!")
            (fun (i, (_, fld)) -> P.dprintf "%a: %a" Index.d_index i Field.d_field fld)
            flds
        end
  end

  and Store: SIG.STORE = struct
    type t = R.t prestore

    let empty = (SLM.empty, SLM.empty)

    let map_data f =
      f |> Field.map_type |> LDesc.map |> SLM.map

    module Data = struct
      let add (ds, fs) l ld =
        let _ = assert (not (SLM.mem l fs)) in
          (SLM.add l ld ds, fs)

      let mem (ds, _) l =
        SLM.mem l ds

      let find (ds, _) l =
        SLM.find l ds

      let find_or_empty sto l =
        try find sto l with Not_found -> LDesc.empty

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
          (ds, SLM.add l cf fs)

      let mem (_, fs) l =
        SLM.mem l fs

      let find (_, fs) l =
        SLM.find l fs

      let fold_locs f b (_, fs) =
        SLM.fold f fs b
    end

    let map f (ds, fs) =
      (map_data f ds, SLM.map (CFun.map f) fs)

    let slm_domain m =
      SLM.fold (fun s _ ss -> s :: ss) m []

    let domain (ds, fs) =
      slm_domain ds ++ slm_domain fs

    let mem (ds, fs) s =
      SLM.mem s ds || SLM.mem s fs

    let subs_slm_dom subs m =
      SLM.fold (fun l d m -> SLM.add (S.Subst.apply subs l) d m) m SLM.empty

    let subs_addrs subs (ds, fs) =
      (subs_slm_dom subs ds, subs_slm_dom subs fs)

    let subs subs (ds, fs) =
      (ds |> map_data (CType.subs subs), fs |> SLM.map (M.flip CFun.subs subs)) |> subs_addrs subs

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
      
    let rec closed ((_, fs) as sto) =
      Data.fold_fields (fun c _ _ fld -> c && ctype_closed (Field.type_of fld) sto) true sto &&
        (* pmr: Not yet right, but we need smarter handling of global stores. *)
        SLM.fold (fun _ cf c -> c && CFun.well_formed empty cf) fs true

    let slm_acc_list f m =
      SLM.fold (fun _ d acc -> f d ++ acc) m []

    let indices (ds, fs) =
      slm_acc_list LDesc.indices ds ++ slm_acc_list CFun.indices fs

    let d_store_addrs () st =
      Pretty.seq (Pretty.text ",") (Sloc.d_sloc ()) (domain st)

    let d_slm d_binding =
      SLMPrinter.docMap ~sep:(P.dprintf ";@!") (fun l d -> P.dprintf "%a |-> %a" S.d_sloc l d_binding d)

    let d_store () (ds, fs) =
      if fs = SLM.empty then
        P.dprintf "[@[%a@]]" (d_slm LDesc.d_ldesc) ds
      else if ds = SLM.empty then
        P.dprintf "[@[%a@]]" (d_slm CFun.d_cfun) fs
      else
        P.dprintf "[@[%a;@!%a@]]" (d_slm LDesc.d_ldesc) ds (d_slm CFun.d_cfun) fs
  end

  (******************************************************************************)
  (******************************* Function Types *******************************)
  (******************************************************************************)
  and CFun: SIG.CFUN = struct
    type t = R.t precfun

    (* API *)
    let make args reto globs sin sout =
      { args     = args;
        ret      = reto;
        globlocs = globs;
        sto_in   = sin;
        sto_out  = sout;
      }

    let map f ft =
      { args     = List.map (Misc.app_snd f) ft.args;
        ret      = f ft.ret;
        globlocs = ft.globlocs;
        sto_in   = Store.map f ft.sto_in;
        sto_out  = Store.map f ft.sto_out;
      }

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
             (apply_sub cf.ret)
             (List.map (S.Subst.apply sub) cf.globlocs)
             (Store.subs sub cf.sto_in)
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

    let fresh_arg_name, _ = M.mk_string_factory "ARG"

    let replace_arg_names anames cf =
      {cf with args = List.map2 (fun an (_, t) -> (an, t)) anames cf.args}

    let normalize_names cf1 cf2 f =
      let ls1, ls2     = M.map_pair ordered_locs (cf1, cf2) in
      let fresh_locs   = List.map (Sloc.to_slocinfo <+> Sloc.fresh_abstract) ls1 in
      let lsub1, lsub2 = M.map_pair (M.flip List.combine fresh_locs) (ls1, ls2) in
      let fresh_args   = List.map (fun _ -> fresh_arg_name ()) cf1.args in
      let asub1, asub2 = M.map_pair (List.map fst <+> M.flip List.combine fresh_args) (cf1.args, cf2.args) in
      let cf1, cf2     = M.map_pair (replace_arg_names fresh_args) (cf1, cf2) in
        (capturing_subs cf1 lsub1 |> map (f cf1.sto_out lsub1 asub1),
         capturing_subs cf2 lsub2 |> map (f cf2.sto_out lsub2 asub2))

    let rec same_shape cf1 cf2 =
      M.same_length (quantified_locs cf1) (quantified_locs cf2) && M.same_length cf1.args cf2.args &&
        let cf1, cf2 = normalize_names cf1 cf2 (fun _ _ _ ct -> ct) in
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
      let whole_instore  = Store.upd cf.sto_in globstore in (* pmr: shouldn't this be the other way around? *)
      let whole_outstore = Store.upd cf.sto_out globstore in
             Store.closed whole_instore
          && Store.closed whole_outstore
          && List.for_all (Store.mem globstore) cf.globlocs
          && not (cf.sto_out |> Store.domain |> List.exists (M.flip List.mem cf.globlocs))
          && List.for_all (fun (_, ct) -> Store.ctype_closed ct whole_instore) cf.args
          && match cf.ret with  (* we can return refs to uninitialized data *)
             | Ref (l, _) -> Store.mem whole_outstore l
             | _          -> true

    let indices cf =
      Store.indices cf.sto_in ++ Store.indices cf.sto_out
  end

  (******************************************************************************)
  (************************************ Specs ***********************************)
  (******************************************************************************)
  and Spec: SIG.SPEC = struct
    type t = R.t prespec

    let empty = (SM.empty, SM.empty, Store.empty)

    let map f (funspec, varspec, storespec) =
      (SM.map (f |> CFun.map |> M.app_fst) funspec,
       SM.map (f |> M.app_fst) varspec,
       Store.map f storespec)

    let add_fun b fn sp (funspec, varspec, storespec) =
      (Misc.sm_protected_add b fn sp funspec, varspec, storespec)

    let add_var b vn sp (funspec, varspec, storespec) =
      (funspec, Misc.sm_protected_add b vn sp varspec, storespec)

    let add_data_loc l ld (funspec, varspec, storespec) =
      (funspec, varspec, Store.Data.add storespec l ld)

    let add_fun_loc l cf (funspec, varspec, storespec) =
      (funspec, varspec, Store.Function.add storespec l cf)

    let upd_store (funspec, varspec, storespec) sto =
      (funspec, varspec, Store.upd storespec sto)

    let mem_fun fn (funspec, _, _) =
      SM.mem fn funspec

    let mem_var vn (_, varspec, _) =
      SM.mem vn varspec

    let get_fun fn (funspec, _, _) =
      try SM.find fn funspec with Not_found -> 
        E.error "Cannot find %s in fun spec" fn |> halt

    let get_var vn (_, varspec, _) =
      try SM.find vn varspec with Not_found -> 
        E.error "Cannot find %s in var spec" vn |> halt

    let store (_, _, storespec) =
      storespec
      
    let funspec = fst3
    let varspec = snd3
    let make    = fun x y z -> (x, y, z)

    let add (funspec, varspec, storespec) spec =  
       spec
       |> SM.fold (fun fn sp spec -> add_fun false fn sp spec) funspec
       |> SM.fold (fun vn sp spec -> add_var false vn sp spec) varspec
       |> (fun (x, y, z) -> (x, y, Store.upd z storespec))
  end

  (******************************************************************************)
  (******************************* Expression Maps ******************************)
  (******************************************************************************)
  module ExpKey = struct
    type t      = Cil.exp
    let compare = compare
  end

  module ExpMap = Map.Make (ExpKey)

  module ExpMapPrinter = P.MakeMapPrinter(ExpMap)

  type ctemap = CType.t ExpMap.t

  let d_ctemap () (em: ctemap): Pretty.doc =
    ExpMapPrinter.d_map "\n" Cil.d_exp CType.d_ctype () em
end

module I = Make (IndexRefinement)

type ctype  = I.CType.t
type cfun   = I.CFun.t
type store  = I.Store.t
type cspec  = I.Spec.t
type ctemap = I.ctemap

let void_ctype   = Int (0, N.top)
(* let ptr_ctype    = Ref (S.fresh_abstract (), N.top) *)
let scalar_ctype = Int (0, N.top)

let d_ctype        = I.CType.d_ctype
let index_of_ctype = I.CType.refinement

(******************************************************************************)
(************************* Refctypes and Friends ******************************)
(******************************************************************************)

module FC = FixConstraint

let reft_of_top = 
  let so = Ast.Sort.t_obj in
  let vv = Ast.Symbol.value_variable so in
  FC.make_reft vv so []

(*******************************************************************)
(********************* Refined Types and Stores ********************)
(*******************************************************************)

let d_index_reft () (i,r) = 
  let di = Index.d_index () i in
  let dc = Pretty.text " , " in
  let dr = Misc.fsprintf (FC.print_reft None) r |> Pretty.text in
  Pretty.concat (Pretty.concat di dc) dr

module Reft = struct
  type t = Index.t * FC.reft
  let d_refinement = d_index_reft
  let is_subref    = fun ir1 ir2 -> assert false
  let of_const     = fun c -> assert false
  let top          = Index.top, reft_of_top 
end

module RefCTypes   = Make (Reft)
module RCt         = RefCTypes

type refctype      = RCt.CType.t
type refcfun       = RCt.CFun.t
type reffield      = RCt.Field.t
type refldesc      = RCt.LDesc.t
type refstore      = RCt.Store.t
type refspec       = RCt.Spec.t

let d_refstore     = RCt.Store.d_store
let d_refctype     = RCt.CType.d_ctype
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
      let s = cr  |> d_refctype () |> Pretty.sprint ~width:80 in
      let l = loc |> Cil.d_loc () |> Pretty.sprint ~width:80 in
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
  let ld = RCt.LDesc.add loc ix (RCt.Field.create Nonfinal rct') ld in
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
let mk_refcfun args globs ist ret ost =
  { args     = args;
    ret      = ret;
    globlocs = globs;
    sto_in   = ist;
    sto_out  = ost; }

let reft_of_refctype = function
  | Int (_,(_,r)) 
  | Ref (_,(_,r)) -> r
