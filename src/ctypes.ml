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
  val lub          : t -> t -> t option
  val is_subref    : t -> t -> bool
  val of_const     : C.constant -> t
  val top          : t
  val d_refinement : unit -> t -> P.doc
end

module IndexRefinement = struct
  type t = Index.t

  let top          = Index.top
  let lub          = fun i1 i2 -> Some (N.widen i1 i2)
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

type 'a prectype =
  | Int of int * 'a     (* fixed-width integer *)
  | Ref of Sloc.t * 'a  (* reference *)
  | Top of 'a           (* "other", hack for function pointers *)

type finality =
  | Final
  | Nonfinal

type 'a prefield = ('a * finality) prectype

type 'a preldesc = (Index.t * (C.location * 'a prefield)) list

type 'a prestore = ('a preldesc) Sloc.SlocMap.t

type 'a precfun =
    { qlocs       : Sloc.t list;                  (* generalized slocs *)
      args        : (string * 'a prectype) list;  (* arguments *)
      ret         : 'a prectype;                  (* return *)
      sto_in      : 'a prestore;                  (* in store *)
      sto_out     : 'a prestore;                  (* out store *)
    }

type 'a prespec = ('a precfun * bool) Misc.StringMap.t 
                * ('a prectype * bool) Misc.StringMap.t 
                * 'a prestore

module type S = sig
  module R : CTYPE_REFINEMENT

  module CType:
  sig
    type t = R.t prectype

    exception NoLUB of t * t

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
    val is_ref      : t -> bool
    val refinements_of_t : t -> R.t list
    val top         : t
  end

  module Field:
  sig
    type t = R.t prefield

    val get_finality : t -> finality
    val set_finality : finality -> t -> t
    val is_final     : t -> bool
    val type_of      : t -> CType.t
    val create       : finality -> CType.t -> t
    val map_type     : ('a prectype -> 'b prectype) -> 'a prefield -> 'b prefield

    val d_field      : unit -> t -> Pretty.doc
  end

  module LDesc:
  sig
    type t = R.t preldesc

    exception TypeDoesntFit of Index.t * CType.t * t

    val empty         : t
    val add           : C.location -> Index.t -> Field.t -> t -> t
    val create        : C.location -> (Index.t * Field.t) list -> t
    val remove        : Index.t -> t -> t
    val mem           : Index.t -> t -> bool
    val find          : Index.t -> t -> (Index.t * Field.t) list
    val foldn         : (int -> 'a -> Index.t -> Field.t -> 'a) -> 'a -> t -> 'a
    val fold          : ('a -> Index.t -> Field.t -> 'a) -> 'a -> t -> 'a
    val map           : ('a prefield -> 'b prefield) -> 'a preldesc -> 'b preldesc
    val mapn          : (int -> Index.t -> 'a prefield -> 'b prefield) -> 'a preldesc -> 'b preldesc
    val iter          : (Index.t -> Field.t -> unit) -> t -> unit
    val indices       : t -> Index.t list
    val d_ldesc       : unit -> t -> Pretty.doc
  end

  module Store:
  sig
    type t = R.t prestore

    val domain       : t -> Sloc.t list
    val slocs        : t -> Sloc.t list
    val mem          : t -> Sloc.t -> bool
    val map_ct       : ('a prectype -> 'b prectype) -> 'a prestore -> 'b prestore
    val map          : ('a -> 'b) -> 'a prestore -> 'b prestore
    val find         : Sloc.t -> t -> LDesc.t
    val find_index   : Sloc.t -> Index.t -> t -> Field.t list
    val fold         : ('a -> Sloc.t -> Index.t -> Field.t -> 'a) -> 'a -> t -> 'a
    val close_under  : t -> Sloc.t list -> t
    val closed       : t -> bool
    val partition    : (Sloc.t -> LDesc.t -> bool) -> t -> t * t
    val upd          : t -> t -> t
      (** [upd st1 st2] returns the store obtained by adding the locations from st2 to st1,
          overwriting the common locations of st1 and st2 with the blocks appearing in st2 *)
    val subs         : Sloc.Subst.t -> t -> t
    val ctype_closed : CType.t -> t -> bool
    val indices_of_t : t -> Index.t list
    val d_store_addrs: unit -> t -> Pretty.doc
    val d_store      : unit -> t -> Pretty.doc

    (* val prestore_split  : 'a prestore -> 'a prestore * 'a prestore
    (** [prestore_split sto] returns (asto, csto) s.t. 
       (1) sto = asto + csto
       (2) locs(asto) \in abslocs 
       (3) locs(csto) \in conlocs *)
       let prestore_split (ps: 'a prestore): 'a prestore * 'a prestore =
       prestore_partition (fun l _ -> S.is_abstract l) ps
    *)
  end

  module CFun:
  sig
    type t = R.t precfun

    val d_cfun             : unit -> t -> Pretty.doc
    val map                : ('a prectype -> 'b prectype) -> 'a precfun -> 'b precfun
    val well_formed        : Store.t -> t -> bool
    val prune_unused_qlocs : t -> t
    val instantiate        : t -> t * (Sloc.t * Sloc.t) list
    val slocs              : t -> Sloc.t list
    val make               : Sloc.t list -> (string * CType.t) list -> CType.t -> Store.t -> Store.t -> t
    val subs               : Sloc.Subst.t -> t -> t
  end

  module ExpKey:
  sig
    type t = Cil.exp
    val compare: t -> t -> int
  end

  module ExpMap: Map.S with type key = ExpKey.t

  module ExpMapPrinter:
  sig
    val d_map:
      ?dmaplet:(Pretty.doc -> Pretty.doc -> Pretty.doc) ->
      string ->
      (unit -> ExpMap.key -> Pretty.doc) ->
      (unit -> 'a -> Pretty.doc) -> unit -> 'a ExpMap.t -> Pretty.doc
  end

  type ctemap = CType.t ExpMap.t

  val d_ctemap: unit -> ctemap -> Pretty.doc

  module Spec:
  sig
    type t      = R.t prespec

    val empty   : t

    val map     : ('a -> 'b) -> 'a prespec -> 'b prespec
    val add_fun : bool -> string -> CFun.t * bool -> t -> t
    val add_var : bool -> string -> CType.t * bool -> t -> t
    val add_loc : Sloc.t -> LDesc.t -> t -> t
    val mem_fun : string -> t -> bool
    val mem_var : string -> t -> bool
    val get_fun : string -> t -> CFun.t * bool
    val get_var : string -> t -> CType.t * bool
    val store   : t -> Store.t
    val funspec : t -> (R.t precfun * bool) Misc.StringMap.t
    val varspec : t -> (R.t prectype * bool) Misc.StringMap.t

    val make    : (R.t precfun * bool) Misc.StringMap.t -> 
                  (R.t prectype * bool) Misc.StringMap.t -> 
                  Store.t -> 
                  t
    val add     : t -> t -> t
  end
end

module Make (R: CTYPE_REFINEMENT) = struct
  module R = R (* TODO: huh? *)

  (***********************************************************************)
  (***************************** Types ***********************************)
  (***********************************************************************)

  module CType = struct
    type t = R.t prectype

    let map f = function
      | Int (i, x) -> Int (i, f x)
      | Ref (l, x) -> Ref (l, f x)
      | Top (x)    -> Top (f x)

    let convert = map

    let d_ctype () = function
      | Int (n, i) -> P.dprintf "int(%d, %a)" n R.d_refinement i
      | Ref (s, i) -> P.dprintf "ref(%a, %a)" S.d_sloc s R.d_refinement i
      | Top (i)    -> P.dprintf "top(%a)" R.d_refinement i

    let width = function
      | Int (n, _) -> n
      | Ref (_)    -> CM.int_width
      | Top (_)    -> assertf "width of top is undefined!" (* CM.int_width *) 
    
    let sloc = function
      | Ref (s, _) -> Some s
      | _          -> None

    let subs subs = function
      | Ref (s, i) -> Ref (S.Subst.apply subs s, i)
      | pct        -> pct

    exception NoLUB of t * t

    let lub_refs t1 t2 r1 r2 =
      match R.lub r1 r2 with
        | Some r -> r
        | None   -> raise (NoLUB (t1, t2))

    let lub t1 t2 =
      match t1, t2 with
        | Int (n1, r1), Int (n2, r2) when n1 = n2    -> Int (n1, lub_refs t1 t2 r1 r2)
        | Ref (s1, r1), Ref (s2, r2) when S.eq s1 s2 -> Ref (s1, lub_refs t1 t2 r1 r2)
        | _                                          -> raise (NoLUB (t1, t2))

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
          | C.CStr _            -> Ref (S.fresh S.Abstract, r)
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

    let is_ref = function
      | Ref _ -> true
      | _     -> false
     
    let refinements_of_t = function
      | Int (_, x) | Ref (_, x) | Top (x) -> [x]

    let top = Top (R.top)
  end

  (******************************************************************************)
  (*********************************** Stores ***********************************)
  (******************************************************************************)

  module Field = struct
    type t = R.t prefield

    let get_finality = function
      | Int (_, (_, fnl)) | Ref (_, (_, fnl)) -> fnl

    let set_finality fnl = function
      | Int (i, (a, _)) -> Int (i, (a, fnl))
      | Ref (i, (a, _)) -> Ref (i, (a, fnl))

    let is_final fld =
      get_finality fld = Final

    let type_of = function
      | Int (n, (a, _)) -> Int (n, a)
      | Ref (s, (a, _)) -> Ref (s, a)

    let create fnl = function
      | Int (n, a) -> Int (n, (a, fnl))
      | Ref (s, a) -> Ref (s, (a, fnl))

    let map_type f fld =
      fld |> type_of |> f |> create (get_finality fld)

    let d_field () fld =
      let pct = type_of fld in
        if is_final fld then P.dprintf "final %a" CType.d_ctype pct else CType.d_ctype () pct
  end

  module LDesc = struct
    type t = R.t preldesc

    exception TypeDoesntFit of Index.t * CType.t * t

    let empty = []

    let fits i fld flds =
      let t = Field.type_of fld in
      let w = CType.width fld in
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

    let iter f ld =
      fold (fun _ i fld -> f i fld) () ld

    let referenced_slocs ld =
      fold begin fun rss _ pct -> match CType.sloc pct with 
        | None -> rss 
        | Some s -> SS.add s rss
      end SS.empty ld

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

  module Store = struct
    type t = R.t prestore

    let map f =
      f |> CType.map |> Field.map_type |> LDesc.map |> SLM.map

    let map_ct f =
      SLM.map (LDesc.map (Field.map_type f))

    let fold f b ps =
      SLM.fold (fun l ld b -> LDesc.fold (fun b i pct -> f b l i pct) b ld) ps b

    let domain ps =
      SLM.fold (fun s _ ss -> s :: ss) ps []

    let slocs ps =
         ps
      |> domain
      |> M.flip (fold (fun acc _ _ pct -> M.maybe_cons (CType.sloc pct) acc)) ps
      |> M.sort_and_compact

    let mem st s =
      SLM.mem s st

    let find l ps =
      try SLM.find l ps with Not_found -> LDesc.empty

    let find_index l i ps =
      ps |> find l |> LDesc.find i |> List.map snd

    (* pmr: why is this not rename_prestore? *)
    let subs_addrs subs ps =
      SLM.fold (fun s ld ps -> SLM.add (S.Subst.apply subs s) ld ps) ps SLM.empty

    let subs subs ps =
      ps |> map_ct (CType.subs subs)
         |> subs_addrs subs

    let upd ps1 ps2 =
      SLM.fold SLM.add ps2 ps1

    let partition f ps =
      SLM.fold begin fun l ld (ps1, ps2) ->
        if f l ld then
          (SLM.add l ld ps1, ps2)
        else (ps1, SLM.add l ld ps2)
      end ps (SLM.empty, SLM.empty)

    let rec close_slocs ps ss =
      let reqs = SS.fold (fun s rss -> SS.add s (SS.union rss (ps |> find s |> LDesc.referenced_slocs))) ss ss in
        if SS.equal reqs ss then ss else close_slocs ps reqs

    let close_under ps ss =
      let reqs = ss |> List.fold_left (M.flip SS.add) SS.empty |> close_slocs ps in
        SS.fold (fun s cps -> SLM.add s (SLM.find s ps) cps) reqs SLM.empty

    let ctype_closed ct sto =
      match ct with
      | Ref (l, _) -> SLM.mem l sto
      | _          -> true
    
    let closed sto =
      fold (fun closed _ _ ct -> closed && ctype_closed ct sto) true sto

    let rename subs ps =
      let cns = LDesc.map (CType.subs subs) in
        SLM.fold (fun l ld sm -> SLM.add (S.Subst.apply subs l) (cns ld) sm) ps SLM.empty

    let indices_of_t st = 
      SLM.fold (fun _ ld acc -> (LDesc.indices ld) ++ acc) st []

    let d_store_addrs () st =
      Pretty.seq (Pretty.text ",") (Sloc.d_sloc ()) (domain st)

    let d_store () s  =
      P.dprintf "[@[%a@]]"
        (SLMPrinter.docMap ~sep:(P.dprintf ";@!")
           (fun l ld -> P.dprintf "%a |-> %a" S.d_sloc l LDesc.d_ldesc ld)) s
  end

  (******************************************************************************)
  (******************************* Function Types *******************************)
  (******************************************************************************)
  module CFun = struct
    type t = R.t precfun

    (* API *)
    let make qslocs args reto sin sout =
      { qlocs   = qslocs; 
        args    = args;
        ret     = reto;
        sto_in  = sin;
        sto_out = sout;
      }

    let map f ft =
      { qlocs   = ft.qlocs;
        args    = List.map (Misc.app_snd f) ft.args;
        ret     = (* Misc.map_opt *) f ft.ret;
        sto_in  = SLM.map (LDesc.map (Field.map_type f)) ft.sto_in;
        sto_out = SLM.map (LDesc.map (Field.map_type f)) ft.sto_out;
      }

    let d_slocs () slocs = P.seq (P.text ";") (S.d_sloc ()) slocs
    let d_arg () (x, ct) = P.dprintf "%s : %a" x CType.d_ctype ct
    let d_args () args   = P.seq (P.dprintf ",@!") (d_arg ()) args
      
    let d_cfun () ft  = 
      P.dprintf "forall    [%a]\narg       (@[%a@])\nret       %a\nstore_in  %a\nstore_out %a"
        d_slocs ft.qlocs
        d_args ft.args
        CType.d_ctype ft.ret
        Store.d_store ft.sto_in
        Store.d_store ft.sto_out
        
    let prune_unused_qlocs ({qlocs = ls; sto_out = sout} as pcf) =
      {pcf with qlocs = List.filter (fun l -> SLM.mem l sout) ls}

    let instantiate {qlocs = ls; args = acts; ret = rcts; sto_in = sin; sto_out = sout} =
      let subs       = List.map (fun l -> (l, S.fresh S.Abstract)) ls in
      let rename_pct = CType.subs subs in
      let rename_ps  = Store.rename subs in
        ({qlocs   = [];
          args    = List.map (fun (name, arg) -> (name, rename_pct arg)) acts;
          ret     = rename_pct rcts;
          sto_in  = rename_ps sin;
          sto_out = rename_ps sout},
         subs)

    let well_formed globstore cf =
      (* pmr: also need to check sto_out includes sto_in, possibly subtyping *)
      let whole_instore  = Store.upd cf.sto_in globstore in
      let whole_outstore = Store.upd cf.sto_out globstore in
        Store.closed whole_instore
          && Store.closed whole_outstore
          && List.for_all (fun (_, ct) -> Store.ctype_closed ct whole_instore) cf.args
          && match cf.ret with  (* we can return refs to uninitialized data *)
             | Ref (l, _) -> SLM.mem l whole_outstore
             | _          -> true

    let slocs cf =
      List.concat [Store.slocs cf.sto_in;
                   Store.slocs cf.sto_out;
                   M.maybe_cons (CType.sloc cf.ret) <|
                       M.map_partial (CType.sloc <.> snd) cf.args]
          |> M.sort_and_compact

    let subs sub cf =
      let apply_sub = CType.subs sub in
        make
          (List.map (S.Subst.apply sub) cf.qlocs)
          (List.map (M.app_snd apply_sub) cf.args)
          (apply_sub cf.ret)
          (Store.subs sub cf.sto_in)
          (Store.subs sub cf.sto_out)
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

  (******************************************************************************)
  (************************************ Specs ***********************************)
  (******************************************************************************)
  module Spec = struct
    type t = R.t prespec

    let empty = (SM.empty, SM.empty, SLM.empty)

    let map f (funspec, varspec, storespec) =
      (SM.map (f |> CType.map |> CFun.map |> M.app_fst) funspec,
       SM.map (f |> CType.map |> M.app_fst) varspec,
       Store.map f storespec)

    let add_fun b fn sp (funspec, varspec, storespec) =
      (Misc.sm_protected_add b fn sp funspec, varspec, storespec)

    let add_var b vn sp (funspec, varspec, storespec) =
      (funspec, Misc.sm_protected_add b vn sp varspec, storespec)

    let add_loc l ld (funspec, varspec, storespec) =
      (funspec, varspec, SLM.add l ld storespec)

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
end

module I = Make (IndexRefinement)

type ctype  = I.CType.t
type cfun   = I.CFun.t
type store  = I.Store.t
type cspec  = I.Spec.t
type ctemap = I.ctemap

let void_ctype   = Int (0, N.top)
let ptr_ctype    = Ref (S.fresh S.Abstract, N.top) 
let scalar_ctype = Int (0, N.top)
