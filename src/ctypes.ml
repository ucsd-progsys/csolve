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

type seq_polarity =     (* direction in which sequence extends *)
  | Pos                 (* +ve unbounded         *)
  | PosNeg              (* +ve and -ve unbounded *)
  | PosB of int         (* +ve fewer than k > 0 times *)

let is_unbounded = function
  | PosB _ -> false 
  | _      -> true

let seq_polarity_lub (p1: seq_polarity) (p2: seq_polarity): seq_polarity =
  match p1, p2 with
    | PosNeg, _ | _, PosNeg -> PosNeg
    | Pos, _    | _, Pos    -> Pos
    | PosB a, PosB b        -> PosB (max a b)

module Index = struct
  type t =
    | IBot                             (* empty sequence *)
    | IInt of int                      (* singleton n >= 0 *)
    | ISeq of int * int * seq_polarity (* arithmetic sequence (n, m): n op mk for all n, m >= 0, k, op given by polarity *)

  let top = ISeq (0, 1, PosNeg)

  let nonneg = ISeq (0, 1, Pos)

  let d_index () = function
    | IBot                -> P.text "false"
    | IInt n              -> P.num n
    | ISeq (n, m, Pos)    -> P.dprintf "%d[%d]" n m
    | ISeq (n, m, PosNeg) -> P.dprintf "%d{%d}" n m
    | ISeq (n, m, PosB k) -> P.dprintf "%d[%d < %d]" n m (m * k)
  
  let rec is_subindex i1 i2 =
    match i1, i2 with
      | IBot, _                                       -> true
      | IInt n, IInt m                                -> n = m
      | IInt n, ISeq (m, k, Pos)                      -> m <= n && (n - m) mod k = 0
      | IInt n, ISeq (m, k, PosB b)                   -> is_subindex (IInt n) (ISeq (m, k, Pos)) && n < m + b * k
      | IInt n, ISeq (m, k, PosNeg)                   -> (n - m) mod k = 0
      | ISeq (n, l, (Pos | PosB _)), ISeq (m, k, Pos) -> m <= n && k <= l && l mod k = 0 && (n - m) mod k = 0
      | ISeq (n, l, _), ISeq (m, k, PosNeg)           -> k <= l && l mod k = 0 && (n - m) mod k = 0
      | ISeq (n, l, PosB a), ISeq (m, k, PosB b)      -> is_subindex (ISeq (n, l, Pos)) (ISeq (m, k, Pos)) && a <= b
      | _                                             -> false

  let of_int i =
    if i >= 0 then IInt i else top

  let lub i1 i2 =
    if is_subindex i1 i2 then
      i2
    else if is_subindex i2 i1 then
      i1
    else
      match i1, i2 with
        | IInt m, IInt n                                  -> ISeq (min n m, abs (n - m), Pos)
        | IInt n, ISeq (m, k, p) | ISeq (m, k, p), IInt n -> ISeq (min n m, M.gcd k (abs (n - m)), p)
        | ISeq (n, l, p), ISeq (m, k, q)                  -> ISeq (min n m, M.gcd l (M.gcd k (abs (n - m))), seq_polarity_lub p q)
        | _                                               -> assert false

  let plus i1 i2 =
    match (i1, i2) with
      | (IBot, _) | (_, IBot)                               -> IBot
      | (IInt n, IInt m)                                    -> IInt (n + m)
      | (IInt n, ISeq (m, k, p)) | (ISeq (m, k, p), IInt n) -> ISeq (n + m, k, p)
      | (ISeq (n1, k1, p1), ISeq (n2, k2, p2))              -> ISeq (n1 + n2, M.gcd k1 k2, seq_polarity_lub p1 p2)

  (* pmr: prove this has the appropriate monotonicity property *)
  let minus i1 i2 =
    match (i1, i2) with
      | (IBot, _) | (_, IBot)                -> IBot
      | (IInt n, IInt m)                     -> IInt (n - m)
      | (ISeq (m, k, p), IInt n) when m >= n -> ISeq (m - n, k, p)
      | _                                    -> top

  let constop op i1 i2 =
    match (i1, i2) with
      | (IBot, _) | (_, IBot) -> IBot
      | (IInt n, IInt m)      -> IInt (op n m)
      | _                     -> top

  let scale x = function
    | IBot           -> IBot
    | IInt n         -> IInt (n * x)
    | ISeq (n, m, p) -> ISeq (n * x, m * x, p)

  (* pmr: prove this has the appropriate monotonicity property *)
  let mult i1 i2 =
    match (i1, i2) with
      | (IBot, _) | (_, IBot)                               -> IBot
      | (IInt n, IInt m)                                    -> IInt (n * m)
      | (IInt n, ISeq (m, k, p)) | (ISeq (m, k, p), IInt n) -> ISeq (n * m, n * k, p)
      | _                                                   -> top

  let div =
    constop (/)

  let unsign = function
    | ISeq (m, k, _) when m < 0 -> nonneg
    | ISeq (_, _, PosNeg)       -> nonneg
    | IInt n when n < 0         -> nonneg
    | i                         -> i
end

(******************************************************************************)
(***************************** Periodic Locations *****************************)
(******************************************************************************)

type ploc =
  | PLAt of int                 (* location n *)
  | PLSeq of int * seq_polarity (* location n plus periodic repeats *)

let d_ploc (): ploc -> P.doc = function
  | PLAt i            -> P.dprintf "PLAt %d" i
  | PLSeq (i, Pos)    -> P.dprintf "PLSeq %d+" i
  | PLSeq (i, PosNeg) -> P.dprintf "PLSeq %d+/-" i
  | PLSeq (i, PosB k) -> P.dprintf "PLSeq %d+%d" i k

module Ploc = struct
  type t = ploc

  let compare = compare
end

module PlocSet        = Set.Make (Ploc)
module PlocSetPrinter = P.MakeSetPrinter (PlocSet)

let d_plocset () ps =
  PlocSetPrinter.d_set ", " d_ploc () ps

let index_of_ploc (pl: ploc) (p: int) =
  match pl with
    | PLAt n         -> Index.IInt n
    | PLSeq (n, pol) -> Index.ISeq (n, p, pol)

let ploc_of_index = function
  | Index.IInt n         -> PLAt n
  | Index.ISeq (n, _, p) -> PLSeq (n, p)
  | Index.IBot           -> halt <| E.bug "Can't convert IBot to ploc@!@!"

let ploc_start: ploc -> int = function
  | PLAt n | PLSeq (n, _) -> n

let ploc_compare (pl1: ploc) (pl2: ploc): int =
  compare (ploc_start pl1) (ploc_start pl2)

let ploc_periodic = function
  | PLSeq _ -> true
  | PLAt _  -> false

let ploc_bounded_periodic = function
  | PLSeq (_, PosB _) -> true
  | _                 -> false

let ploc_unbounded_periodic = function
  | PLSeq (_, Pos)    -> true
  | PLSeq (_, PosNeg) -> true
  | _                 -> false

let ploc_contains (pl1: ploc) (pl2: ploc) (p: int): bool =
  match (pl1, pl2) with
    | (PLAt n1, PLAt n2)                            -> n1 = n2
    | (PLAt n, PLSeq (m, PosB 0))                   -> n = m
    | (PLAt _,             PLSeq _)                          
    | (PLSeq (_, PosB _),  PLSeq (_, Pos))        
    | (PLSeq (_, Pos),    PLSeq (_, PosNeg)) 
    | (PLSeq (_, PosB _), PLSeq (_, PosNeg))        -> false
    | (PLSeq (n1, PosNeg), PLAt n2)              
    | (PLSeq (n1, PosNeg), PLSeq (n2, _))           -> (n2 - n1) mod p = 0
    | (PLSeq (n1, Pos), PLAt n2)                 
    | (PLSeq (n1, Pos), PLSeq (n2, (Pos | PosB _))) -> (n2 - n1) mod p = 0 && n1 <= n2 
    | (PLSeq (n1, PosB k), PLAt n2)                 -> (n2 - n1) mod p = 0 && n1 <= n2 && n2 < n1 + (p * k)
    | (PLSeq (n1, PosB k1), PLSeq (n2, PosB k2))    -> (n2 - n1) mod p = 0 && n1 <= n2 && n2 + (p * k2) <= n1 + (p * k1)

let ploc_contains_index pl p i =
  ploc_contains pl (ploc_of_index i) p && 
    match i with Index.ISeq (_, k, _) -> k mod p = 0 | _ -> true
 
let ploc_offset (pl: ploc) (n: int): ploc =
  match pl with
    | PLAt n'       -> PLAt (n + n')
    | PLSeq (n', p) -> PLSeq (n + n', p)

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
  let lub          = fun i1 i2 -> Some (Index.lub i1 i2)
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

type 'a contents = (ploc * C.location * 'a prefield) list

type 'a preldesc = (* period: *) int option * 'a contents

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
    val collide     : ploc -> t -> ploc -> t -> int -> bool
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

    exception TypeDoesntFit of ploc * CType.t * t

    val empty         : t
    val get_period    : t -> int option
    val add           : C.location -> ploc -> Field.t -> t -> t
    val add_index     : C.location -> Index.t -> Field.t -> t -> t
    val create        : C.location -> (Index.t * Field.t) list -> t
    val remove        : ploc -> t -> t
    val shrink_period : int -> (Field.t -> Field.t -> 'b -> 'b) -> 'b -> t -> t * 'b
    val mem           : ploc -> t -> bool
    val find          : ploc -> t -> (ploc * Field.t) list
    val find_index    : Index.t -> t -> (ploc * Field.t) list
    val foldn         : (int -> 'a -> ploc -> Field.t -> 'a) -> 'a -> t -> 'a
    val fold          : ('a -> ploc -> Field.t -> 'a) -> 'a -> t -> 'a
    val map           : ('a prefield -> 'b prefield) -> 'a preldesc -> 'b preldesc
    val mapn          : (int -> ploc -> 'a prefield -> 'b prefield) -> 'a preldesc -> 'b preldesc
    val iter          : (ploc -> Field.t -> unit) -> t -> unit
    val indices_of_t  : t -> Index.t list
    val d_ldesc       : unit -> t -> Pretty.doc
  end

  module Store:
  sig
    type t = R.t prestore

    val domain       : t -> Sloc.t list
    val slocs        : t -> Sloc.t list
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

    let rec closest_start_before p pl1 pl2 =
      match pl1, pl2 with
        | PLAt n, _                                                      -> n
        | PLSeq (n, (Pos | PosNeg)), _ | PLSeq (n, _), PLSeq (_, PosNeg) -> p * ((ploc_start pl2 - n) / p)
        | PLSeq (n, PosB k), _                                           ->
            let s = n + p * k in
              if s >= ploc_start pl2 then closest_start_before p (PLSeq (n, Pos)) pl2 else s

    (* TODO: RJ, please check *)
    let collide pl1 pct1 pl2 pct2 p =
      let (pl1, pct1), (pl2, pct2) = if ploc_start pl1 <= ploc_start pl2 then ((pl1, pct1), (pl2, pct2)) else ((pl2, pct2), (pl1, pct1)) in
      let w1, w2                   = width pct1, width pct2 in
      let s1, s2                   = closest_start_before p pl1 pl2, ploc_start pl2 in
      let forward_collision        = s1 + w1 > s2 &&
                                       match pl1 with
                                         | PLAt _                    -> false
                                         | PLSeq (_, (Pos | PosNeg)) -> s2 + w2 > s1 + p
                                         | PLSeq (n, PosB k)         ->
                                             let s1 = s1 + p in
                                               s1 < n + k * p && s2 + w2 > s1 in
      let backward_collision      = match pl2 with
                                       | PLAt _                    -> false
                                       | PLSeq (_, (Pos | PosB _)) -> false
                                       | PLSeq (_, PosNeg)         -> s2 + w2 > s1 + p
      in forward_collision || backward_collision

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
    (* - The list always contains at least one item.
       - The period is non-negative if present.
       - If no period is present, the list contains no periodic locations.
       - The list does not contain the location PLEverywhere.
       - For all (pl1, pct1), (pl2, pct2) in the list, not (Ctypes.collide pl1 pct1 pl2 pct2 period)
       (if there is no period, then pl1 and pl2 are not periodic, so the value used for the period is irrelevant).
       - All (pl, pct) in the list are sorted by pl according to ploc_start.

       The period is always positive.
    *)
    type t = R.t preldesc

    exception TypeDoesntFit of ploc * CType.t * t

    let empty = (None, [])

    let get_period (po, _) =
      po

    (* 0 is an ok default for all the functions we'll be calling by the above invariant. *)
    let get_period_default (po: int option): int =
      M.get_option 0 po

    let collides_with pl1 fld1 po (pl2, _, fld2) =
      CType.collide pl1 (Field.type_of fld1) pl2 (Field.type_of fld2) (get_period_default po)
      >> (function true -> ignore <| P.printf "collides_with: pl1, fld1 = %a, %a  pl2, fld2 = %a, %a po = %d \n" 
            d_ploc pl1 Field.d_field fld1
            d_ploc pl2 Field.d_field fld2
            (get_period_default po)     
          | _ -> ())

    let rec insert loc pl fld = function
      | []                                 -> [(pl, loc, fld)]
      | (pl2, loc2, fld2) :: flds' as flds -> if ploc_start pl <= ploc_start pl2 
                                              then (pl, loc, fld) :: flds
                                              else (pl2, loc2, fld2) :: insert loc pl fld flds'

    let fits pl fld po flds =
      (not (ploc_unbounded_periodic pl) || (M.maybe_bool po && CType.width (Field.type_of fld) <= get_period_default po))
      && not (List.exists (collides_with pl fld po) flds)

    let add loc pl fld (po, flds) =
      if fits pl fld po flds then (po, insert loc pl fld flds) else 
        (* let _ = asserti false "type doesnt fit!" in *) 
        raise (TypeDoesntFit (pl, Field.type_of fld, (po, flds)))

    let remove pl (po, flds) =
      (po, List.filter (fun (pl2, _, _) -> not (pl = pl2)) flds)

    let swallow_repeats f b flds =
      try
        let pl1, _, fld1 = List.find (fun (pl, _, _) -> ploc_periodic pl) flds in
        let rs, us       = List.partition (fun (pl2, _, _) -> ploc_start pl1 < ploc_start pl2) flds in
        let b            = List.fold_left (fun b (_, _, fld2) -> f fld1 fld2 b) b rs in
          (us, b)
      with Not_found ->
        (* No periods, so nothing to do *)
        (flds, b)

    let shrink_period p f b (po, flds) =
      asserti (p > 0) "god damn assertion fails in shrink_period";
      let p     = M.gcd (get_period_default po) p in
      let gs    = M.groupby (fun (pl, _, _) -> (ploc_start pl) mod p) flds in
      let gs, b = List.fold_left (fun (gs, b) g -> let (g, b) = swallow_repeats f b g in (g :: gs, b)) ([], b) gs in
      let flds  = List.sort (fun (pl1, _, _) (pl2, _, _) -> ploc_compare pl1 pl2) (List.concat gs) in
        if not (M.exists_pair (fun (pl1, loc1, fld1) (pl2, loc2, fld2) -> CType.collide pl1 fld1 pl2 fld2 p) flds) then
          ((Some p, flds), b)
        else
          (* pmr: TODO - better error here *)
          assert false

    let shrink_period_fail_on_conflict p ld =
      (* pmr: TODO - better error on failure *)
      shrink_period p (fun _ _ _ -> assert false) () ld |> fst

    let add_index loc i fld ld =
      match i with
      | Index.ISeq (n, m, p) -> 
          ld |> ((is_unbounded p) <?> shrink_period_fail_on_conflict m)
             |> add loc (PLSeq (n, p)) fld
      | _  -> 
          add loc (ploc_of_index i) fld ld

    let create loc iflds =
      List.fold_left (add_index loc |> M.uncurry |> M.flip) empty iflds

    let mem pl1 (po, pcts) =
      if ploc_periodic pl1 && not (Misc.maybe_bool po) then
        false
      else
        let p = get_period_default po in
          List.exists (fun (pl2, _, _) -> ploc_contains pl1 pl2 p || ploc_contains pl2 pl1 p) pcts

    let find pl1 (po, pcts) =
      if ploc_periodic pl1 && not (Misc.maybe_bool po) then
        []
      else
        let p = get_period_default po in
             pcts
          |> List.filter (fun (pl2, _, _) -> ploc_contains pl1 pl2 p || ploc_contains pl2 pl1 p)
          |> List.map (fun (pl, _, pct) -> (pl, pct))

    let find_index i ((po, _) as ld) =
      let pcts = find (ploc_of_index i) ld in
      let p    = get_period_default po in
        List.filter (fun (pl, pct) -> ploc_contains_index pl p i) pcts

    let rec foldn_aux f n b = function
      | []                   -> b
      | (pl, _, pct) :: pcts -> foldn_aux f (n + 1) (f n b pl pct) pcts

    let foldn f b (po, pcts) =
      foldn_aux f 0 b pcts

    let fold f b ld =
      foldn (fun _ b pl pct -> f b pl pct) b ld

    let mapn f (po, flds) =
      (po, M.mapi (fun n (pl, loc, fld) -> (pl, loc, f n pl fld)) flds)

    let map f (po, flds) =
      (po, List.map (fun (pl, loc, fld) -> (pl, loc, f fld)) flds)

    let iter f ld =
      fold (fun _ pl fld -> f pl fld) () ld

    let referenced_slocs ld =
      fold begin fun rss _ pct -> match CType.sloc pct with 
        | None -> rss 
        | Some s -> SS.add s rss
      end SS.empty ld

    let indices_of_t (po, pcts) = 
      match po with 
      | None   -> List.map (function (PLAt n, _, _) -> Index.IInt n) pcts
      | Some p -> List.map (fun (pl,_,_) -> index_of_ploc pl p) pcts

    let d_ldesc () (po, flds) =
      let p = get_period_default po in
        P.dprintf "@[%t@]" (fun () -> P.seq (P.dprintf ",@!") (fun (pl, _, fld) -> P.dprintf "%a: %a" Index.d_index (index_of_ploc pl p) Field.d_field fld) flds)
  end

  module Store = struct
    type t = R.t prestore

    let map f =
      f |> CType.map |> Field.map_type |> LDesc.map |> SLM.map

    let map_ct f =
      SLM.map (LDesc.map (Field.map_type f))

    let fold f b ps =
      SLM.fold begin fun l ld b ->
        let p = LDesc.get_period ld |> M.get_option 0 in
          LDesc.fold (fun b pl pct -> f b l (index_of_ploc pl p) pct) b ld
      end ps b

    let domain ps =
      SLM.fold (fun s _ ss -> s :: ss) ps []

    let slocs ps =
         ps
      |> domain
      |> M.flip (fold (fun acc _ _ pct -> M.maybe_cons (CType.sloc pct) acc)) ps
      |> M.sort_and_compact

    let find l ps =
      try SLM.find l ps with Not_found -> LDesc.empty

    let find_index l i ps =
      ps |> find l |> LDesc.find_index i |> List.map snd

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
      SLM.fold (fun _ ld acc -> (LDesc.indices_of_t ld) ++ acc) st []

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

let void_ctype   = Int (0, Index.top)
let ptr_ctype    = Ref (S.fresh S.Abstract, Index.top) 
let scalar_ctype = Int (0, Index.top)

