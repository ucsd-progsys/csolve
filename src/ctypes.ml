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

type seq_polarity = (* whether sequence extends positively only or in both directions *)
  | Pos
  | PosNeg

module Index = struct
  let seq_polarity_lub (p1: seq_polarity) (p2: seq_polarity): seq_polarity =
    match p1, p2 with
      | PosNeg, _ | _, PosNeg -> PosNeg
      | _                     -> Pos

  type t =
    | IBot                             (* empty sequence *)
    | IInt of int                      (* singleton n >= 0 *)
    | ISeq of int * int * seq_polarity (* arithmetic sequence (n, m): n + mk for all n, m >= 0, k *)

  let top = ISeq (0, 1, PosNeg)

  let nonneg = ISeq (0, 1, Pos)

  let d_index () = function
    | IBot                -> P.text "false"
    | IInt n              -> P.num n
    | ISeq (n, m, Pos)    -> P.dprintf "%d[%d]" n m
    | ISeq (n, m, PosNeg) -> P.dprintf "%d{%d}" n m

  let is_subindex i1 i2 =
    match (i1, i2) with
      | (IBot, _)                             -> true
      | (IInt n, IInt m)                      -> n = m
      | (IInt n, ISeq (m, k, Pos))            -> m <= n && (n - m) mod k = 0
      | (IInt n, ISeq (m, k, PosNeg))         -> (n - m) mod k = 0
      | (ISeq (n, l, Pos), ISeq (m, k, Pos))  -> m <= n && k <= l && l mod k = 0 && (n - m) mod k = 0
      | (ISeq (n, l, _), ISeq (m, k, PosNeg)) -> k <= l && l mod k = 0 && (n - m) mod k = 0
      | _                                     -> false

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
(************************************ Types ***********************************)
(******************************************************************************)

type 'a prectype =
  | CTInt of int * 'a  (* fixed-width integer *)
  | CTRef of Sloc.t * 'a (* reference *)

let prectype_map f = function
  | CTInt (i, x) -> CTInt (i, f x)
  | CTRef (l, x) -> CTRef (l, f x)

let d_prectype (d_i: unit -> 'a -> P.doc) (): 'a prectype -> P.doc = function
  | CTInt (n, i) -> P.dprintf "int(%d, %a)" n d_i i
  | CTRef (s, i) -> P.dprintf "ref(%a, %a)" S.d_sloc s d_i i

let prectype_width: 'a prectype -> int = function
  | CTInt (n, _) -> n
  | CTRef (_)    -> CM.int_width

let prectype_sloc: 'a prectype -> S.t option = function
  | CTRef (s, _) -> Some s
  | CTInt _      -> None

let prectype_subs (subs: S.Subst.t): 'a prectype -> 'a prectype = function
  | CTRef (s, i) -> CTRef (S.Subst.apply subs s, i)
  | pct          -> pct

let prectype_eq (pct1: 'a prectype) (pct2: 'a prectype): bool =
  match (pct1, pct2) with
    | (CTRef (l1, i1), CTRef (l2, i2)) -> S.eq l1 l2 && i1 = i2
    | _                                -> pct1 = pct2

let is_void: 'a prectype -> bool = function
  | CTInt (0, _) -> true
  | _            -> false

let is_ref: 'a prectype -> bool = function
  | CTRef _ -> true
  | _       -> false

type ctype = Index.t prectype

let d_ctype () (ct: ctype): P.doc =
  d_prectype Index.d_index () ct

exception NoLUB of ctype * ctype

let ctype_lub (t1: ctype) (t2: ctype): ctype =
  match (t1, t2) with
    | (CTInt (n1, i1), CTInt (n2, i2)) when n1 = n2    -> CTInt (n1, Index.lub i1 i2)
    | (CTRef (s1, i1), CTRef (s2, i2)) when S.eq s1 s2 -> CTRef (s1, Index.lub i1 i2)
    | _                                                -> raise (NoLUB (t1, t2))

let is_subctype (ct1: ctype) (ct2: ctype): bool =
  match (ct1, ct2) with
    | (CTInt (n1, i1), CTInt (n2, i2)) when n1 = n2    -> Index.is_subindex i1 i2
    | (CTRef (s1, i1), CTRef (s2, i2)) when S.eq s1 s2 -> Index.is_subindex i1 i2
    | _                                                -> false

let ctype_of_const: C.constant -> ctype = function
  | C.CInt64 (v, ik, _) -> CTInt (C.bytesSizeOfInt ik, Index.of_int (Int64.to_int v))
  | C.CChr c            -> CTInt (CM.int_width, Index.IInt (Char.code c))
  | C.CReal (_, fk, _)  -> CTInt (CM.bytesSizeOfFloat fk, Index.top)
  | C.CStr _            -> CTRef (S.fresh S.Abstract, Index.IInt 0)
  | c                   -> halt <| E.bug "Unimplemented ctype_of_const: %a@!@!" C.d_const c

let void_ctype = CTInt (0, Index.top)

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

let ploc_periodic: ploc -> bool = function
  | PLAt _  -> false
  | PLSeq _ -> true

let ploc_contains (pl1: ploc) (pl2: ploc) (p: int): bool =
  match (pl1, pl2) with
    | (PLAt n1, PLAt n2)                   -> n1 = n2
    | (PLAt _, PLSeq _)                    -> false
    | (PLSeq (n1, Pos), PLAt n2)           -> n1 <= n2 && (n2 - n1) mod p = 0
    | (PLSeq (n1, PosNeg), PLAt n2)        -> (n2 - n1) mod p = 0
    | (PLSeq (n1, Pos), PLSeq (n2, Pos))   -> n1 <= n2 && (n2 - n1) mod p = 0
    | (PLSeq (n1, Pos), PLSeq (_, PosNeg)) -> false
    | (PLSeq (n1, PosNeg), PLSeq (n2, _))  -> (n2 - n1) mod p = 0

let ploc_contains_index pl p i =
  match (pl, i) with
    | (_, Index.IBot)                             -> false
    | (PLAt n, Index.IInt m)                      -> n = m
    | (PLAt _, Index.ISeq _)                      -> false
    | (PLSeq (n, Pos), Index.IInt m)              -> n <= m && (m - n) mod p = 0
    | (PLSeq (n, PosNeg), Index.IInt m)           -> (m - n) mod p = 0
    | (PLSeq (n, Pos), Index.ISeq (m, k, Pos))    -> k mod p = 0 && n <= m && (m - n) mod p = 0
    | (PLSeq (n, Pos), Index.ISeq (m, k, PosNeg)) -> false
    | (PLSeq (n, PosNeg), Index.ISeq (m, k, _))   -> k mod p = 0 && (m - n) mod p = 0

let ploc_offset (pl: ploc) (n: int): ploc =
  match pl with
    | PLAt n'       -> PLAt (n + n')
    | PLSeq (n', p) -> PLSeq (n + n', p)

let prectypes_collide (pl1: ploc) (pct1: 'a prectype) (pl2: ploc) (pct2: 'a prectype) (p: int): bool =
  let ((pl1, pct1), (pl2, pct2)) = if ploc_start pl1 <= ploc_start pl2 then ((pl1, pct1), (pl2, pct2)) else ((pl2, pct2), (pl1, pct1)) in
  let (s1, s2)                   = (ploc_start pl1, ploc_start pl2) in
  let d                          = s2 - s1 in
  let pl1                        = if ploc_periodic pl1 then ploc_offset pl1 (p * (d / p)) else pl1 in
    s1 + prectype_width pct1 > s2 || (ploc_periodic pl1 && s2 + prectype_width pct2 > (s1 + p))

(******************************************************************************)
(*********************************** Stores ***********************************)
(******************************************************************************)

exception TypeDoesntFit

module LDesc = struct
  (* - The list always contains at least one item.
     - The period is non-negative if present.
     - If no period is present, the list contains no periodic locations.
     - The list does not contain the location PLEverywhere.
     - For all (pl1, pct1), (pl2, pct2) in the list, not (prectypes_collide pl1 pct1 pl2 pct2 period)
       (if there is no period, then pl1 and pl2 are not periodic, so the value used for the period is irrelevant).
     - All (pl, pct) in the list are sorted by pl according to ploc_start.

     The period is always positive.
  *)
  type 'a contents = (ploc * 'a prectype) list

  type 'a t = (* period: *) int option * 'a contents

  let empty: 'a t = (None, [])

  let get_period ((po, _): 'a t): int option =
    po

  (* 0 is an ok default for all the functions we'll be calling by the above invariant. *)
  let get_period_default (po: int option): int =
    M.get_option 0 po

  let collides_with (pl1: ploc) (pct1: 'a prectype) (po: int option) ((pl2, pct2): ploc * 'a prectype): bool =
    prectypes_collide pl1 pct1 pl2 pct2 (get_period_default po)

  let fits (pl: ploc) (pct: 'a prectype) (po: int option) (pcts: (ploc * 'a prectype) list): bool =
    (not (ploc_periodic pl) || (M.maybe_bool po && prectype_width pct <= get_period_default po))
      && not (List.exists (collides_with pl pct po) pcts)

  let rec insert (pl: ploc) (pct: 'a prectype): (ploc * 'a prectype) list -> (ploc * 'a prectype) list = function
    | []                           -> [(pl, pct)]
    | (pl2, pct2) :: pcts' as pcts -> if ploc_start pl <= ploc_start pl2 then (pl, pct) :: pcts else (pl2, pct2) :: insert pl pct pcts'

  let add (pl: ploc) (pct: 'a prectype) ((po, pcts): 'a t): 'a t =
    if fits pl pct po pcts then (po, insert pl pct pcts) else raise TypeDoesntFit

  let remove (pl: ploc) ((po, pcts): 'a t): 'a t =
    (po, List.filter (fun (pl2, _) -> not (pl = pl2)) pcts)

  let swallow_repeats (f: 'a prectype -> 'a prectype -> 'b -> 'b) (b: 'b) (pcts: (ploc * 'a prectype) list): (ploc * 'a prectype) list * 'b =
    try
      let (pl1, pct1) = List.find (fun (pl, _) -> ploc_periodic pl) pcts in
      let (rs, us)    = List.partition (fun (pl2, _) -> ploc_start pl1 < ploc_start pl2) pcts in
      let b           = List.fold_left (fun b (_, pct2) -> f pct1 pct2 b) b rs in
        (us, b)
    with Not_found ->
      (* No periods, so nothing to do *)
      (pcts, b)

  let shrink_period (p: int) (f: 'a prectype -> 'a prectype -> 'b -> 'b) (b: 'b) ((po, pcts): 'a t): 'a t * 'b =
    assert (p > 0);
    let p       = M.gcd (get_period_default po) p in
    let gs      = M.groupby (fun (pl, _) -> (ploc_start pl) mod p) pcts in
    let (gs, b) = List.fold_left (fun (gs, b) g -> let (g, b) = swallow_repeats f b g in (g :: gs, b)) ([], b) gs in
    let pcts    = List.sort (M.liftfst2 ploc_compare) (List.concat gs) in
      if not (M.exists_pair (fun (pl1, pct1) (pl2, pct2) -> prectypes_collide pl1 pct1 pl2 pct2 p) pcts) then
        ((Some p, pcts), b)
      else
        (* pmr: this is not quite descriptive enough *)
        raise TypeDoesntFit

  let shrink_period_fail_on_conflict (p: int) (ld: 'a t): 'a t =
    shrink_period p (fun _ _ _ -> raise TypeDoesntFit) () ld |> fst

  let add_index i pct ld =
    match i with
      | Index.ISeq (n, m, p) -> ld |> shrink_period_fail_on_conflict m |> add (PLSeq (n, p)) pct
      | _                    -> add (ploc_of_index i) pct ld

  let create icts =
    List.fold_left (M.uncurry add_index |> M.flip) empty icts

  let find (pl1: ploc) ((po, pcts): 'a t): (ploc * 'a prectype) list =
    if ploc_periodic pl1 && not (Misc.maybe_bool po) then
      []
    else
      let p = get_period_default po in
        List.filter (fun (pl2, _) -> ploc_contains pl1 pl2 p || ploc_contains pl2 pl1 p) pcts

  let find_index i ((po, _) as ld) =
    let pcts = find (ploc_of_index i) ld in
    let p    = get_period_default po in
      List.filter (fun (pl, pct) -> ploc_contains_index pl p i) pcts

  let rec foldn_aux (f: int -> 'a -> ploc -> 'b prectype -> 'a) (n: int) (b: 'a): (ploc * 'b prectype) list -> 'a = function
    | []                -> b
    | (pl, pct) :: pcts -> foldn_aux f (n + 1) (f n b pl pct) pcts

  let foldn (f: int -> 'a -> ploc -> 'b prectype -> 'a) (b: 'a) ((po, pcts): 'b t): 'a =
    foldn_aux f 0 b pcts

  let fold (f: 'a -> ploc -> 'b prectype -> 'a) (b: 'a) (ld: 'b t): 'a =
    foldn (fun _ b pl pct -> f b pl pct) b ld

  let mapn (f: int -> ploc -> 'a prectype -> 'b prectype) ((po, pcts): 'a t): 'b t =
    (po, pcts |> foldn_aux (fun n pcts pl pct -> (pl, f n pl pct) :: pcts) 0 [] |> List.rev)

  let map (f: 'a prectype -> 'b prectype) (ld: 'a t): 'b t =
    mapn (fun _ _ pct -> f pct) ld

  let referenced_slocs (ld: 'a t): SS.t =
    fold (fun rss _ pct -> match prectype_sloc pct with None -> rss | Some s -> SS.add s rss) SS.empty ld

  let d_ldesc (pt: unit -> 'a prectype -> P.doc) () ((po, pcts): 'a t): P.doc =
    let p = get_period_default po in
      (* JHALA
         let s = P.concat (P.text ",") P.break in
         let d = P.seq s (fun (pl, pct) -> P.dprintf "@[%a: %a@]" d_index (index_of_ploc pl p) pt pct) pcts in
         P.concat P.align (P.concat d P.unalign) *)
      P.dprintf "@[%t@]" (fun () -> P.seq (P.dprintf ",@!") (fun (pl, pct) -> P.dprintf "%a: %a" Index.d_index (index_of_ploc pl p) pt pct) pcts)
end

module PreStore = struct
  type 'a t = ('a LDesc.t) SLM.t

  let map f =
    f |> prectype_map |> LDesc.map |> SLM.map

  let map_ct f =
    SLM.map (LDesc.map f)

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
    |> M.flip (fold (fun acc _ _ pct -> M.maybe_cons (prectype_sloc pct) acc)) ps
    |> M.sort_and_compact

  let find l ps =
    try SLM.find l ps with Not_found -> LDesc.empty

  let find_index l i ps =
    ps |> find l |> LDesc.find_index i |> List.map snd

  (* pmr: why is this not rename_prestore? *)
  let subs_addrs subs ps =
    SLM.fold (fun s ld ps -> SLM.add (S.Subst.apply subs s) ld ps) ps SLM.empty

  let subs subs ps =
    ps |> map_ct (prectype_subs subs)
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

  let rename subs ps =
    let cns = LDesc.map (prectype_subs subs) in
      SLM.fold (fun l ld sm -> SLM.add (S.Subst.apply subs l) (cns ld) sm) ps SLM.empty

  let d_prestore_addrs () st =
    Pretty.seq (Pretty.text ",") (Sloc.d_sloc ()) (domain st)

  let d_prestore d_i () s  =
    P.dprintf "[@[%a@]]"
      (SLMPrinter.docMap ~sep:(P.dprintf ";@!")
         (fun l ld -> P.dprintf "%a |-> %a" S.d_sloc l (LDesc.d_ldesc (d_prectype d_i)) ld)) s
end

type store = Index.t PreStore.t

let d_store () (s: store): P.doc =
  SLMPrinter.d_map "\n" S.d_sloc (LDesc.d_ldesc d_ctype) () s

let prectype_closed (ct: 'a prectype) (sto: 'a PreStore.t) =
  match ct with
    | CTInt _      -> true
    | CTRef (l, _) -> SLM.mem l sto

(* move into module *)
let prestore_closed (sto: 'a PreStore.t): bool =
  PreStore.fold (fun closed _ _ ct -> closed && prectype_closed ct sto) true sto

(******************************************************************************)
(******************************* Function Types *******************************)
(******************************************************************************)

type 'a precfun =
  { qlocs       : S.t list;                     (* generalized slocs *)
    args        : (string * 'a prectype) list;  (* arguments *)
    ret         : 'a prectype;                  (* return *)
    sto_in      : 'a PreStore.t;                (* in store *)
    sto_out     : 'a PreStore.t;                (* out store *)
  }

type cfun = Index.t precfun

(* API *)
let mk_cfun qslocs args reto sin sout =
  { qlocs   = qslocs; 
    args    = args;
    ret     = reto;
    sto_in  = sin;
    sto_out = sout;
  }

let precfun_map f ft =
  { qlocs   = ft.qlocs;
    args    = List.map (Misc.app_snd f) ft.args;
    ret     = (* Misc.map_opt *) f ft.ret;
    sto_in  = SLM.map (LDesc.map f) ft.sto_in;
    sto_out = SLM.map (LDesc.map f) ft.sto_out;
  }

let d_slocs () slocs     = P.seq (P.text ";") (S.d_sloc ()) slocs
let d_arg d_i () (x, ct) = P.dprintf "%s : %a" x (d_prectype d_i) ct
let d_args d_i () args   = P.seq (P.dprintf ",@!") (d_arg d_i ()) args

let d_precfun d_i () ft  = 
  P.dprintf "forall    [%a]\narg       (@[%a@])\nret       %a\nstore_in  %a\nstore_out %a"
    d_slocs ft.qlocs
    (d_args d_i) ft.args
    (d_prectype d_i) ft.ret
    (PreStore.d_prestore d_i) ft.sto_in
    (PreStore.d_prestore d_i) ft.sto_out

let d_cfun () ft =
  d_precfun Index.d_index () ft

let cfun_instantiate ({qlocs = ls; args = acts; ret = rcts; sto_in = sin; sto_out = sout}: 'a precfun): 'a precfun * (S.t * S.t) list =
  let subs       = List.map (fun l -> (l, S.fresh S.Abstract)) ls in
  let rename_pct = prectype_subs subs in
  let rename_ps  = PreStore.rename subs in
    ({qlocs   = [];
      args    = List.map (fun (name, arg) -> (name, rename_pct arg)) acts;
      ret     = rename_pct rcts;
      sto_in  = rename_ps sin;
      sto_out = rename_ps sout},
     subs)

let precfun_well_formed (globstore: 'a PreStore.t) (cf: 'a precfun): bool =
     (* pmr: also need to check sto_out includes sto_in, possibly subtyping *)
  let whole_instore  = PreStore.upd cf.sto_in globstore in
  let whole_outstore = PreStore.upd cf.sto_out globstore in
     prestore_closed whole_instore
  && prestore_closed whole_outstore
  && List.for_all (fun (_, ct) -> prectype_closed ct whole_instore) cf.args
  && match cf.ret with  (* we can return refs to uninitialized data *)
        | CTRef (l, _) -> SLM.mem l whole_outstore
        | _            -> true

let cfun_slocs (cf: cfun): S.t list =
    List.concat [PreStore.slocs cf.sto_in;
                 PreStore.slocs cf.sto_out;
                 M.maybe_cons (prectype_sloc cf.ret) <|
                     M.map_partial (prectype_sloc <.> snd) cf.args]
 |> M.sort_and_compact

let cfun_subs (sub: S.Subst.t) (cf: cfun): cfun =
  let apply_sub = prectype_subs sub in
  mk_cfun (List.map (S.Subst.apply sub) cf.qlocs)
          (List.map (M.app_snd apply_sub) cf.args)
          (apply_sub cf.ret)
          (PreStore.subs sub cf.sto_in)
          (PreStore.subs sub cf.sto_out)

(******************************************************************************)
(******************************* Expression Maps ******************************)
(******************************************************************************)

(* pmr: need to check that expressions have unique types (which should certainly hold anyway) *)
module ExpKey = struct
  type t      = Cil.exp
  let compare = compare
end

module ExpMap = Map.Make (ExpKey)

module ExpMapPrinter = P.MakeMapPrinter(ExpMap)

type ctemap = ctype ExpMap.t

let d_ctemap () (em: ctemap): Pretty.doc =
  ExpMapPrinter.d_map "\n" Cil.d_exp d_ctype () em

(******************************************************************************)
(************************************ Specs ***********************************)
(******************************************************************************)

module PreSpec = struct
  type 'a t = ('a precfun * bool) SM.t * ('a prectype * bool) SM.t * 'a PreStore.t

  let empty = (SM.empty, SM.empty, SLM.empty)

  let map (f: 'a -> 'b) ((funspec, varspec, storespec): 'a t): 'b t =
    (SM.map (f |> prectype_map |> precfun_map |> M.app_fst) funspec,
     SM.map (f |> prectype_map |> M.app_fst) varspec,
     PreStore.map f storespec)

  let add_fun (fn: string) (sp: 'a precfun * bool) ((funspec, varspec, storespec): 'a t): 'a t =
    (SM.add fn sp funspec, varspec, storespec)

  let add_var (vn: string) (vspc: 'a prectype * bool) ((funspec, varspec, storespec): 'a t): 'a t =
    (funspec, SM.add vn vspc varspec, storespec)

  let add_loc (l: S.t) (ld: 'a LDesc.t) ((funspec, varspec, storespec): 'a t): 'a t =
    (funspec, varspec, SLM.add l ld storespec)

  let mem_fun (fn: string) ((funspec, _, _): 'a t): bool =
    SM.mem fn funspec

  let mem_var (vn: string) ((_, varspec, _): 'a t): bool =
    SM.mem vn varspec

  let store ((_, _, storespec): 'a t): 'a PreStore.t =
    storespec
end

type cspec = Index.t PreSpec.t
