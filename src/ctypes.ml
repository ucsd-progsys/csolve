module M = Misc
module P = Pretty
module E = Errormsg

open M.Ops

(******************************************************************************)
(*********************************** Indices **********************************)
(******************************************************************************)

type index =
  | IBot               (* empty sequence *)
  | IInt of int        (* singleton n >= 0 *)
  | ISeq of int * int  (* arithmetic sequence (n, m): n + mk for all k, n, m >= 0 *)
  | ITop               (* sequence of all values (including negatives) *)

let d_index (): index -> P.doc = function
  | IBot        -> P.text "⊥"
  | IInt n      -> P.num n
  | ISeq (n, m) -> P.dprintf "%d[%d]" n m
  | ITop        -> P.text "⊤"

let index_of_int (i: int): index =
  if i >= 0 then IInt i else ITop

let index_lub (i1: index) (i2: index): index =
  match (i1, i2) with
    | (IBot, i) | (i, IBot)                         -> i
    | (ITop, _) | (_, ITop)                         -> ITop
    | (IInt m, IInt n)                              -> if m = n then IInt m else ISeq (min n m, abs (n - m))
    | (IInt n, ISeq (m, k)) | (ISeq (m, k), IInt n) -> ISeq (min n m, M.gcd k (abs (n - m)))
    | (ISeq (n, l), ISeq (m, k))                    -> ISeq (min n m, M.gcd l (M.gcd k (abs (n - m))))

let index_plus (i1: index) (i2: index): index =
  match (i1, i2) with
    | (IBot, _) | (_, IBot)                         -> IBot
    | (ITop, _) | (_, ITop)                         -> ITop
    | (IInt n, IInt m)                              -> IInt (n + m)
    | (IInt n, ISeq (m, k)) | (ISeq (m, k), IInt n) -> ISeq (n + m, k)
    | (ISeq (n1, k1), ISeq (n2, k2)) when k1 = k2   -> ISeq (n1 + n2, k1)
    | (ISeq (n1, _), ISeq (n2, _))                  -> ISeq (n1 + n2, 1)

(* pmr: prove this has the appropriate monotonicity property *)
let index_minus (i1: index) (i2: index): index =
  match (i1, i2) with
    | (IBot, _) | (_, IBot)             -> IBot
    | (ITop, _) | (_, ITop)             -> ITop
    | (IInt n, IInt m) (* when n >= m *) -> IInt (n - m)
    | (ISeq (m, k), IInt n) when m >= n -> ISeq (m - n, k)
    | _                                 -> ITop

let index_constop (op: int -> int -> int) (i1: index) (i2: index): index =
  match (i1, i2) with
    | (IBot, _) | (_, IBot) -> IBot
    | (ITop, _) | (_, ITop) -> ITop
    | (IInt n, IInt m)      -> IInt (op n m)
    | _                     -> ITop

let index_scale (x: int): index -> index = function
  | IBot        -> IBot
  | ITop        -> ITop
  | IInt n      -> IInt (n * x)
  | ISeq (n, m) -> ISeq (n * x, m * x)

(* pmr: prove this has the appropriate monotonicity property *)
let index_mult (i1: index) (i2: index): index =
  match (i1, i2) with
    | (IBot, _) | (_, IBot)                         -> IBot
    | (ITop, _) | (_, ITop)                         -> ITop
    | (IInt n, IInt m)                              -> IInt (n * m)
    | (IInt n, ISeq (m, k)) | (ISeq (m, k), IInt n) -> ISeq (n * m, n * k)
    | _                                             -> ITop

let index_div: index -> index -> index =
  index_constop (/)

let is_subindex (i1: index) (i2: index): bool =
  match (i1, i2) with
    | (IBot, _)                  -> true
    | (_, ITop)                  -> true
    | (IInt n, IInt m)           -> n = m
    | (IInt n, ISeq (m, k))      -> m <= n && (n - m) mod k = 0
    | (ISeq (n, l), ISeq (m, k)) -> m <= n && k <= l && l mod k = 0 && (n - m) mod k = 0
    | _                          -> false

(******************************************************************************)
(************************************ Types ***********************************)
(******************************************************************************)

(* type sloc = int (* store locations *) *)

type sloc = ALoc of int | CLoc of int   (* store location *)

type 'a prectype =
  | CTInt of int * 'a  (* fixed-width integer *)
  | CTRef of sloc * 'a (* reference *)

let prectype_map f = function
  | CTInt (i, x) -> CTInt (i, f x)
  | CTRef (l, x) -> CTRef (l, f x)

let d_sloc (): sloc -> P.doc = function
  | ALoc i -> P.dprintf "%d" i
  | CLoc i -> P.dprintf "%d" i

let d_prectype (d_i: unit -> 'a -> P.doc) (): 'a prectype -> P.doc = function
  | CTInt (n, i) -> P.dprintf "int(%d, %a)" n d_i i
  | CTRef (s, i) -> P.dprintf "ref(%a, %a)" d_sloc s d_i i

let prectype_width: 'a prectype -> int = function
  | CTInt (n, _) -> n
  | CTRef (_)    -> 1

let prectype_replace_sloc (s1: sloc) (s2: sloc): 'a prectype -> 'a prectype = function
  | CTRef (s3, i) when s3 = s1 -> CTRef (s2, i)
  | pct                        -> pct

type ctype = index prectype


let d_ctype () (ct: ctype): P.doc =
  d_prectype d_index () ct

exception NoLUB of ctype * ctype

let ctype_lub (t1: ctype) (t2: ctype): ctype =
  match (t1, t2) with
    | (CTInt (n1, i1), CTInt (n2, i2)) when n1 = n2 -> CTInt (n1, index_lub i1 i2)
    | (CTRef (s1, i1), CTRef (s2, i2)) when s1 = s2 -> CTRef (s1, index_lub i1 i2)
    | _                                             -> raise (NoLUB (t1, t2))

let is_subctype (ct1: ctype) (ct2: ctype): bool =
  match (ct1, ct2) with
    | (CTInt (n1, i1), CTInt (n2, i2)) when n1 = n2 -> is_subindex i1 i2
    | (CTRef (s1, i1), CTRef (s2, i2)) when s1 = s2 -> is_subindex i1 i2
    | _                                             -> false

(******************************************************************************)
(***************************** Periodic Locations *****************************)
(******************************************************************************)

type ploc =
  | PLAt of int   (* location n *)
  | PLSeq of int  (* location n plus periodic repeats *)

let index_of_ploc (pl: ploc) (p: int) =
  match pl with
    | PLAt n  -> IInt n
    | PLSeq n -> ISeq (n, p)

let ploc_of_index: index -> ploc = function
  | IInt n           -> PLAt n
  | ISeq (n, _)      -> PLSeq n
  | IBot | ITop as i -> E.s <| E.error "Can't convert %a to ploc" d_index i

let ploc_start: ploc -> int = function
  | PLAt n | PLSeq n -> n

let ploc_compare (pl1: ploc) (pl2: ploc): int =
  compare (ploc_start pl1) (ploc_start pl2)

let ploc_le (pl1: ploc) (pl2: ploc): bool =
  ploc_start pl1 <= ploc_start pl2

let ploc_periodic: ploc -> bool = function
  | PLAt _  -> false
  | PLSeq _ -> true

let ploc_contains (pl1: ploc) (pl2: ploc) (p: int): bool =
  match (pl1, pl2) with
    | (PLAt n1, PLAt n2)                         -> n1 = n2
    | (PLAt _, PLSeq _)                          -> false
    | (PLSeq n1, PLAt n2) | (PLSeq n1, PLSeq n2) -> n1 <= n2 && (n2 - n1) mod p = 0

let ploc_offset (pl: ploc) (n: int): ploc =
  match pl with
    | PLAt n'  -> PLAt (n + n')
    | PLSeq n' -> PLSeq (n + n')

let prectypes_collide (pl1: ploc) (pct1: 'a prectype) (pl2: ploc) (pct2: 'a prectype) (p: int): bool =
  let ((pl1, pct1), (pl2, pct2)) = if ploc_le pl1 pl2 then ((pl1, pct1), (pl2, pct2)) else ((pl2, pct2), (pl1, pct1)) in
  let d                          = ploc_start pl2 - ploc_start pl1 in
  let pl1                        = if ploc_periodic pl1 then ploc_offset pl1 (p * (d / p)) else pl1 in
  let (s1, s2)                   = (ploc_start pl1, ploc_start pl2) in
    s1 + prectype_width pct1 > s2 || (ploc_periodic pl1 && s2 + prectype_width pct2 > (s1 + p))

(******************************************************************************)
(*********************************** Stores ***********************************)
(******************************************************************************)

exception TypeDoesntFit

module LDesc = struct
  (* Invariants:
     - The period is non-negative if present.
     - If no period is present, the list contains no periodic locations.
     - For all (pl1, pct1), (pl2, pct2) in the list, not (prectypes_collide pl1 pct1 pl2 pct2 period)
       (if there is no period, then pl1 and pl2 are not periodic, so the value used for the period is irrelevant).
     - All (pl, pct) in the list are sorted by pl according to ploc_le.
  *)
  type 'a t = (* period: *) int option * (ploc * 'a prectype) list

  let empty: 'a t = (None, [])

  let get_period ((po, _): 'a t): int option =
    po

  (* 0 is an ok default for all the functions we'll be calling by the above invariant. *)
  let get_period_default (po: int option): int =
    M.get_option 0 po

  let collides_with (pl1: ploc) (pct1: 'a prectype) (po: int option) ((pl2, pct2): ploc * 'a prectype): bool =
    prectypes_collide pl1 pct1 pl2 pct2 (get_period_default po)

  let collisions (pl: ploc) (pct: 'a prectype) ((po, pcts): 'a t): (ploc * 'a prectype) list =
    List.find_all (collides_with pl pct po) pcts

  let fits (pl: ploc) (pct: 'a prectype) ((po, pcts): 'a t): bool =
    (not (ploc_periodic pl) || (M.maybe_bool po && prectype_width pct <= get_period_default po))
      && not (List.exists (collides_with pl pct po) pcts)

  (* Don't use this directly - use add instead!  This does not check for collisions. *)
  let rec insert (pl: ploc) (pct: 'a prectype): (ploc * 'a prectype) list -> (ploc * 'a prectype) list = function
    | []                           -> [(pl, pct)]
    | (pl2, pct2) :: pcts' as pcts -> if ploc_le pl pl2 then (pl, pct) :: pcts else (pl2, pct2) :: insert pl pct pcts'

  let add (pl: ploc) (pct: 'a prectype) ((po, pcts) as ld: 'a t): 'a t =
    if fits pl pct ld then (po, insert pl pct pcts) else raise TypeDoesntFit

  let remove (pl: ploc) ((po, pcts): 'a t): 'a t =
    (po, List.filter (fun (pl2, _) -> not (pl = pl2)) pcts)

  let swallow_repeats (f: 'a prectype -> 'a prectype -> 'b -> 'b) (b: 'b) (pcts: (ploc * 'a prectype) list): (ploc * 'a prectype) list * 'b =
    try
      let (pl1, pct1) = List.find (fun (pl, _) -> ploc_periodic pl) pcts in
      let (rs, us)    = List.partition (fun (pl2, _) -> not (ploc_le pl2 pl1)) pcts in
      let b           = List.fold_left (fun b (_, pct2) -> f pct1 pct2 b) b rs in
        (us, b)
    with Not_found ->
      (* No periods, so nothing to do *)
      (pcts, b)

  let shrink_period (p: int) (f: 'a prectype -> 'a prectype -> 'b -> 'b) (b: 'b) ((po, pcts): 'a t): 'a t * 'b =
    let p       = M.gcd (get_period_default po) p in
    let gs      = M.groupby (fun (pl, _) -> (ploc_start pl) mod p) pcts in
    let (gs, b) = List.fold_left (fun (gs, b) g -> let (g, b) = swallow_repeats f b g in (g :: gs, b)) ([], b) gs in
    let pcts    = List.sort (M.Ops.liftfst2 ploc_compare) (List.concat gs) in
      if not (M.exists_pair (fun (pl1, pct1) (pl2, pct2) -> prectypes_collide pl1 pct1 pl2 pct2 p) pcts) then
        ((Some p, pcts), b)
      else
        (* pmr: this is not quite descriptive enough *)
        raise TypeDoesntFit

  let find (pl1: ploc) ((po, pcts): 'a t): (ploc * 'a prectype) list =
    let p = get_period_default po in
      List.filter (fun (pl2, _) -> ploc_contains pl1 pl2 p || ploc_contains pl2 pl1 p) pcts

  let map (f: 'a prectype -> 'b prectype) ((po, pcts): 'a t): 'b t =
    (po, List.map (fun (pl, pct) -> (pl, f pct)) pcts)

  let rec foldn_aux (f: int -> 'a -> ploc -> 'b prectype -> 'a) (n: int) (b: 'a): (ploc * 'b prectype) list -> 'a = function
    | []                -> b
    | (pl, pct) :: pcts -> foldn_aux f (n + 1) (f n b pl pct) pcts

  let foldn (f: int -> 'a -> ploc -> 'b prectype -> 'a) (b: 'a) ((_, pcts): 'b t): 'a =
    foldn_aux f 0 b pcts

  let mapn (f: int -> ploc -> 'a prectype -> 'b prectype) ((po, pcts): 'a t): 'b t =
    (po, foldn_aux (fun n pcts pl pct -> (pl, f n pl pct) :: pcts) 0 [] pcts |> List.rev)

  let d_ldesc (pt: unit -> 'a prectype -> P.doc) () ((po, pcts): 'a t): P.doc =
    let p = get_period_default po in
      P.seq (P.text ", ") (fun (pl, pct) -> P.dprintf "%a: %a" d_index (index_of_ploc pl p) pt pct) pcts
end

module SlocKey = struct
  type t      = sloc
  let compare = compare
end

module SLM = Map.Make (SlocKey)

type 'a prestore = ('a LDesc.t) SLM.t

let prestore_map = fun f -> f |> prectype_map |> LDesc.map |> SLM.map

let prestore_find (l: sloc) (ps: 'a prestore): 'a LDesc.t =
  try SLM.find l ps with Not_found -> LDesc.empty

type store = index prestore

module SLMPrinter = P.MakeMapPrinter(SLM)

let d_store () (s: store): P.doc =
  SLMPrinter.d_map "\n" d_sloc (LDesc.d_ldesc d_ctype) () s
