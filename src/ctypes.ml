module M = Misc
module P = Pretty
module E = Errormsg
module S = Sloc

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
  | CTRef (_)    -> 1

let prectype_sloc: 'a prectype -> S.t option = function
  | CTRef (s, _) -> Some s
  | CTInt _      -> None

let prectype_replace_sloc (s1: S.t) (s2: S.t): 'a prectype -> 'a prectype = function
  | CTRef (s3, i) when S.eq s3 s1 -> CTRef (s2, i)
  | pct                           -> pct

let prectype_subs (subs: (S.t * S.t) list) (pct: 'a prectype): 'a prectype =
  List.fold_right (M.uncurry prectype_replace_sloc) subs pct

let prectype_eq (pct1: 'a prectype) (pct2: 'a prectype): bool =
  match (pct1, pct2) with
    | (CTRef (l1, i1), CTRef (l2, i2)) -> S.eq l1 l2 && i1 = i2
    | _                                -> pct1 = pct2

type ctype = index prectype

let d_ctype () (ct: ctype): P.doc =
  d_prectype d_index () ct

exception NoLUB of ctype * ctype

let ctype_lub (t1: ctype) (t2: ctype): ctype =
  match (t1, t2) with
    | (CTInt (n1, i1), CTInt (n2, i2)) when n1 = n2    -> CTInt (n1, index_lub i1 i2)
    | (CTRef (s1, i1), CTRef (s2, i2)) when S.eq s1 s2 -> CTRef (s1, index_lub i1 i2)
    | _                                                -> raise (NoLUB (t1, t2))

let is_subctype (ct1: ctype) (ct2: ctype): bool =
  match (ct1, ct2) with
    | (CTInt (n1, i1), CTInt (n2, i2)) when n1 = n2    -> is_subindex i1 i2
    | (CTRef (s1, i1), CTRef (s2, i2)) when S.eq s1 s2 -> is_subindex i1 i2
    | _                                                -> false

let void_ctype = CTInt (0, ITop)

(******************************************************************************)
(***************************** Periodic Locations *****************************)
(******************************************************************************)

type ploc =
  | PLAt of int   (* location n *)
  | PLSeq of int  (* location n plus periodic repeats *)
  | PLEverywhere  (* location 0, plus repeats infinitely in both directions *)

let index_of_ploc (pl: ploc) (p: int) =
  match pl with
    | PLAt n       -> IInt n
    | PLSeq n      -> ISeq (n, p)
    | PLEverywhere -> ITop

let ploc_of_index: index -> ploc = function
  | IInt n      -> PLAt n
  | ISeq (n, _) -> PLSeq n
  | ITop        -> PLEverywhere
  | IBot        -> E.s <| E.bug "Can't convert IBot to ploc@!@!"

let ploc_start: ploc -> int = function
  | PLAt n | PLSeq n -> n
  | PLEverywhere     -> E.s <| E.bug "PLEverywhere has no start@!@!"

let ploc_compare (pl1: ploc) (pl2: ploc): int =
  compare (ploc_start pl1) (ploc_start pl2)

let ploc_le (pl1: ploc) (pl2: ploc): bool =
  ploc_start pl1 <= ploc_start pl2

let ploc_periodic: ploc -> bool = function
  | PLAt _       -> false
  | PLSeq _      -> true
  | PLEverywhere -> true

let ploc_contains (pl1: ploc) (pl2: ploc) (p: int): bool =
  match (pl1, pl2) with
    | (PLAt n1, PLAt n2)                               -> n1 = n2
    | (PLAt _, PLSeq _)                                -> false
    | (PLSeq n1, PLAt n2) | (PLSeq n1, PLSeq n2)       -> n1 <= n2 && (n2 - n1) mod p = 0
    | (PLEverywhere, PLEverywhere)                     -> true
    | (PLEverywhere, PLAt n) | (PLEverywhere, PLSeq n) -> n mod p = 0
    | (_, PLEverywhere)                                -> false

let ploc_contains_index (pl: ploc) (p: int) (i: index): bool =
  match (pl, i) with
    | (PLEverywhere, _)      -> true
    | (PLAt n, IInt m)       -> n = m
    | (PLSeq n, IInt m)      -> (m - n) mod p = 0
    | (PLSeq n, ISeq (m, k)) -> k mod p = 0 && n <= m && (m - n) mod p = 0
    | _                      -> false

let ploc_offset (pl: ploc) (n: int): ploc =
  match pl with
    | PLAt n'      -> PLAt (n + n')
    | PLSeq n'     -> PLSeq (n + n')
    | PLEverywhere -> E.s <| E.bug "Can't offset PLEverywhere@!@!"

let prectypes_collide (pl1: ploc) (pct1: 'a prectype) (pl2: ploc) (pct2: 'a prectype) (p: int): bool =
  match (pl1, pl2) with
    | (PLEverywhere, _) | (_, PLEverywhere) -> true
    | _                                     ->
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
  (* Two data structures for the price of one!

     The following is either:

     A) Empty
     B) A uniform LDesc, which contains one type at all locations (anchored at 0);
        the period is always the size of this single type
     C) A non-uniform LDesc, with different items at different positions, with the following invariants
     - The list always contains at least one item.
     - The period is non-negative if present.
     - If no period is present, the list contains no periodic locations.
     - The list does not contain the location PLEverywhere.
     - For all (pl1, pct1), (pl2, pct2) in the list, not (prectypes_collide pl1 pct1 pl2 pct2 period)
       (if there is no period, then pl1 and pl2 are not periodic, so the value used for the period is irrelevant).
     - All (pl, pct) in the list are sorted by pl according to ploc_le.

     The period is always positive.
  *)
  (* pmr: XXX this structure will be first against the wall when the revolution comes *)
  type 'a contents =
    | Empty
    | Uniform of 'a prectype
    | NonUniform of (ploc * 'a prectype) list

  type 'a t = (* period: *) int option * 'a contents

  let empty: 'a t = (None, Empty)

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
    | (pl2, pct2) :: pcts' as pcts -> if ploc_le pl pl2 then (pl, pct) :: pcts else (pl2, pct2) :: insert pl pct pcts'

  let add (pl: ploc) (pct: 'a prectype) ((po, cnts): 'a t): 'a t =
    match (cnts, pl) with
      | (Empty, PLEverywhere)        -> if get_period_default po = prectype_width pct then (Some (prectype_width pct), Uniform pct) else raise TypeDoesntFit
      | (Empty, _)                   -> if fits pl pct po [] then (po, NonUniform (insert pl pct [])) else raise TypeDoesntFit
      | (Uniform _, _)               -> raise TypeDoesntFit
      | (NonUniform _, PLEverywhere) -> raise TypeDoesntFit
      | (NonUniform pcts, _)         -> if fits pl pct po pcts then (po, NonUniform (insert pl pct pcts)) else raise TypeDoesntFit

  let remove (pl: ploc) ((po, cnts): 'a t): 'a t =
    match cnts with
      | Empty           -> (po, Empty)
      | Uniform pct     -> if pl = PLEverywhere then (po, Empty) else (po, Uniform pct)
      | NonUniform pcts ->
          match List.filter (fun (pl2, _) -> not (pl = pl2)) pcts with
            | []   -> (po, Empty)
            | pcts -> (po, NonUniform pcts)

  let swallow_repeats (f: 'a prectype -> 'a prectype -> 'b -> 'b) (b: 'b) (pcts: (ploc * 'a prectype) list): (ploc * 'a prectype) list * 'b =
    try
      let (pl1, pct1) = List.find (fun (pl, _) -> ploc_periodic pl) pcts in
      let (rs, us)    = List.partition (fun (pl2, _) -> not (ploc_le pl2 pl1)) pcts in
      let b           = List.fold_left (fun b (_, pct2) -> f pct1 pct2 b) b rs in
        (us, b)
    with Not_found ->
      (* No periods, so nothing to do *)
      (pcts, b)

  let shrink_period (p: int) (f: 'a prectype -> 'a prectype -> 'b -> 'b) (b: 'b) ((po, cnts): 'a t): 'a t * 'b =
    assert (p > 0);
    match cnts with
      | Empty           -> ((Some (M.gcd (get_period_default po) p), Empty), b)
      | Uniform pct     -> if p mod prectype_width pct = 0 then ((po, Uniform pct), b) else raise TypeDoesntFit
      | NonUniform pcts ->
          let p       = M.gcd (get_period_default po) p in
          let gs      = M.groupby (fun (pl, _) -> (ploc_start pl) mod p) pcts in
          let (gs, b) = List.fold_left (fun (gs, b) g -> let (g, b) = swallow_repeats f b g in (g :: gs, b)) ([], b) gs in
          let pcts    = List.sort (M.Ops.liftfst2 ploc_compare) (List.concat gs) in
            if not (M.exists_pair (fun (pl1, pct1) (pl2, pct2) -> prectypes_collide pl1 pct1 pl2 pct2 p) pcts) then
              ((Some p, NonUniform pcts), b)
            else
              (* pmr: this is not quite descriptive enough *)
              raise TypeDoesntFit

  let shrink_period_fail_on_conflict (p: int) (ld: 'a t): 'a t =
    shrink_period p (fun _ _ _ -> raise TypeDoesntFit) () ld |> fst

  let add_index (i: index) (pct: 'a prectype) (ld: 'a t): 'a t =
    match i with
      | ISeq (n, p) -> ld |> shrink_period_fail_on_conflict p |> add (PLSeq n) pct
      | ITop        -> ld |> shrink_period_fail_on_conflict (prectype_width pct) |> add PLEverywhere pct
      | _           -> add (ploc_of_index i) pct ld

  let create (icts: (index * 'a prectype) list): 'a t =
    List.fold_left (M.uncurry add_index |> M.flip) empty icts

  let find (pl1: ploc) ((po, cnts): 'a t): (ploc * 'a prectype) list =
    match cnts with
      | Empty           -> []
      | Uniform pct     -> if ploc_contains PLEverywhere pl1 (prectype_width pct) then [(PLEverywhere, pct)] else []
      | NonUniform pcts ->
          if ploc_periodic pl1 && not (Misc.maybe_bool po) then
            []
          else
            let p = get_period_default po in
              List.filter (fun (pl2, _) -> ploc_contains pl1 pl2 p || ploc_contains pl2 pl1 p) pcts

  let find_index (i: index) ((po, _) as ld: 'a t) =
    let pcts = find (ploc_of_index i) ld in
    let p    = get_period_default po in
      List.filter (fun (pl, pct) -> ploc_contains_index pl p i) pcts

  let rec foldn_aux (f: int -> 'a -> ploc -> 'b prectype -> 'a) (n: int) (b: 'a): (ploc * 'b prectype) list -> 'a = function
    | []                -> b
    | (pl, pct) :: pcts -> foldn_aux f (n + 1) (f n b pl pct) pcts

  let foldn (f: int -> 'a -> ploc -> 'b prectype -> 'a) (b: 'a) ((po, cnts): 'b t): 'a =
    match cnts with
      | Empty           -> b
      | Uniform pct     -> f 0 b PLEverywhere pct
      | NonUniform pcts -> foldn_aux f 0 b pcts

  let fold (f: 'a -> ploc -> 'b prectype -> 'a) (b: 'a) (ld: 'b t): 'a =
    foldn (fun _ b pl pct -> f b pl pct) b ld

  let mapn (f: int -> ploc -> 'a prectype -> 'b prectype) ((po, cnts): 'a t): 'b t =
    match cnts with
      | Empty           -> (po, Empty)
      | Uniform pct     -> (po, Uniform (f 0 PLEverywhere pct))
      | NonUniform pcts -> (po, NonUniform (foldn_aux (fun n pcts pl pct -> (pl, f n pl pct) :: pcts) 0 [] pcts |> List.rev))

  let map (f: 'a prectype -> 'b prectype) (ld: 'a t): 'b t =
    mapn (fun _ _ pct -> f pct) ld

  let d_ldesc (pt: unit -> 'a prectype -> P.doc) () ((po, cnts): 'a t): P.doc =
    match cnts with
      | Empty           -> P.text ""
      | Uniform pct     -> P.dprintf "*: %a" pt pct
      | NonUniform pcts ->
          let p = get_period_default po in
            P.seq (P.text ", ") (fun (pl, pct) -> P.dprintf "%a: %a" d_index (index_of_ploc pl p) pt pct) pcts
end

module SLM = Map.Make (Sloc)

type 'a prestore = ('a LDesc.t) SLM.t

let prestore_map f =
  f |> prectype_map |> LDesc.map |> SLM.map

let prestore_map_ct f =
  SLM.map (LDesc.map f)

let prestore_fold f b ps =
  SLM.fold begin fun l ld b ->
    let p = LDesc.get_period ld |> M.get_option 0 in
      LDesc.fold (fun b pl pct -> f b l (index_of_ploc pl p) pct) b ld
  end ps b

let prestore_find (l: S.t) (ps: 'a prestore): 'a LDesc.t =
  try SLM.find l ps with Not_found -> LDesc.empty

let prestore_find_index (l: S.t) (i: index) (ps: 'a prestore): 'a prectype list =
   ps |> prestore_find l |> LDesc.find_index i |> List.map snd

let prestore_subs_addrs subs ps = 
  List.fold_left begin fun ps (l1, l2) ->
    if not (SLM.mem l1 ps) then ps else
      ps |> SLM.remove l1 
         |> SLM.add l2 (SLM.find l1 ps) 
  end ps subs

let prestore_slocset (ps: 'a prestore): S.SlocSet.t =
  prestore_fold begin fun ss _ _ pct ->
    match prectype_sloc pct with
      | Some s -> S.SlocSet.add s ss
      | None   -> ss
  end S.SlocSet.empty ps

let prestore_subs (subs: (S.t * S.t) list) (ps: 'a prestore): 'a prestore =
  ps |> prestore_map_ct (prectype_subs subs) 
     |> prestore_subs_addrs subs

let prestore_upd (ps1: 'a prestore) (ps2: 'a prestore): 'a prestore =
  SLM.fold SLM.add ps2 ps1

let prestore_filter (f: S.t -> 'a LDesc.t -> bool) (ps: 'a prestore): 'a prestore * 'a prestore =
  SLM.fold begin fun l ld (ps1, ps2) ->
    if f l ld then
      (SLM.add l ld ps1, ps2)
    else (ps1, SLM.add l ld ps2)
  end ps (SLM.empty, SLM.empty)

let prestore_split (ps: 'a prestore): 'a prestore * 'a prestore =
  prestore_filter (fun l _ -> S.is_abstract l) ps

type store = index prestore

let ctype_closed (ct: ctype) (sto: store) =
  match ct with
    | CTInt _      -> true
    | CTRef (l, i) -> prestore_find_index l i sto != []

let store_closed (sto: store): bool =
  prestore_fold (fun closed _ _ ct -> closed && ctype_closed ct sto) true sto

module SLMPrinter = P.MakeMapPrinter(SLM)

let d_precstore d_i () s  =
  P.dprintf "@[[%a]@]" (SLMPrinter.d_map ";\n" S.d_sloc (LDesc.d_ldesc (d_prectype d_i))) s

let d_store () (s: store): P.doc =
  SLMPrinter.d_map "\n" S.d_sloc (LDesc.d_ldesc d_ctype) () s

let d_prestore_addrs () st =
  let slocs = SLM.fold (fun k _ acc -> k :: acc) st [] in
  Pretty.seq (Pretty.text ",") (Sloc.d_sloc ()) slocs

(******************************************************************************)
(******************************* Function Types *******************************)
(******************************************************************************)

type 'a precfun =
  { qlocs       : S.t list;                     (* generalized slocs *)
    args        : (string * 'a prectype) list;  (* arguments *)
    ret         : 'a prectype;                  (* return *)
    sto_in      : 'a prestore;                  (* in store *)
    sto_out     : 'a prestore;                  (* out store *)
  }

type cfun = index precfun

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

let precfun_slocset (pcf: 'a precfun): S.SlocSet.t =
  let args = List.map snd pcf.args in
  let sos  = pcf.ret :: args |> List.map prectype_sloc in
  let ss1  = M.fold_left_partial (S.SlocSet.add |> M.flip) S.SlocSet.empty sos in
  let ss2  = prestore_slocset pcf.sto_out in
    S.SlocSet.union ss1 ss2

let d_slocs () slocs     = P.seq (P.text ";") (S.d_sloc ()) slocs
let d_arg d_i () (x, ct) = P.dprintf "%s : %a" x (d_prectype d_i) ct
let d_args d_i () args   = P.seq (P.text ", ") (d_arg d_i ()) args

let d_precfun d_i () ft  = 
  P.dprintf "forall [%a] arg (%a) %a store_in %a store_out %a"
  d_slocs ft.qlocs
  (d_args d_i) ft.args
  (d_prectype d_i) ft.ret
  (d_precstore d_i) ft.sto_in
  (d_precstore d_i) ft.sto_out

let d_cfun () ft =
  d_precfun d_index () ft

let rename_prectype (subs: (S.t * S.t) list) (pct: 'a prectype): 'a prectype =
  List.fold_right (M.uncurry <| prectype_replace_sloc) subs pct

let rename_prestore (subs: (S.t * S.t) list) (ps: 'a prestore): 'a prestore =
  let cns = LDesc.map (rename_prectype subs) in
    SLM.fold begin fun l ld sm ->
      let l = try List.assoc l subs with Not_found -> l in
        SLM.add l (cns ld) sm
    end ps SLM.empty

let cfun_instantiate ({qlocs = ls; args = acts; ret = rcts; sto_in = sin; sto_out = sout}: 'a precfun): 'a precfun * (S.t * S.t) list =
  let subs       = List.map (fun l -> (l, S.fresh S.Abstract)) ls in
  let rename_pct = rename_prectype subs in
  let rename_ps  = rename_prestore subs in
    ({qlocs   = [];
      args    = List.map (fun (name, arg) -> (name, rename_pct arg)) acts;
      ret     = rename_pct rcts;
      sto_in  = rename_ps sin;
      sto_out = rename_ps sout},
     subs)

let cfun_well_formed (cf: cfun): bool =
     (* pmr: also need to check sto_out includes sto_in, possibly subtyping *)
     store_closed cf.sto_in
  && store_closed cf.sto_out
  && List.for_all (fun (_, ct) -> ctype_closed ct cf.sto_in) cf.args
  && match cf.ret with  (* we can return refs to uninitialized data *)
        | CTRef (l, _) -> SLM.mem l cf.sto_out
        | _            -> true

(******************************************************************************)
(******************************** Environments ********************************)
(******************************************************************************)

type ctypeenv = cfun Misc.StringMap.t

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
