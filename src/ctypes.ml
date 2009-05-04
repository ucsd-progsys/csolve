module M = Misc

(******************************************************************************)
(*********************************** Indices **********************************)
(******************************************************************************)

type index =
  | IBot               (* empty sequence *)
  | IInt of int        (* singleton *)
  | ISeq of int * int  (* arithmetic sequence (n, m): n + mk for all k >= 0 *)

let index_lub (i1: index) (i2: index): index =
  match (i1, i2) with
    | (IBot, i) | (i, IBot)                         -> i
    | (IInt m, IInt n)                              -> if m = n then IInt m else ISeq (min n m, abs (n - m))
    | (IInt n, ISeq (m, k)) | (ISeq (m, k), IInt n) -> ISeq (min n m, M.gcd k (abs (n - m)))
    | (ISeq (n, l), ISeq (m, k))                    -> ISeq (min n m, M.gcd l (M.gcd k (abs (n - m))))

let index_plus (i1: index) (i2: index): index =
  match (i1, i2) with
    | (IBot, _) | (_, IBot)                         -> IBot
    | (IInt n, IInt m)                              -> IInt (n + m)
    | (IInt n, ISeq (m, k)) | (ISeq (m, k), IInt n) -> ISeq (n + m, k)
    | (ISeq (n1, k1), ISeq (n2, k2)) when k1 = k2   -> ISeq (n1 + n2, k1)
    | (ISeq (n1, _), ISeq (n2, _))                  -> ISeq (n1 + n2, 1)

let is_subindex (i1: index) (i2: index): bool =
  match (i1, i2) with
    | (IBot, _)                  -> true
    | (IInt n, IInt m)           -> n = m
    | (IInt n, ISeq (m, k))      -> m <= n && (n - m) mod k = 0
    | (ISeq (n, l), ISeq (m, k)) -> m <= n && k <= l && l mod k = 0 && (n - m) mod k = 0
    | _                          -> false

(******************************************************************************)
(****************************** Index Constraints *****************************)
(******************************************************************************)

type indexvar = int

type indexexp =
  | IEInt of int
  | IEVar of indexvar
  | IEPlus of indexvar * indexvar

type indexcstr =
  | ICLess of indexexp * indexvar

module IVM =
  Map.Make
    (struct
       type t      = indexvar
       let compare = compare
     end)

type indexsol = index IVM.t

let indexsol_find (iv: indexvar) (is: indexsol): index =
  try IVM.find iv is with Not_found -> IBot

let indexexp_apply (is: indexsol): indexexp -> index = function
  | IEInt n           -> IInt n
  | IEVar iv          -> indexsol_find iv is
  | IEPlus (iv1, iv2) -> index_plus (indexsol_find iv1 is) (indexsol_find iv2 is)

let refine_index (ie: indexexp) (iv: indexvar) (is: indexsol): indexsol =
  IVM.add iv (index_lub (indexexp_apply is ie) (indexsol_find iv is)) is

let indexcstr_sat (ICLess (ie, iv): indexcstr) (is: indexsol): bool =
  is_subindex (indexexp_apply is ie) (indexsol_find iv is)

(******************************************************************************)
(************************************ Types ***********************************)
(******************************************************************************)

type sloc = int (* store locations *)

type 'a prectype =
  | CTInt of int * 'a  (* fixed-width integer *)
  | CTRef of sloc * 'a (* reference *)

let prectype_width: 'a prectype -> int = function
  | CTInt (n, _) -> n
  | CTRef (_)    -> 1

let prectype_replace_sloc (s1: sloc) (s2: sloc): 'a prectype -> 'a prectype = function
  | CTRef (s3, i) when s3 = s1 -> CTRef (s2, i)
  | pct                        -> pct

type ctype = index prectype

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
(****************************** Type Constraints ******************************)
(******************************************************************************)

type ctypevar = indexvar prectype

let ctypevar_apply (is: indexsol): ctypevar -> ctype = function
  | CTInt (n, iv) -> CTInt (n, indexsol_find iv is)
  | CTRef (s, iv) -> CTRef (s, indexsol_find iv is)

type ctypecstr =
  | CTCSubtype of ctypevar * ctypevar

let ctypecstr_replace_sloc (s1: sloc) (s2: sloc) (CTCSubtype (ctv1, ctv2): ctypecstr): ctypecstr =
  CTCSubtype (prectype_replace_sloc s1 s2 ctv1, prectype_replace_sloc s1 s2 ctv2)

let refine_ctype (ctv1: ctypevar) (ctv2: ctypevar) (is: indexsol): indexsol =
  match (ctv1, ctv2) with
    | (CTInt (n1, iv1), CTInt (n2, iv2)) when n1 = n2 -> refine_index (IEVar iv1) iv2 is
    | (CTRef (s1, iv1), CTRef (s2, iv2)) when s1 = s2 -> refine_index (IEVar iv1) iv2 is
    | _                                               -> raise (NoLUB (ctypevar_apply is ctv1, ctypevar_apply is ctv2))

let equalize_ctypes (ctv1: ctypevar) (ctv2: ctypevar) (is: indexsol): indexsol =
  refine_ctype ctv2 ctv1 (refine_ctype ctv1 ctv2 is)

let ctypecstr_sat (CTCSubtype (ctv1, ctv2): ctypecstr) (is: indexsol): bool =
  is_subctype (ctypevar_apply is ctv1) (ctypevar_apply is ctv2)

(******************************************************************************)
(*********************************** Stores ***********************************)
(******************************************************************************)

type ploc =
  | PLAt of int   (* location n *)
  | PLSeq of int  (* location n plus periodic repeats *)

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
    (not (ploc_periodic pl) || M.maybe_bool po) && not (List.exists (collides_with pl pct po) pcts)

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
        (us @ [(pl1, pct1)], b)
    with Not_found ->
      (* No repeats, so nothing to do *)
      (pcts, b)

  let shrink_period (p: int) (f: 'a prectype -> 'a prectype -> 'b -> 'b) (b: 'b) ((po, pcts): 'a t): 'a t * 'b =
    let p       = M.gcd (get_period_default po) p in
    let gs      = M.groupby (fun (pl, _) -> (ploc_start pl) mod p) pcts in
    let (gs, b) = List.fold_left (fun (gs, b) g -> let (g, b) = swallow_repeats f b g in (g :: gs, b)) ([], b) gs in
    let pcts    = List.sort (M.Ops.liftfst2 ploc_compare) (List.concat gs) in
      if not (M.exists_pair (fun (pl1, pct1) (pl2, pct2) -> prectypes_collide pl1 pct1 pl2 pct2 p) pcts) then
        ((Some p, pcts), b)
      else
        assert false

  let find (pl1: ploc) ((po, pcts): 'a t): (ploc * 'a prectype) list =
    let p = get_period_default po in
      List.filter (fun (pl2, _) -> ploc_contains pl1 pl2 p || ploc_contains pl2 pl1 p) pcts

  let map (f: 'a prectype -> 'b prectype) ((po, pcts): 'a t): 'b t =
    (po, List.map (fun (pl, pct) -> (pl, f pct)) pcts)
end

module SLM =
  Map.Make
    (struct
       type t      = sloc
       let compare = compare
     end)

type 'a prestore = ('a LDesc.t) SLM.t

let prestore_find (l: sloc) (ps: 'a prestore): 'a LDesc.t =
  try SLM.find l ps with Not_found -> LDesc.empty

type store = ctype prestore

(******************************************************************************)
(****************************** Store Constraints *****************************)
(******************************************************************************)

type storecstr =
  | SCInc of sloc * indexvar * ctypevar (* (l, i, ct): (i, ct) in store(l) *)

let storecstr_replace_sloc (s1: sloc) (s2: sloc) (SCInc (s3, iv, ctv): storecstr): storecstr =
  SCInc ((if s3 = s1 then s2 else s3), iv, prectype_replace_sloc s1 s2 ctv)

type storesol = indexvar prestore

let storesol_add (l: sloc) (pl: ploc) (ctv: ctypevar) (ss: storesol): storesol =
  SLM.add l (LDesc.add pl ctv (prestore_find l ss)) ss

let refine_store (l: sloc) (iv: indexvar) (ctv: ctypevar) (is: indexsol) (ss: storesol): indexsol * storesol =
  match indexsol_find iv is with
    | IBot        -> (is, ss)
    | IInt n      ->
        let pl = PLAt n in
          begin match LDesc.find pl (prestore_find l ss) with
            | []          -> begin try (is, storesol_add l pl ctv ss) with TypeDoesntFit -> assert false end
            | [(_, ctv2)] -> (equalize_ctypes ctv ctv2 is, ss)
            | _           -> assert false
        end
    | ISeq (n, m) ->
        let (ld, is) = LDesc.shrink_period m equalize_ctypes is (prestore_find l ss) in
        let pl       = PLSeq n in
        let pcts     = LDesc.find pl ld in
        let is       = List.fold_left (fun is (_, ctv2) -> equalize_ctypes ctv ctv2 is) is pcts in
          if List.exists (fun (pl2, _) -> ploc_periodic pl2 && ploc_le pl2 pl) pcts then
            (* If this sequence is included in an existing one, there's nothing left to do *)
            (is, ss)
          else
            (* Otherwise, remove "later", overlapping elements and add this sequence.
               (Note if there's no including sequence, all the elements we found previously
                come after this one.) *)
            let ld = List.fold_left (fun ld (pl2, _) -> LDesc.remove pl2 ld) ld pcts in
            let ld = try LDesc.add pl ctv ld with TypeDoesntFit -> assert false in
              (is, SLM.add l ld ss)

let storecstr_sat (SCInc (l, iv, ctv): storecstr) (is: indexsol) (ss: storesol): bool =
  let ld = prestore_find l ss in
  let ct = ctypevar_apply is ctv in
    match indexsol_find iv is with
      | IBot        -> true
      | IInt n      ->
          begin match LDesc.find (PLAt n) ld with
            | [(_, ctv2)] -> ct = ctypevar_apply is ctv2
            | _           -> false
          end
      | ISeq (n, m) ->
          match LDesc.get_period ld with
            | None   -> false
            | Some p ->
                (* This is stricter than the formal algorithm requires - we actually
                   enforce that a sequence is represented only by a sequence element,
                   rather than by several individual elements followed by a sequence *)
                let pl = PLSeq n in
                  match LDesc.find pl ld with
                    | [(pl2, ctv2)] -> m mod p = 0 && ploc_contains pl2 pl p && ct = ctypevar_apply is ctv2
                    | _             -> false

(******************************************************************************)
(*************************** Systems of Constraints ***************************)
(******************************************************************************)

type cstr =
  | CSIndex of indexcstr
  | CSCType of ctypecstr
  | CSStore of storecstr

type store_unifier =
  | SUnify of sloc * sloc

type cstrsol = store_unifier list * indexsol * storesol

let cstr_replace_sloc (s1: sloc) (s2: sloc): cstr -> cstr = function
  | CSCType ctc -> CSCType (ctypecstr_replace_sloc s1 s2 ctc)
  | CSStore sc  -> CSStore (storecstr_replace_sloc s1 s2 sc)
  | CSIndex ic  -> CSIndex ic

let cstr_sat ((_, is, ss): cstrsol): cstr -> bool = function
  | CSIndex ic  -> indexcstr_sat ic is
  | CSCType ctc -> ctypecstr_sat ctc is
  | CSStore sc  -> storecstr_sat sc is ss

let refine ((is, ss): indexsol * storesol): cstr -> indexsol * storesol = function
  | CSIndex (ICLess (ie, iv))         -> (refine_index ie iv is, ss)
  | CSCType (CTCSubtype (ctv1, ctv2)) -> (refine_ctype ctv1 ctv2 is, ss)
  | CSStore (SCInc (l, iv, ctv))      -> refine_store l iv ctv is ss

let rec solve_rec (cs: cstr list) ((sus, is, ss) as csol: cstrsol): cstrsol =
  match (try Some (List.find (fun c -> not (cstr_sat csol c)) cs) with Not_found -> None) with
    | None   -> csol
    | Some c ->
        let (cs, sus, is, ss) =
          try
            let (is, ss) = refine (is, ss) c in
              (cs, sus, is, ss)
          with
            | NoLUB (CTRef (s1, _), CTRef (s2, _)) ->
                let cs = List.map (cstr_replace_sloc s1 s2) cs in
                let ss = SLM.map (LDesc.map (prectype_replace_sloc s1 s2)) (SLM.remove s1 ss) in
                  (cs, SUnify (s1, s2) :: sus, is, ss)
            | _ -> assert false
        in solve_rec cs (sus, is, ss)

let solve (cs: cstr list): cstrsol =
  solve_rec cs ([], IVM.empty, SLM.empty)

(******************************************************************************)
(************************************ Tests ***********************************)
(******************************************************************************)
(*
let (_, is, ss) = solve [CSIndex (ICLess (IEInt 4, 0)); CSIndex (ICLess (IEInt 2, 0))] in
  assert (IVM.find 0 is = ISeq (2, 2))

let (_, is, ss) = solve [CSIndex (ICLess (IEInt 4, 0)); CSIndex (ICLess (IEInt 3, 0))] in
  assert (IVM.find 0 is = ISeq (3, 1))

let ctv = CTInt (1, 0) in
let (_, is, ss) = solve [CSIndex (ICLess (IEInt 4, 0)); CSStore (SCInc (0, 0, ctv))] in
  assert (LDesc.find (PLAt 4) (prestore_find 0 ss) = [(PLAt 4, CTInt (1, 0))])

let ctv = CTInt (1, 0) in
let (_, is, ss) = solve [CSIndex (ICLess (IEInt 4, 0)); CSIndex (ICLess (IEInt 2, 0)); CSStore (SCInc (0, 0, ctv))] in
  assert (LDesc.find (PLAt 6) (prestore_find 0 ss) = [(PLSeq 2, CTInt (1, 0))])

let (_, is, ss) = solve [CSIndex (ICLess (IEInt 4, 0)); CSIndex (ICLess (IEInt 2, 1))] in
  assert (IVM.find 0 is = IInt 4);
  assert (IVM.find 1 is = IInt 2)

let ctv1 = CTInt (1, 0) in
let ctv2 = CTInt (1, 1) in
let (_, is, ss) = solve [CSIndex (ICLess (IEInt 4, 0));
                         CSIndex (ICLess (IEInt 2, 1));
                         CSStore (SCInc (0, 0, ctv1));
                         CSStore (SCInc (0, 0, ctv2))]
in
  assert (ctypevar_apply is ctv1 = CTInt (1, ISeq (2, 2)));
  assert (ctypevar_apply is ctv2 = CTInt (1, ISeq (2, 2)));
  assert (List.map (fun (_, ctv) -> ctypevar_apply is ctv) (LDesc.find (PLAt 6) (prestore_find 0 ss)) = [CTInt (1, ISeq (2, 2))])

let ctv1 = CTInt (1, 0) in
let ctv2 = CTInt (1, 1) in
let ctv3 = CTInt (1, 2) in
let (_, is, ss) = solve [CSIndex (ICLess (IEInt 4, 0));
                         CSIndex (ICLess (IEInt 2, 1));
                         CSStore (SCInc (0, 0, ctv1));
                         CSStore (SCInc (0, 0, ctv2));
                         CSCType (CTCSubtype (ctv2, ctv3))]
in assert (ctypevar_apply is ctv3 = CTInt (1, ISeq (2, 2)))

let (_, is, ss) = solve [CSIndex (ICLess (IEVar 0, 0))] in
  assert (indexsol_find 0 is = IBot)

let (_, is, ss) = solve [CSIndex (ICLess (IEVar 0, 0)); CSIndex (ICLess (IEInt 4, 0))] in
  assert (indexsol_find 0 is = IInt 4)

let (_, is, ss) = solve [CSIndex (ICLess (IEPlus (0, 0), 0)); CSIndex (ICLess (IEInt 4, 0))] in
  assert (indexsol_find 0 is = ISeq (4, 4))

let (su, is, ss) = solve [CSCType (CTCSubtype (CTRef (0, 0), CTRef (1, 0)))] in
  assert (su = [SUnify (0, 1)])

let (su, is, ss) = solve [CSStore (SCInc (0, 0, CTInt (1, 0)));
                          CSIndex (ICLess (IEInt 1, 0));
                          CSCType (CTCSubtype (CTRef (0, 0), CTRef (1, 0)))] in
  assert (su = [SUnify (0, 1)]);
  assert (prestore_find 0 ss = LDesc.empty);
  assert (indexsol_find 0 is = IInt 1);
  assert (List.map (fun (_, ctv) -> ctypevar_apply is ctv) (LDesc.find (PLAt 1) (prestore_find 1 ss)) != [])
*)

(******************************************************************************)
(**************************** Constraint Generation ***************************)
(******************************************************************************)

module ExpMap =
  Map.Make (struct
              type t      = (* statment id: *) int * Cil.exp
              let compare = compare
            end)

let infer_shapes (f: Cil.fundec): Cil.fundec * ctype ExpMap.t * store =
  assert false
