(* Union-find for slocs, plus some bookkeeping to keep abstract
   vs. concrete locations straight *)

module IM = Misc.IntMap

open Misc.Ops

type slocindex = int

type sloctype = Abstract | Concrete

type t = ((* parent *) slocindex * (* rank *) int * sloctype) IM.t

let empty = IM.empty

let fresh_slocindex, reset_fresh_slocindex = Misc.mk_int_factory ()

let fresh_slocindex (st: sloctype) (s: t): slocindex * t =
  let si = fresh_slocindex () in
    (si, IM.add si (si, 0, st) s)

let rec ranked_find (si: slocindex) (s: t): slocindex * int * sloctype * t =
  let (pi, r, st) = IM.find si s in
    if pi = si then
      (pi, r, st, s)
    else
      let (root, r, _, s) = ranked_find pi s in
        (* pmr: should steal root's rank + 1, no? *)
        (root, r, Concrete, IM.add si (root, r, Concrete) s)

let find (si: slocindex) (s: t): slocindex * sloctype * t =
  let (pi, _, st, s) = ranked_find si s in
    (pi, st, s)

let concretize (si: slocindex) (s: t): t =
  let (p, r, _) = IM.find si s in
    IM.add si (p, r, Concrete) s

let unify (si1: slocindex) (si2: slocindex) (s: t): t =
  let (root1, r1, _, s) = ranked_find si1 s in
  let (root2, r2, _, s) = ranked_find si2 s in
  let s                 = s |> concretize root1 |> concretize root2 in
    if r1 > r2 then
      s |> IM.add si2 (root1, r2, Concrete)
    else if r1 < r2 then
      s |> IM.add si1 (root2, r1, Concrete)
    else
      s |> IM.add si2 (root1, r2, Concrete) |> IM.add root1 (root1, r1 + 1, Concrete)
