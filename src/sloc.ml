open Misc.Ops

type sloctype = Abstract | Concrete 

type slocid = int

type t = Sloc of slocid * sloctype

let (fresh_slocid, reset_fresh_slocid) = Misc.mk_int_factory ()

let fresh lty =
  Sloc (fresh_slocid (), lty)

let compare (Sloc (lid1, _): t) (Sloc (lid2, _): t): int =
  compare lid1 lid2

let eq (l1: t) (l2: t): bool =
  compare l1 l2 = 0

let sloc_type (Sloc (_, lty): t): sloctype =
  lty

let is_abstract (l: t): bool =
  sloc_type l = Abstract

let unify (l1: t) (l2: t): unit =
  (* pmr: obviously this is going away *)
  failwith "Unify no longer works on immutable slocs"

let to_string (Sloc (lid, lty): t): string =
  match lty with
    | Abstract -> "A" ^ string_of_int lid
    | Concrete -> "C" ^ string_of_int lid

let d_sloc () (l: t): Pretty.doc =
  Pretty.text <| to_string l

(* Avoiding cyclic types when making SlocSet and SlocMap *)
type sloc = t

module ComparableSloc =
  struct
    type t = sloc
    let compare = compare
  end

module SlocSet = Set.Make(ComparableSloc)

module SlocMap = Map.Make(ComparableSloc)
