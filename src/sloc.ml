open Misc.Ops

type sloctype =
  | Abstract
  | Concrete

type slocid = int

type t =
    {lid             : slocid;
     mutable lty     : sloctype;
     mutable lparent : t option}

let (fresh_slocid, reset_fresh_slocid) = Misc.mk_int_factory ()

let create (lid: slocid) (lty: sloctype): t =
  {lid = lid; lty = lty; lparent = None}

let fresh (lty: sloctype): t =
  {lid = fresh_slocid (); lty = lty; lparent = None}

let rec repr (l: t): t =
  match l.lparent with
    | Some l -> repr l
    | None   -> l

let compare (l1: t) (l2: t): int =
  compare ((repr l1).lid) ((repr l2).lid)

let eq (l1: t) (l2: t): bool =
  compare l1 l2 = 0

let sloc_type (l: t): sloctype =
  l.lty

let is_abstract (l: t): bool =
  l.lty = Abstract

let unify (l1: t) (l2: t): unit =
  let (l1, l2) = (repr l1, repr l2) in
    l2.lty     <- Abstract;
    l1.lparent <- Some l2

let to_string (l: t): string =
  match l.lty with
    | Abstract -> "A" ^ string_of_int l.lid
    | Concrete -> "C" ^ string_of_int l.lid

let d_sloc () (l: t): Pretty.doc =
  Pretty.text <| to_string l

(*
let sloc_is_abstract: Sloc.t -> bool = function
  | ALoc _ -> true
  | CLoc _ -> false

let abstract_sloc: sloc -> sloc = function
  | ALoc l | CLoc l -> ALoc l

let (inst_sloc, reset_inst_slocs) =
  let f, g = M.mk_int_factory () in
  let f'   = function ALoc _ -> ALoc (f ()) | CLoc _ -> CLoc (f ()) in
    (f', g)

*)
