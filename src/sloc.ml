module P = Pretty
module M = Misc

open Misc.Ops

type sloctype = Abstract | Concrete 

type slocid = int

type t = Sloc of slocid * sloctype

let (fresh_slocid, reset_fresh_slocid) = Misc.mk_int_factory ()

let fresh lty =
  Sloc (fresh_slocid (), lty)

let none = Sloc (-1, Abstract)

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

(******************************************************************************)
(******************************* Maps Over Slocs ******************************)
(******************************************************************************)

(* Avoiding cyclic types when making SlocSet and SlocMap *)
type sloc = t

module ComparableSloc =
  struct
    type t = sloc
    let compare = compare
  end

module SlocSet = Set.Make(ComparableSloc)

module SlocMap = Map.Make(ComparableSloc)

module SMP = P.MakeMapPrinter(SlocMap)

(******************************************************************************)
(***************************** Slocs Substitutions ****************************)
(******************************************************************************)

type subst = (t * t) list

let empty_subst = []

let d_subst (): subst -> P.doc =
  P.dprintf "[@[%a@]]" (P.d_list "," (fun () (sfrom, sto) -> P.dprintf "%a -> %a" d_sloc sfrom d_sloc sto))

let subst_apply (sub: subst) (s: t): t =
  try List.assoc s sub with Not_found -> s

let subst_extend (sfrom: t) (sto: t) (sub: subst): subst =
  let sub  = List.map (subst_apply [(sfrom, sto)] |> M.app_snd) sub in
    if not (List.mem_assoc sfrom sub) then
      (sfrom, sto) :: sub
    else
      sub

let subst_compose (sub1: subst) (sub2: subst): subst =
  let sub = List.map (subst_apply sub1 |> M.app_snd) sub2 in
    List.fold_left begin fun sub (sfrom, sto) ->
      if not (List.mem_assoc sfrom sub2) then
        (sfrom, sto) :: sub
      else
        sub
    end sub sub1

let subst_dom (sub: subst): t list =
  List.map fst sub

let subst_rng (sub: subst): t list =
  List.map snd sub

let subst_slocs (sub: subst): t list =
  List.split sub |> M.uncurry (@) |> M.sort_and_compact
