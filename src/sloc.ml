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

module Subst = struct
  type t = (sloc * sloc) list

  let empty = []

  let d_subst (): t -> P.doc =
    P.dprintf "[@[%a@]]" (P.d_list "," (fun () (sfrom, sto) -> P.dprintf "%a -> %a" d_sloc sfrom d_sloc sto))

  let apply (sub: t) (s: sloc): sloc =
    try List.assoc s sub with Not_found -> s

  let extend (sfrom: sloc) (sto: sloc) (sub: t): t =
    let sub  = List.map (apply [(sfrom, sto)] |> M.app_snd) sub in
      if not (List.mem_assoc sfrom sub) then
        (sfrom, sto) :: sub
      else
        sub

  let compose (sub1: t) (sub2: t): t =
    let sub = List.map (apply sub1 |> M.app_snd) sub2 in
      List.fold_left begin fun sub (sfrom, sto) ->
        if not (List.mem_assoc sfrom sub2) then
          (sfrom, sto) :: sub
        else
          sub
      end sub sub1

  let dom (sub: t): sloc list =
    List.map fst sub

  let rng (sub: t): sloc list =
    List.map snd sub

  let slocs (sub: t): sloc list =
    List.split sub |> M.uncurry (@) |> M.sort_and_compact

  let images (sub: t): sloc list list =
    sub |> M.groupby fst |> List.map (List.map snd)

  let all_slocs_eq: sloc list -> bool = function
    | []      -> true
    | s :: ss -> List.fold_left (fun all_eq s' -> all_eq && eq s s') true ss

  let well_defined (sub: t): bool =
    sub |> images |> List.for_all all_slocs_eq

  let transpose (sub: t): t =
    List.map (fun (x, y) -> (y, x)) sub
end
