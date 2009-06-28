open Misc.Ops

type sloctype = Abstract | Concrete 

type rigidity = Free | Rigid (* Rigid slocs cannot be unified with other Rigid slocs *)

type slocid = int

type t =
    {lid             : slocid;
     lrigid          : rigidity;
     mutable lty     : sloctype;
     mutable lparent : t option}

let (fresh_slocid, reset_fresh_slocid) = Misc.mk_int_factory ()

let fresh lty lrigid =
  {lid = fresh_slocid (); lty = lty; lrigid = lrigid; lparent = None}

let rec repr (l: t): t =
  match l.lparent with
    | Some l -> repr l
    | None   -> l

let compare (l1: t) (l2: t): int =
  compare ((repr l1).lid) ((repr l2).lid)

let eq (l1: t) (l2: t): bool =
  compare l1 l2 = 0

let sloc_type (l: t): sloctype =
  (repr l).lty

let is_abstract (l: t): bool =
  (repr l).lty = Abstract

exception CantUnify of t * t

let unify (l1: t) (l2: t): unit =
  let (l1, l2) = (repr l1, repr l2) in
    if eq l1 l2 then
      ()
    else begin
      begin match (l1.lrigid, l2.lrigid) with
        | (Free, _) -> l1.lparent <- Some l2
        | (_, Free) -> l2.lparent <- Some l1
        | _         -> raise (CantUnify (l1, l2))
      end;
      (repr l2).lty     <- Abstract;
    end

let to_string (l: t): string =
  let l = repr l in
    match l.lty with
      | Abstract -> "A" ^ string_of_int l.lid
      | Concrete -> "C" ^ string_of_int l.lid

let d_sloc () (l: t): Pretty.doc =
  Pretty.text <| to_string l

(* Avoiding non-cyclic types when making SlocSet *)
type sloc = t

module SlocSet = Set.Make(struct
                            type t = sloc
                            let compare = compare
                          end)
