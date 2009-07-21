open Misc.Ops

type sloctype = Abstract | Concrete 

type slocid = int

type t =
    {lid             : slocid;
     mutable lty     : sloctype;
     mutable lparent : t option}

let (fresh_slocid, reset_fresh_slocid) = Misc.mk_int_factory ()

let fresh lty =
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
  (repr l).lty

let is_abstract (l: t): bool =
  (repr l).lty = Abstract

let unify (l1: t) (l2: t): unit =
  let (l1, l2) = (repr l1, repr l2) in
    if eq l1 l2 then
      ()
    else begin
      l2.lty     <- Abstract;
      l1.lparent <- Some l2
    end

let to_string (l: t): string =
  let l = repr l in
    match l.lty with
      | Abstract -> "A" ^ string_of_int l.lid
      | Concrete -> "C" ^ string_of_int l.lid

let d_sloc () (l: t): Pretty.doc =
  Pretty.text <| to_string l

(* Avoiding cyclic types when making SlocSet *)
type sloc = t

module SlocSet = Set.Make(struct
                            type t = sloc
                            let compare = compare
                          end)

module SlocMap =
  struct
    type 'a t = (sloc * 'a) list
    type key  = sloc

    let empty = []

    let key_eq (k: key) ((l, _): key * 'a): bool =
      eq k l

    let is_empty (sm: 'a t): bool =
      sm = []

    let rec remove (k: key): 'a t -> 'a t = function
      | []           -> []
      | (l, v) :: sm -> if eq l k then sm else (l, v) :: remove k sm

    let add (k: key) (v: 'a) (sm: 'a t): 'a t =
      (k, v) :: remove k sm

    let find (k: key) (sm: 'a t): 'a =
      List.find (key_eq k) sm |> snd

    let mem (k: key) (sm: 'a t): bool =
      List.exists (key_eq k) sm

    let fold (f: key -> 'a -> 'b -> 'b) (sm: 'a t) (b: 'b): 'b =
      List.fold_left (fun b (k, v) -> f k v b) b sm

    let mapi (f: key -> 'a -> 'b) (sm: 'a t): 'b t =
      fold (fun k v sm -> (k, f k v) :: sm) sm [] |> List.rev

    let map (f: 'a -> 'b) (sm: 'a t): 'b t =
      mapi (fun _ v -> f v) sm

    let iter (f: key -> 'a -> unit) (sm: 'a t): unit =
      fold (fun k v _ -> f k v) sm ()
  end
