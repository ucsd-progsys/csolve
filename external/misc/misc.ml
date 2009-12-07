(*
 * Copyright ? 1990-2007 The Regents of the University of California. All rights reserved. 
 *
 * Permission is hereby granted, without written agreement and without 
 * license or royalty fees, to use, copy, modify, and distribute this 
 * software and its documentation for any purpose, provided that the 
 * above copyright notice and the following two paragraphs appear in 
 * all copies of this software. 
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY 
 * FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES 
 * ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN 
 * IF THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY 
 * OF SUCH DAMAGE. 
 * 
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES, 
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY 
 * AND FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE PROVIDED HEREUNDER IS 
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION 
 * TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 *
 *)

(* $Id: misc.ml,v 1.14 2006/09/26 01:47:01 jhala Exp $
 *
 * This file is part of the SIMPLE Project.
 *)

(**
 * This module provides some miscellaneous useful helper functions.
 *)

(* Used without "Misc" prefix *)
module Ops = struct
  
  let (|>) x f = f x

  let (<|) f x = f x

  let (>>) x f = f x; x

  let (|>>) xo f = match xo with None -> None | Some x -> f x

  let (+=) x n = x := !x + n; !x

  let (++) = List.rev_append 
  
  let (+++)= fun (x1s, y1s) (x2s, y2s) -> (x1s ++ x2s, y1s ++ y2s)
  
  let id = fun x -> x

  let un = fun x -> ()

  let (<.>) f g  = fun x -> x |> g |> f 
  
  let (<+>) f g  = fun x -> x |> f |> g 
  
  let failure fmt = 
    Printf.ksprintf failwith fmt

  let asserti p fmt = 
    Printf.ksprintf (fun x -> if not p then (print_string (x^"\n"); ignore(0/0)) else ()) fmt
  
  let asserts p fmt =
    Printf.ksprintf (fun x -> if not p then failwith x) fmt

  let assertf fmt =
    Printf.ksprintf failwith fmt

  let halt _ =
    assert false

  let fst3 (x,_,_) = x
  let snd3 (_,x,_) = x
  let thd3 (_,_,x) = x

  let withfst3 (_,y,z) x = (x,y,z)
  let withsnd3 (x,_,z) y = (x,y,z)
  let withthd3 (x,y,_) z = (x,y,z)


  let print_now s = 
    print_string s; 
    flush stdout

(*  
  let pretty_string f x = 
    Pretty.dprintf "%a" f x |> Pretty.sprint ~width:80 
*)

end

open Ops

let choose b f g = if b then f else g

let liftfst2 (f: 'a -> 'a -> 'b) (x: 'a * 'c) (y: 'a * 'c): 'b =
  f (fst x) (fst y)

let curry f   = fun x y -> f (x,y)
let uncurry f = fun (x,y) -> f x y

module type KeyValType =
  sig
    type t
    val compare : t -> t -> int

    type v
    val default : v
  end

module MapWithDefault (K: KeyValType) =
  struct
    include Map.Make(K)

    let find (i: K.t) (m: K.v t): K.v =
      try find i m with Not_found -> K.default
  end

module IntMap = 
  Map.Make 
  (struct
    type t = int
    let compare i1 i2 = 
      compare i1 i2
  end)

module IntSet =
  Set.Make
  (struct
    type t = int
    let compare i1 i2 =
      compare i1 i2
  end)

module IntIntMap = 
  Map.Make 
  (struct
    type t = int * int
    let compare i1 i2 = 
      compare i1 i2
   end)

module StringMap = 
  Map.Make 
  (struct
    type t = string 
    let compare i1 i2 = compare i1 i2
  end)

module StringSet =
  Set.Make
  (struct
    type t = string
    let compare i1 i2 = compare i1 i2
  end)

let sm_extend sm1 sm2 =
  StringMap.fold StringMap.add sm2 sm1 

let sm_filter f sm = 
  StringMap.fold begin fun x y sm -> 
    if f x y then StringMap.add x y sm else sm 
  end sm StringMap.empty 

let sm_of_list kvs = 
  List.fold_left (fun sm (k,v) -> StringMap.add k v sm) StringMap.empty kvs

let sm_to_list sm = 
  StringMap.fold (fun k v acc -> (k,v)::acc) sm [] 

let sm_print_keys name sm =
  sm |> sm_to_list 
     |> List.map fst 
     |> String.concat ", "
     |> Printf.printf "%s : %s \n" name

let foldn f n b = 
  let rec foo acc i = 
    if i >= n then acc else foo (f acc i) (i+1) 
  in foo b 0 

let rec range i j = 
  if i >= j then [] else i::(range (i+1) j)

let dump s = 
  print_string s; flush stdout

let mapn f n = 
  foldn (fun acc i -> (f i) :: acc) n [] 
  |> List.rev

let chop_last = function
  | [] -> failure "ERROR: Misc.chop_last"
  | xs -> xs |> List.rev |> List.tl |> List.rev

let list_snoc xs = 
  match List.rev xs with 
  | [] -> assertf "list_snoc with empty list!"
  | h::t  -> h, List.rev t

let negfilter f xs = 
  List.fold_left (fun acc x -> if f x then acc else x::acc) [] xs 
  |> List.rev

let get_option d = function  
  | Some x -> x 
  | None   -> d

let map_partial f xs =
  List.rev 
    (List.fold_left 
      (fun acc x -> 
        match f x with 
        | None   -> acc
        | Some z -> (z::acc)) [] xs)

let fold_left_partial f b xs =
  List.fold_left begin fun b xo ->
    match xo with
      | Some x -> f b x
      | None   -> b
  end b xs

let list_reduce f = function
  | []    -> assertf "ERROR: list_reduce with empty list"
  | x::xs -> List.fold_left f x xs

let list_max x xs = 
  List.fold_left max x xs

let getf a i fmt = 
  try a.(i) with ex -> assertf fmt

let do_catchf s f x =
  try f x with ex -> 
     (Printf.printf "%s hits exn: %s \n" s (Printexc.to_string ex); 
      assert false)

let do_catch s f x =
  try f x with ex -> 
     (Printf.printf "%s hits exn: %s \n" s (Printexc.to_string ex); raise ex) 

let do_catch_ret s f x y = 
  try f x with ex -> 
     (Printf.printf "%s hits exn: %s \n" s (Printexc.to_string ex); y) 

let do_memo memo f args key = 
  try Hashtbl.find memo key with Not_found ->
    let rv = f args in
    let _ = Hashtbl.replace memo key rv in
    rv

let map_pair   = fun f (x1, x2)     -> (f x1, f x2)
let map_triple = fun f (x1, x2, x3) -> (f x1, f x2, f x3)
let app_fst    = fun f (a, b)       -> (f a, b)
let app_snd    = fun f (a, b)       -> (a, f b)
let app_fst3   = fun f (a, b, c)    -> (f a, b, c)
let app_snd3   = fun f (a, b, c)    -> (a, f b, c)
let app_thd3   = fun f (a, b, c)    -> (a, b, f c)
let pad_snd    = fun f x            -> (x, f x)
let pad_fst    = fun f y            -> (f y, y)

let twrap s f x =
  let _  = Printf.printf "calling %s \n" s in
  let rv = f x in
  let _  = Printf.printf "returned from %s \n" s in
  rv

let mapfold f b xs =
  List.fold_left begin
    fun (acc, ys) x -> let (acc', y) = f acc x in (acc', y::ys)
  end (b, []) xs
  |> app_snd List.rev 

let filter f xs = 
  List.fold_left (fun xs' x -> if f x then x::xs' else xs') [] xs
  |> List.rev

let map2 f xs ys = 
  let _ = asserti (List.length xs = List.length ys) "Misc.map2" in
  List.map2 f xs ys

let map f xs = 
  List.rev_map f xs |> List.rev

let flatten xss =
  xss
  |> List.fold_left (fun acc xs -> xs ++ acc) []
  |> List.rev

let flatsingles xss =
  xss |> List.fold_left (fun acc -> function [x] -> x::acc | _ -> assertf "flatsingles") []
      |> List.rev

let splitflatten xsyss = 
  let xss, yss = List.split xsyss in
  (flatten xss, flatten yss)

let flap f xs =
  xs |> List.rev_map f |> flatten |> List.rev

let flap_pair f = splitflatten <.> map f

let tr_rev_flatten xs =
  List.fold_left (fun x xs -> x ++ xs) [] xs

let tr_rev_flap f xs =
  List.fold_left (fun xs x -> (f x) ++ xs) [] xs

let rec fast_unflat ys = function
  | x :: xs -> fast_unflat ([x] :: ys) xs
  | [] -> ys


let rec rev_perms s = function
  | [] -> s
  | e :: es -> rev_perms 
    (tr_rev_flap (fun e -> List.rev_map (fun s -> e :: s) s) e) es 

let product = function
  | e :: es -> rev_perms (fast_unflat [] e) es
  | es -> es 

let cross_product xs ys = 
  map begin fun x ->
    map begin fun y ->
      (x,y)
    end ys
  end xs
  |> flatten

let append_pref p s =
  (p ^ "." ^ s)

let fsort f xs =
  let cmp = fun (k1,_) (k2,_) -> compare k1 k2 in
  xs |> map (fun x -> ((f x), x)) 
     |> List.sort cmp 
     |> map snd

let sort_and_compact ls =
  let rec _sorted_compact l = 
    match l with
	h1::h2::tl ->
	  let rest = _sorted_compact (h2::tl) in
	    if h1 = h2 then rest else h1::rest
      | tl -> tl
  in
    _sorted_compact (List.sort compare ls)   

let sort_and_compact xs = 
  List.sort compare xs 
  |> List.fold_left 
       (fun ys x -> match ys with
        | y::_ when x=y -> ys
        | _::_          -> x::ys
        | []            -> [x])
       [] 
  |> List.rev

let hashtbl_to_list t = 
  Hashtbl.fold (fun x y l -> (x,y)::l) t []

let hashtbl_keys t = 
  Hashtbl.fold (fun x y l -> x::l) t []
  |> sort_and_compact

let hashtbl_invert t = 
  let t' = Hashtbl.create 17 in
  hashtbl_to_list t 
  |> List.iter (fun (x,y) -> Hashtbl.replace t' y x) 
  |> fun _ -> t'


let distinct xs = 
 List.length xs = List.length (sort_and_compact xs)

(** repeats f: unit - > unit i times *)
let rec repeat_fn f i = 
  if i = 0 then ()
  else (f (); repeat_fn f (i-1))

(* chop s chopper returns ([x;y;z...]) if s = x.chopper.y.chopper ...*)
let chop s chopper = Str.split (Str.regexp chopper) s  

(* like chop only the chop is by chop+ *)
let chop_star chopper s = 
    Str.split (Str.regexp (Printf.sprintf "[%s+]" chopper)) s

let bounded_chop s chopper i = Str.bounded_split (Str.regexp chopper) s i 

let is_prefix p s = 
  Str.string_match (Str.regexp p) s 0

let is_substring s subs = 
  let reg = Str.regexp subs in
  try ignore(Str.search_forward reg s 0); true
  with Not_found -> false

let is_suffix suffix s = 
  let k = String.length suffix
  and n = String.length s in
  (n-k >= 0) && Str.string_match (Str.regexp suffix) s (n-k)

let iteri f xs =
  List.fold_left (fun i x -> f i x; i+1) 0 xs
  |> ignore

let numbered_list xs =
  xs |> List.fold_left (fun (i, acc) x -> (i+1, (i,x)::acc)) (0,[]) 
     |> snd 
     |> List.rev 

exception FalseException

let sm_protected_add fail k v sm = 
  if not (StringMap.mem k sm) then StringMap.add k v sm else 
    if not fail then sm else 
      assertf "protected_add: duplicate binding for %s \n" k

let sm_adds k v sm = 
  let vs = try StringMap.find k sm with Not_found -> [] in
  StringMap.add k (v::vs) sm

let sm_bindings sm =
  StringMap.fold (fun k v acc -> (k,v) :: acc) sm []
  
let intmap_bindings im =
  IntMap.fold (fun k v acc -> (k,v) :: acc) im []

let rec intmap_for_all f m =
  try 
    IntMap.iter (fun i v -> if not (f i v) then raise FalseException) m;
    true
  with FalseException -> false

let hashtbl_to_list_all t = 
  hashtbl_keys t |> map (Hashtbl.find_all t) 

let clone x n = 
  let rec f n xs = if n <= 0 then xs else f (n-1) (x::xs) in
  f n []

let distinct xs = 
  List.length (sort_and_compact xs) = List.length xs

let flip f x y =
  f y x

let trunc i j = 
  let (ai,aj) = (abs i, abs j) in
  if aj <= ai then j else ai*j/aj 

let map_to_string f xs = 
  String.concat "," (List.map f xs)

let suffix_of_string = fun s i -> String.sub s i (String.length s - 1)

(* [count_map xs] = fun x -> number of times x appears in xs if non-zero *)
let count_map rs =
  List.fold_left 
    (fun m r -> 
      let c = try IntMap.find r m with Not_found -> 0 in
      IntMap.add r (c+1) m)
    IntMap.empty rs

let o2s f = function
  | Some x -> "Some "^ (f x)
  | None   -> "None"

let fixpoint f x =
  let rec acf b x =
    let x', b' = f x in
    if b' then acf true x' else (x', b) in
  acf false x

let is_prefix p s = 
  let reg = Str.regexp p in
  Str.string_match reg s 0

let rec pprint_many_box s f ppf = function
  | []     -> ()
  | x::[]  -> Format.fprintf ppf "%a" f x
  | x::xs' -> (Format.fprintf ppf "%a%s@\n" f x s; 
               pprint_many_box s f ppf xs')

let rec pprint_many brk s f ppf = function
  | []     -> ()
  | x::[]  -> Format.fprintf ppf "%a" f x
  | x::xs' -> ((if brk 
                then Format.fprintf ppf "%a%s@," f x s 
                else Format.fprintf ppf "%a%s" f x s); 
                pprint_many brk s f ppf xs')

let pprint_str ppf s =
  Format.fprintf ppf "%s" s

let pprint_ints ppf is = 
  pprint_many_box ";" (fun ppf i -> Format.fprintf ppf "%d" i) ppf is

let fsprintf f p = 
  Format.fprintf Format.str_formatter "@[%a@]" f p;
  Format.flush_str_formatter ()

let rec same_length l1 l2 = match l1, l2 with
  | [], []           -> true
  | _ :: xs, _ :: ys -> same_length xs ys
  | _                -> false

let only_one s = function
    x :: [] -> Some x
  | x :: xs -> failwith s
  | [] -> None

let maybe_one = function
    [x] -> Some x
  | _ -> None


(*****************************************************************)
(******************** Mem Management *****************************)
(*****************************************************************)

open Gc
(* open Format *)

let pprint_gc s =
  (*printf "@[Gc@ Stats:@]@.";
  printf "@[minor@ words:@ %f@]@." s.minor_words;
  printf "@[promoted@ words:@ %f@]@." s.promoted_words;
  printf "@[major@ words:@ %f@]@." s.major_words;*)
  (*printf "@[total allocated:@ %fMB@]@." (floor ((s.major_words +. s.minor_words -. s.promoted_words) *. (4.0) /. (1024.0 *. 1024.0)));*)

  Format.printf "@[total allocated:@ %fMB@]@." (floor ((allocated_bytes ()) /. (1024.0 *. 1024.0)));
  Format.printf "@[minor@ collections:@ %i@]@." s.minor_collections;
  Format.printf "@[major@ collections:@ %i@]@." s.major_collections;
  Format.printf "@[heap@ size:@ %iMB@]@." (s.heap_words * 4 / (1024 * 1024));
  (*printf "@[heap@ chunks:@ %i@]@." s.heap_chunks;
  (*printf "@[live@ words:@ %i@]@." s.live_words;
  printf "@[live@ blocks:@ %i@]@." s.live_blocks;
  printf "@[free@ words:@ %i@]@." s.free_words;
  printf "@[free@ blocks:@ %i@]@." s.free_blocks;
  printf "@[largest@ free:@ %i@]@." s.largest_free;
  printf "@[fragments:@ %i@]@." s.fragments;*)*)
  Format.printf "@[compactions:@ %i@]@." s.compactions;
  (*printf "@[top@ heap@ words:@ %i@]@." s.top_heap_words*) ()

let dump_gc s =
  Format.printf "@[%s@]@." s;
  pprint_gc (Gc.quick_stat ())



let append_to_file f s = 
  let oc = Unix.openfile f [Unix.O_WRONLY; Unix.O_APPEND; Unix.O_CREAT] 420  in
  ignore (Unix.write oc s 0 ((String.length s)-1) ); 
  Unix.close oc

let write_to_file f s =
  let oc = open_out f in
  output_string oc s; 
  close_out oc

let get_unique =
  let cnt = ref 0 in
  (fun () -> let rv = !cnt in incr cnt; rv)

let flip f x y =
  f y x

let maybe_map f = function Some x -> Some (f x) | None -> None

let maybe_iter f = function Some x -> f x | None -> ()

let maybe = function Some x -> x | _ -> assertf "maybe called with None" 

let maybe_cons m xs = match m with
  | None -> xs
  | Some x -> x :: xs

let maybe_list xs = 
  List.fold_right maybe_cons xs []

let list_assoc_flip xs = 
  let r (x, y) = (y, x) in
    List.map r xs

let fold_lefti f b xs =
  List.fold_left (fun (i,b) x -> ((i+1), f i b x)) (0,b) xs

let mapi f xs = 
  xs |> fold_lefti (fun i acc x -> (f i x) :: acc) [] 
     |> snd |> List.rev

let fold_left_flip f b xs =
  List.fold_left (flip f) b xs

let fold_left_swap f xs b =
  List.fold_left f b xs

let rec map3 f xs ys zs = match (xs, ys, zs) with
  | ([], [], []) -> []
  | (x :: xs, y :: ys, z :: zs) -> f x y z :: map3 f xs ys zs
  | _ -> assert false

let zip_partition xs bs =
  let (xbs,xbs') = List.partition snd (List.combine xs bs) in
  (List.map fst xbs, List.map fst xbs')

let rec perms es =
  match es with
    | s :: [] ->
        List.map (fun c -> [c]) s
    | s :: es ->
        flap (fun c -> List.map (fun d -> c :: d) (perms es)) s
    | [] ->
        []

let flap2 f xs ys = 
  List.flatten (List.map2 f xs ys)

let flap3 f xs ys zs =
  List.flatten (map3 f xs ys zs)

let split3 lst =
  List.fold_right (fun (x, y, z) (xs, ys, zs) -> (x :: xs, y :: ys, z :: zs)) lst ([], [], [])

let split4 lst =
  List.fold_right (fun (w, x, y, z) (ws, xs, ys, zs) -> (w :: ws, x :: xs, y :: ys, z :: zs)) lst ([], [], [], [])

let combine3 xs ys zs =
  map3 (fun x y z -> (x, y, z)) xs ys zs

(* these do odd things with order for performance 
 * it is possible that fast is a misnomer *)
let fast_flatten xs =
  List.fold_left (++) [] xs

let fast_append v v' =
  let (v, v') = if List.length v > List.length v' then (v', v) else (v, v') in
  List.rev_append v v'

let fast_flap f xs =
  List.fold_left (fun xs x -> List.rev_append (f x) xs) [] xs

let rec fast_unflat ys = function
  | x :: xs -> fast_unflat ([x] :: ys) xs
  | [] -> ys

let rec rev_perms s = function
  | [] -> s
  | e :: es -> rev_perms 
    (fast_flap (fun e -> List.rev_map (fun s -> e :: s) s) e) es 

let rev_perms = function
  | e :: es -> rev_perms (fast_unflat [] e) es
  | es -> es 

let tflap2 (e1, e2) f =
  List.fold_left (fun bs b -> List.fold_left (fun aas a -> f a b :: aas) bs e1) [] e2

let tflap3 (e1, e2, e3) f =
  List.fold_left begin fun cs c -> 
    List.fold_left begin fun bs b -> 
      List.fold_left begin fun aas a -> 
        f a b c :: aas
      end bs e1
    end cs e2
  end[] e3

let rec expand f xs ys =
  match xs with
  | [] -> ys
  | x::xs ->
      let (xs',ys') = f x in
      expand f (List.rev_append xs' xs) (List.rev_append ys' ys)

let rec get_first f = function
  | x::xs when f x -> Some x 
  | _::xs          -> get_first f xs
  | []             -> None

let join f xs ys = 
  let rec fuse acc xs ys = 
    match xs, ys with 
    | [],_ | _, []                              -> List.rev acc
    | ((kx, _)::xs', (ky,_)::_  ) when kx < ky  -> fuse acc xs' ys
    | ((kx, _)::_  , (ky,_)::ys') when kx > ky  -> fuse acc xs  ys' 
    | ((kx, x)::xs', (ky,y)::ys') (* kx = ky *) -> fuse ((x,y)::acc) xs' ys' in
  let xs' = List.map (fun x -> (f x, x)) xs |> List.sort compare in
  let ys' = List.map (fun y -> (f y, y)) ys |> List.sort compare in
  fuse [] xs' ys'

let groupby (f: 'a -> 'b) (xs: 'a list): 'a list list =
  let t        = Hashtbl.create 17 in
  let lookup x = try Hashtbl.find t x with Not_found -> [] in
  (* build table *)
  List.iter begin fun x -> 
    Hashtbl.replace t (f x) (x :: lookup (f x))
  end xs;
  (* build cluster *)
  Hashtbl.fold (fun _ xs xxs -> xs :: xxs) t []
  |> List.map List.rev 

let exists_pair (f: 'a -> 'a -> bool) (xs: 'a list): bool =
  fst (List.fold_left (fun (b, ys) x -> (b || List.exists (f x) ys, x :: ys)) (false, []) xs)

let rec find_pair (f: 'a -> 'a -> bool): 'a list -> 'a * 'a = function
  | []    -> raise Not_found
  | x::xs -> try (x, List.find (f x) xs) with Not_found -> find_pair f xs

let rec is_unique = function
  | []      -> true
  | x :: xs -> if List.mem x xs then false else is_unique xs

let map_opt f = function
  | Some o -> Some (f o)
  | None -> None

let resl_opt f = function
  | Some o -> f o
  | None -> []

let resi_opt f = function
  | Some o -> f o
  | None -> ()

let opt_iter f l = 
  List.iter (resi_opt f) l

let array_to_index_list a =
  Array.fold_left (fun (i,rv) v -> (i+1,(i,v)::rv)) (0,[]) a
  |> snd
  |> List.rev

let hashtbl_of_list xys = 
  let t = Hashtbl.create 37 in
  let _ = List.iter (fun (x,y) -> Hashtbl.add t x y) xys in
  t

let array_flapi f a =
  Array.fold_left (fun (i, acc) x -> (i+1, (f i x) :: acc)) (0,[]) a
  |> snd 
  |> List.rev
  |> flatten

let array_fold_lefti f acc a =
  Array.fold_left (fun (i, acc) x -> (i + 1, f i acc x)) (0, acc) a |> snd

let array_map2 f xa ya = 
  Array.mapi (fun i x -> f x (ya.(i))) xa

exception NotForall

let array_forall f a =
  try
    Array.iter (fun e -> if f e then () else raise NotForall) a; true
  with NotForall ->
    false

let array_combine a1 a2 = 
  asserts (Array.length a1 = Array.length a2) "array_combine";
  Array.init (Array.length a1) (fun i -> (a1.(i), a2.(i)))


let compose f g a = f (g a)

let maybe_bool = function
  Some _ -> true
  | None -> false

let rec gcd (a: int) (b: int): int =
  if b = 0 then a else gcd b (a mod b)

let mk_int_factory () =
  let id = ref (-1) in
    ((fun () -> incr id; !id), (fun () -> id := -1))

let mk_char_factory () =
  let (fresh_int, reset_fresh_int) = mk_int_factory () in
    ((fun () -> Char.chr (fresh_int () + Char.code 'a')), reset_fresh_int)

let mk_string_factory s =
  let (fresh_int, reset_fresh_int) = mk_int_factory () in
    ((fun () -> s^(string_of_int (fresh_int ()))), reset_fresh_int)

let swap (x,y) = (y,x)

(* ('a * (int * 'b) list) list -> (int * ('a * 'b) list) list *)
let transpose x_iys_s = 
  let t = Hashtbl.create 17 in
  List.iter begin fun (x, iys) ->
    List.iter begin fun (i, y) -> 
      Hashtbl.add t i (x,y) 
    end iys
  end x_iys_s; 
  hashtbl_keys t |> List.map (fun i -> (i, Hashtbl.find_all t i))




let absolute_name name =
  if not (Filename.is_relative name) then name else
    let b    = Filename.basename name in
    let d    = Filename.dirname name in
    let dir  = Sys.getcwd () in
    let _    = Sys.chdir (Filename.concat dir d) in
    let dir' = Sys.getcwd () in
    let rv   = Filename.concat dir' b in
    let _    = Sys.chdir dir in
    rv


