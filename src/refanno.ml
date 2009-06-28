module LM = Ctypes.SLM
module SM = Misc.StringMap

open Cil
open Misc.Ops

type ctab = (string, Sloc.t) Hashtbl.t

(** Gen: Generalize a concrete location into an abstract location
 *  Ins: Instantiate an abstract location with a concrete location
 *  New: Call-site instantiation of abs-loc-var with abs-loc-param
 *  NewC: Call-site instantiation of conc-loc-var NewC = (New;Ins) 
 NewC is an artifact of the fact that Sinfer only creates abstract locations *)
type annotation = 
  | Gen  of Sloc.t * Sloc.t             (* CLoc, ALoc *)
  | Ins  of Sloc.t * Sloc.t             (* ALoc, CLoc *)
  | New  of Sloc.t * Sloc.t             (* Xloc, Yloc *) 
  | NewC of Sloc.t * Sloc.t * Sloc.t    (* XLoc, Aloc, CLoc *) 



type block_annotation = annotation list list

let fresh_cloc () =
  Sloc.fresh Sloc.Concrete Sloc.Free

let cloc_of_v theta v =
  Misc.do_memo theta fresh_cloc () v.vname

let loc_of_var_expr theta =
  let rec loc_rec = function
    | Lval (Var v, _) ->
        Some (cloc_of_v theta v)
    | BinOp (_, e1, e2, _) ->
        let l1 = loc_rec e1 in
        let l2 = loc_rec e2 in
        (match (l1, l2) with
          | (None, l) | (l, None) -> l
          | (Some l1, Some l2) when Sloc.eq l1 l2 -> Some l1
          | _ -> None)
    | CastE (_, e) -> loc_rec e
    | _ -> None in
  loc_rec

let sloc_of_expr ctm e =
  match Ctypes.ExpMap.find e ctm with
  | Ctypes.CTInt _      -> None
  | Ctypes.CTRef (s, _) -> Some s

let cloc_of_aloc conc al =
  try LM.find al conc with Not_found -> al

let instantiate f conc al cl =
  let cl' = cloc_of_aloc conc al in
  if Sloc.eq cl' al then 
    (LM.add al cl conc, [f (al, cl)])
  else if Sloc.eq cl' cl then
    (conc, [])
  else 
    (LM.add al cl conc, [Gen (cl', al); f (al, cl)])

let annotate_set ctm theta conc = function
  (* v1 := *v2 *)
  | (Var v1, _), Lval (Mem (Lval (Var v2, _) as e), _) 
  | (Var v1, _), Lval (Mem (CastE (_, Lval (Var v2, _)) as e), _) ->
      let al = sloc_of_expr ctm e |> Misc.maybe in
      let cl = cloc_of_v theta v2 in
      instantiate (fun (x,y) -> Ins (x,y)) conc al cl 
  
  (* v := e *)
  | (Var v, _), e ->
      let _ = CilMisc.check_pure_expr e in
      loc_of_var_expr theta e
      |> Misc.maybe_iter (Hashtbl.replace theta v.vname) 
      |> fun _ -> (conc, [])

  (* *v := _ *)
  | (Mem (Lval (Var v, _) as e), _), _ 
  | (Mem (CastE (_, Lval (Var v, _)) as e), _), _ ->
      let al = sloc_of_expr ctm e |> Misc.maybe in
      let cl = cloc_of_v theta v in
      instantiate (fun (x,y) -> Ins (x,y)) conc al cl   
  
  (* Uh, oh! *)
  | lv, e -> 
      Errormsg.error "annotate_set: lv = %a, e = %a" Cil.d_lval lv Cil.d_exp e;
      assertf "annotate_set: unknown set"

let concretize_new conc = function
  | New (x,y) -> 
      if Sloc.is_abstract x then 
        let y' = cloc_of_aloc conc y in
        if Sloc.eq y y' then 
          (conc, [New (x,y)])
        else
          (LM.remove y conc, [Gen (y', y); New (x, y)]) 
      else
        let _  = asserts (Sloc.is_abstract y) "concretize_new" in
        let cl = Sloc.fresh Sloc.Concrete Sloc.Free in
        instantiate (fun (y,cl) -> NewC (x,y,cl)) conc y cl
  | _ -> assertf "concretize_new 2"

let rec new_cloc_of_aloc al = function
  | NewC (_,al',cl) :: ns when Sloc.eq al al' -> Some cl
  | _ :: ns -> new_cloc_of_aloc al ns
  | []  -> None

let sloc_of_ret ctm theta (conc, anns) = function 
  | (Var v, NoOffset) as lv ->
      Lval lv
      |>  sloc_of_expr ctm 
      |>> fun al -> new_cloc_of_aloc al anns
      |>> fun cl -> Hashtbl.replace theta v.vname cl; None
      |>> fun _  -> None 
  | _ ->
      assertf "sloc_of_ret"

let annotate_instr ctm theta conc = function
  | ns, Cil.Call (lvo,_,_,_) ->
      let conc, anns = Misc.mapfold concretize_new conc ns in
      let conc_anns  = (conc, Misc.flatten anns) in
      let _          = lvo |>> sloc_of_ret ctm theta conc_anns in
      conc_anns

  | _, Cil.Set (lv, e, _) -> 
      annotate_set ctm theta conc (lv, e)

  | _, instr ->
      Errormsg.error "annotate_instr: %a" Cil.d_instr instr;
      assertf "TBD: annotate_instr"

let annotate_end conc =
  LM.fold (fun al cl anns -> (Gen (cl, al)) :: anns) conc []

let annotate_block ctm theta anns instrs = 
  let _ = asserts (List.length anns = 1 + List.length instrs) "annotate_block"
  in
  let ainstrs = List.combine (Misc.chop_last anns) instrs in
  let conc, anns' =
    List.fold_left begin fun (conc, anns') ainstr -> 
      let conc', ann = annotate_instr ctm theta conc ainstr in
      (conc', ann::anns')
    end (LM.empty, []) ainstrs in
  let gens = annotate_end conc in
  List.rev (gens :: anns')

(*****************************************************************************)
(********************************** API **************************************)
(*****************************************************************************)

(* API *)
let annotate_cfg cfg ctm anna  =
  let theta  = Hashtbl.create 17 in
  let annota =
    Array.mapi begin fun i b -> 
      match b.Ssa.bstmt.skind with
      | Instr is -> annotate_block ctm theta anna.(i) is
      | _ -> []
    end cfg.Ssa.blocks in
  (annota, theta)

(* API *)
let cloc_of_varinfo theta v =
  try
    let l = Hashtbl.find theta v.vname in
      if not (Sloc.is_abstract l) then
        Some l
      else
        assertf "cloc_of_varinfo: absloc! (%s)" v.vname
  with Not_found -> None


(* API *)
let merge_annots a1 a2 = 
  let _ = asserts (Array.length a1 = Array.length a2) "merge_annots 1" in
  Misc.array_map2 begin fun anns anns' -> 
    let _ = asserts (List.length anns = List.length anns') "merge_annots 2" in
    List.map2 (++) anns anns'
  end a1 a2


(*****************************************************************************)
(********************** Pretty Printing **************************************)
(*****************************************************************************)

let d_annotation () = function
  | Gen (cl, al) -> 
      Pretty.dprintf "Generalize(%a->%a) " Sloc.d_sloc cl Sloc.d_sloc al 
  | Ins (al, cl) -> 
      Pretty.dprintf "Instantiate(%a->%a) " Sloc.d_sloc al Sloc.d_sloc cl 
  | New (al, cl) -> 
      Pretty.dprintf "New(%a->%a) " Sloc.d_sloc al Sloc.d_sloc cl 
  | NewC (cl, al, cl') -> 
      Pretty.dprintf "NewC(%a->%a->%a) " Sloc.d_sloc cl Sloc.d_sloc al Sloc.d_sloc cl'




let d_annotations () anns = 
  Pretty.seq (Pretty.text ", ") 
    (fun ann -> Pretty.dprintf "%a" d_annotation ann) 
    anns

let d_block_annotation () annss =
  Misc.numbered_list annss
  |> Pretty.d_list "\n" (fun () (i,x) -> Pretty.dprintf "%i: %a" i d_annotations x) ()

(* API *)
let d_block_annotation_array =
  Pretty.docArray 
    ~sep:(Pretty.text "\n")
    (fun i x -> Pretty.dprintf "block %i: @[%a@]" i d_block_annotation x) 

(* API *)
let d_ctab () t = 
  let vcls = Misc.hashtbl_to_list t in
  Pretty.seq (Pretty.text ", ") 
     (fun (vn, cl) -> Pretty.dprintf "[Theta(%s) = %a" vn Sloc.d_sloc cl) 
     vcls
