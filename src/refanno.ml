module LM = Ctypes.SLM
module SM = Misc.StringMap

open Cil
open Misc.Ops

type ctab = (string, Ctypes.sloc) Hashtbl.t

type annotation = 
  | Gen  of Ctypes.sloc * Ctypes.sloc      (* CLoc s, ALoc s' *)
  | Inst of Ctypes.sloc * Ctypes.sloc      (* ALoc s', CLoc s *)

type block_annotation = annotation list list

let fresh_cloc =
  let t, _ = Misc.mk_int_factory () in
  (fun () -> Ctypes.CLoc (t()))

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
          | (l1, l2) when l1 = l2 -> l1
          | _ -> None)
    | CastE (_, e) -> loc_rec e
    | _ -> None in
  loc_rec

let sloc_of_expr ctm e =
  match Inferctypes.ExpMap.find e ctm with
  | Ctypes.CTInt _ -> None
  | Ctypes.CTRef (s, _) -> Some s

let instantiate conc al cl =
  if LM.mem al conc then
    let cl' = LM.find al conc in
    if cl  = cl' then 
      (conc, []) 
    else 
      (LM.add al cl conc, [Gen (cl', al); Inst (al, cl)])
  else
    (LM.add al cl conc, [Inst (al, cl)])

let annotate_instr ctm theta conc = function
    (* v1 := *v2 *)
  | Cil.Set ((Var v1, _), Lval (Mem (Lval (Var v2, _) as e), _), _) ->
      instantiate conc (Misc.maybe (sloc_of_expr ctm e)) (cloc_of_v theta v2)
    (* v := e *)
  | Cil.Set ((Var v, _), e, _) ->
      let _ = match loc_of_var_expr theta e with
              | Some c -> Hashtbl.replace theta v.vname c
              | None -> () in
      (conc, []) 
    (* *v := _ *)
  | Cil.Set ((Mem (Lval(Var v, _) as e), _), _, _) ->
      instantiate conc (Misc.maybe (sloc_of_expr ctm e)) (cloc_of_v theta v)
  | Cil.Call (_,_,_,_) ->
      if !Constants.dropcalls then (conc, []) else
        assertf "TBD: annotate_instr -- calls"
  | instr -> 
      Errormsg.error "annotate_instr: %a" Cil.d_instr instr;
      assertf "annotate_instr: unknown instr"
      (* if !Constants.safe then assertf "annotate_instr" else (conc, []) *)

let annotate_end conc =
  LM.fold (fun al cl anns -> (Gen (cl, al)) :: anns) conc []

let annotate_block ctm theta instrs : block_annotation = 
  let conc, anns = 
    List.fold_left begin fun (conc, anns) instr -> 
      let conc', ann = annotate_instr ctm theta conc instr in
      (conc', ann::anns)
    end (LM.empty, []) instrs in
  let gens = annotate_end conc in
  List.rev (gens :: anns)

(*****************************************************************************)
(********************************** API **************************************)
(*****************************************************************************)

(* API *)
let annotate_cfg cfg ctm  =
  let theta  = Hashtbl.create 17 in
  let annota =
    Array.mapi begin fun i b -> 
      match b.Ssa.bstmt.skind with
      | Instr is -> annotate_block ctm theta is
      | _ -> []
    end cfg.Ssa.blocks in
  (annota, theta)

(* API *)
let cloc_of_varinfo theta v = 
  try 
    match Hashtbl.find theta v.vname with 
    | Ctypes.CLoc _ as l -> l 
    | Ctypes.ALoc _      -> assertf "cloc_of_varinfo: absloc! (%s)" v.vname
  with Not_found ->
    assertf "cloc_of_varinfo: unknown (%s)" v.vname


(*****************************************************************************)
(********************** Pretty Printing **************************************)
(*****************************************************************************)

let d_annotation () = function
  | Gen (cl, al) -> 
      Pretty.dprintf "Generalize(%a->%a) " Ctypes.d_sloc cl Ctypes.d_sloc al 
  | Inst (al, cl) -> 
      Pretty.dprintf "Instantiate(%a->%a) " Ctypes.d_sloc al Ctypes.d_sloc cl 

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
     (fun (vn, cl) -> Pretty.dprintf "[Theta(%s) = %a" vn Ctypes.d_sloc cl) 
     vcls
