(*
 * Copyright © 1990-2009 The Regents of the University of California. All rights reserved. 
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

module Misc = FixMisc

module M   = Misc
module P   = Pretty
module C   = Cil
module T   = CilTag
module Cs  = Constants
module E   = Errormsg
module ST  = Ssa_transform
module RA  = Refanno
module S   = Sloc
module SLM = S.SlocMap
module SS  = S.SlocSet
module CM  = CilMisc
module VM  = CM.VarMap
module SM  = M.StringMap
module FI  = FixInterface
module Sh  = Shape
module FF  = FinalFields

open Misc.Ops
open Ctypes
open ShapeInfra

module LDesc  = I.LDesc
module Store  = I.Store
module UStore = I.Store.Unify
module Ct     = I.CType
module CFun   = I.CFun
module Field  = I.Field
module CSpec  = I.Spec 
module TVarInst = IndexTypes.TVarInst 
  
let mydebug = true

(******************************************************************************)
(******************************** Environments ********************************)
(******************************************************************************)

module IM = M.IntMap

type ctvenv = ctype VM.t

type funenv = (cfun * ctvenv) VM.t

let funenv_entry_of_cfun cf =
  (cf, VM.empty)

let ctenv_of_funenv fe =
  VM.map fst fe

let funenv_of_ctenv env =
  VM.fold (fun f cf fe -> VM.add f (funenv_entry_of_cfun cf) fe) env VM.empty

type env = funenv * ctvenv

type ctvemap = I.ctemap

(******************************************************************************)
(******************************* Error Reporting ******************************)
(******************************************************************************)

let d_vartypes_long () vars =
  P.docList 
    ~sep:(P.dprintf "@!") 
    (fun (v, ct) -> P.dprintf "%s [%t]: %a" v.C.vname (fun () -> v.C.vdescr) Ct.d_ctype ct) () vars

let d_vartypes () vars =
  P.docList ~sep:(P.dprintf "@!") (fun (v, ct) -> P.dprintf "%s: %a" v.C.vname Ct.d_ctype ct) () vars

(******************************************************************************)
(***************************** Constraint Solving *****************************)
(******************************************************************************)

let _DEBUG_print_ve s ve =
  P.printf "%s START " s;
  VM.iter begin fun v ct ->
    ignore <| P.printf "[v=%s, ct=%a]" v.Cil.vname Ct.d_ctype ct
  end ve; 
  P.printf " END\n"

let constrain_lval tgr loc et tsub sub sto = function
  | (C.Var v, C.NoOffset)       -> (tsub, sub, sto)
  | (C.Mem e, C.NoOffset) as lv ->
    begin match et#ctype_of_exp loc e with
      | Ref (s, i) ->
           lv
        |> et#ctype_of_lval loc
        |> Field.create Nonfinal dummy_fieldinfo
        |> UStore.add_field sto sub tsub s i
        |> (fun (s1, s2, s3) -> (s3, s2, s1))
      | _ -> E.s <| C.bug "constraining ref lval gave back non-ref type in constrain_lval@!@!"
    end
  | lv -> E.s <| C.bug "constrain_lval got lval with offset: %a@!@!" C.d_lval lv

class exprConstraintVisitor (tgr, loc, et, fs, tsub, sub, sto) = object (self)
  inherit C.nopCilVisitor

  val sto = ref sto
  val sub = ref sub
  val tsub = ref tsub

  method get_sub_sto = (!tsub, !sub, !sto)
    
  method private lift_sub_sto (sub', sto') = (!tsub, sub', sto')

  method private set_sub_sto (tsub', sub', sto') =
    sub := sub';
    sto := sto';
    tsub := tsub'

  method vexpr e =
    self#constrain_exp e;
    C.DoChildren

  method private constrain_const e = function
    | C.CStr _   ->
      begin match et#ctype_of_exp loc e with
        | Ref (s, _) ->
             Field.create Nonfinal dummy_fieldinfo (Int (1, Index.top))
          |> UStore.add_field !sto !sub !tsub s Index.nonneg
          |> (fun (s1,s2,s3) -> (s3,s2,s1)) (* M.swap *)
          (* |> self#lift_sub_sto *)
          |> self#set_sub_sto
        | _ -> assert false
      end
    | _ -> ()

  method private constrain_addrof = function
    | (C.Var v, C.NoOffset) as lv ->
        begin match et#ctype_of_exp loc (C.AddrOf lv) with
          | FRef (f, _) -> ()
          | _ -> assert false
        end
    | _ -> assert false

  method private constrain_mem ctmem e =
    match et#ctype_of_exp loc e with
      | Ref (s, i) ->
        let sto, sub, tsub = UStore.unify_overlap !sto !sub !tsub s i in
        let s        = S.Subst.apply sub s in
          begin match s |> Store.find_or_empty sto |> LDesc.find i |>: (snd <+> Field.type_of) with
            | []   ->
              E.s <| C.error "Reading location (%a, %a) before writing data to it@!" S.d_sloc s Index.d_index i (* TODO: prettycil *)
            | [ct] ->
              if (ct, ctmem) |> M.map_pair Ct.refinement |> M.uncurry Index.is_subindex then
                let sto,sub,tsub = UStore.unify_ctype_locs sto sub tsub ctmem ct in 
                self#set_sub_sto (tsub, sub, sto)
              else
                E.s <| C.error "In-heap type %a not a subtype of expected type %a@!"
                         Ct.d_ctype ct Ct.d_ctype ctmem (* TODO: prettycil *)
            | _ -> assert false
          end
      | _ -> E.s <| C.bug "constraining mem gave back non-ref type@!"

  method private constrain_exp = function
    | C.Lval ((C.Mem e, C.NoOffset) as lv) -> self#constrain_mem (et#ctype_of_lval loc lv) e
    | C.Lval lv | C.StartOf lv             -> lv |> constrain_lval tgr loc et !tsub !sub !sto 
                                                 |> (* self#lift_sub_sto |>  *)self#set_sub_sto
    | C.Const c as e                       -> self#constrain_const e c
    | C.UnOp (uop, e, t)                   -> ()
    | C.BinOp (bop, e1, e2, t)             -> ()
    | C.CastE (C.TPtr _, C.Const c) as e   -> self#constrain_const e c
    | C.CastE (ct, e)                      -> ()
    | C.SizeOf t                           -> ()
    | C.AddrOf lv                          -> self#constrain_addrof lv
    | e                                    -> E.s <| C.error "Unimplemented constrain_exp: %a@!@!" C.d_exp e
end

let constrain_exp tgr loc et fs tsub sub sto e =
  let ecv = new exprConstraintVisitor (tgr, loc, et, fs, tsub, sub, sto) in
  let _   = C.visitCilExpr (ecv :> C.cilVisitor) e in
    ecv#get_sub_sto

let constrain_args tgr loc et fs tsub sub sto es =
  List.fold_right begin fun e (cts, tsub, sub, sto) ->
    let tsub, sub, sto = constrain_exp tgr loc et fs tsub sub sto e in
      ((et#ctype_of_exp loc e) :: cts, tsub, sub, sto)
  end es ([], tsub, sub, sto)

let unify_and_check_subtype tgr sto sub tsub e ct1 ct2 =
  let sto, sub, tsub = UStore.unify_ctype_locs sto sub tsub ct1 ct2 in
    if not (Index.is_subindex (Ct.refinement ct1) (Ct.refinement ct2)) then begin
      C.error "Expression %a has type %a, expected a subtype of %a@!"
        C.d_exp e Ct.d_ctype ct1 Ct.d_ctype ct2; (* TODO: prettycil *)
      raise (UStore.UnifyFailure (sub, sto))
    end;
    (sto, sub, tsub)
      
let check_poly_inclusion s1 s2 sub = 
  let s1vars, s2vars = Store.vars s1, Store.vars s2 in
  if List.exists (fun v -> not (List.mem v s2vars)) s1vars then begin
    C.error "Stores %a and %a do not match in their free variables@!"
      Store.d_store s1 Store.d_store s2;
      raise (UStore.UnifyFailure (sub, s1))
  end;
  ()
    
let instantiate_locs g f gl fl =
  if List.mem gl <| CFun.quantified_locs g
    && not (List.mem fl <| CFun.quantified_locs f) 
    (* && List.mem fl (f.sto_out |> Store.domain)  *)then
    Some (gl, fl)
  else
    None
      
let rec pad_names = function
  | ([], [])          -> []
  | (l::ls1, [])      -> (l, Sloc.copy_abstract [] l)::pad_names (ls1, [])
  | ([], l::ls2)      -> (Sloc.copy_abstract [] l, l)::pad_names ([], ls2)
  | (l::ls1, l'::ls2) -> (l,l')::pad_names(ls1, ls2)
    
let subs_of_fref_args sub fcts acts = 
  (* g <: f *)
    List.fold_left2 begin fun sub ct1 ct2 ->   
      match ct1, ct2 with 
        | FRef (f, _), FRef (g, r) -> 
          let gls, fls = M.map_pair CFun.ordered_locs (g,f) |> pad_names |> List.split in
          List.fold_left2 
          (fun s gl fl -> instantiate_locs g f gl fl::s) [] gls fls
        |> M.list_somes 
        |> M.flip Sloc.Subst.compose sub
        | _ -> sub
    end sub fcts acts
      
let instantiate_fref_args sub fcts acts = 
  let sub = subs_of_fref_args sub fcts acts in
  let acts = acts 
         |>: begin function 
             | FRef (f, r) -> FRef (CFun.sub_uqlocs sub f, r) 
             | ct -> ct 
         end
  in 
  (acts, sub)
  
let cond_add_annot cond a annots = if cond then a :: annots else annots
    
let constrain_app tgr i (fs, _) et cf tsub sub sto lvo args =
  let loc                 = C.get_instrLoc i in
  let cts, tsub, sub, sto = constrain_args tgr loc et fs tsub sub sto args in
  let cts = cts |>: (Ct.subs sub <.> TVarInst.apply tsub) in
  let srcinfo       = CM.srcinfo_of_instr i (Some !C.currentLoc) in
  let cfi, isub, tinsti, tinst, hsub = CFun.instantiate srcinfo cf cts sto in
  let cts, isub     = instantiate_fref_args isub (cfi.args |>: snd) cts  in
  let annot         = List.map (fun (sfrom, sto) -> RA.New (sfrom, sto)) isub
                   |> List.append (tinsti |>: fun (tfrom, tto) -> RA.TNew (tfrom, tto))
                   |> cond_add_annot (hsub <> StoreSubst.empty) (RA.HInst hsub)
                   |> cond_add_annot (tinst <> TVarInst.empty) (RA.TInst tinst)
  in
  let sto           = cfi.sto_out
                   |> Store.domain
                   |> List.fold_left Store.ensure_sloc sto in
  let sto, sub, tsub  = Store.fold_fields begin fun (sto, sub, tsub) s i fld ->
                        UStore.add_field sto sub tsub s i fld
                      end (sto, sub, tsub) cfi.sto_out in
  let sto, sub, tsub = List.fold_left2 begin fun (sto, sub, tsub) (ea, cta) (_, ctf) ->
    unify_and_check_subtype tgr sto sub tsub ea (TVarInst.apply tsub cta) ctf
  end (sto, sub, tsub) (List.combine args cts) cfi.args in
  let _ = check_poly_inclusion cfi.sto_out sto sub in
    match lvo with
      | None    -> (annot, tsub, sub, sto)
      | Some lv ->
        let ctlv     = et#ctype_of_lval loc lv in
        let tsub, sub, sto = constrain_lval tgr loc et tsub sub sto lv in
        let sto, sub, tsub = unify_and_check_subtype tgr sto sub tsub (C.Lval lv) (Ct.subs isub cf.ret) ctlv in
          (annot, tsub, sub, sto)

let constrain_return tgr loc et fs tsub sub sto rtv = function
    | None ->
      if Ct.is_void rtv then
        ([], tsub, sub, sto)
      else
        E.s <| C.error "Returning void value for non-void function\n\n"
    | Some e ->
      let tsub, sub, sto = constrain_exp tgr loc et fs tsub sub sto e in
      let sto, sub, tsub = unify_and_check_subtype tgr sto sub tsub e (et#ctype_of_exp loc e) rtv in
        ([], tsub, sub, sto)
          
let assert_store_type_correct tgr loc lv e ct = match lv with
  | (C.Mem _, _) ->
    let heap_ct = lv |> C.typeOfLval |> fresh_heaptype loc in
      if not <| Index.is_subindex (Ct.refinement ct) (Ct.refinement heap_ct) then
        E.s <| C.error "Expression %a has type %a, expected type %a\n\n"
          C.d_exp e d_ctype ct d_ctype heap_ct (* TODO: prettycil *)
  | _ -> ()

let find_function_fref loc sub ct = match Ct.subs sub ct with    
  | FRef (f, _) -> f
  | _ -> assert false

let find_function loc et fs tsub sub sto = function
  | C.Var f, C.NoOffset -> fs |> VM.find f |> fst
  | C.Mem e, C.NoOffset -> find_function_fref loc sub <| et#ctype_of_exp loc e

let unify_tvars tsub ct1 ct2 = match TVarInst.apply tsub ct1, TVarInst.apply tsub ct2 with
  | (TVar t, ct2') -> TVarInst.extend t ct2' tsub
  | _ -> tsub

let constrain_instr_aux tgr ((fs, _) as env) et (bas, tsub, sub, sto) i =
  let loc = C.get_instrLoc i  in
  let _ = C.currentLoc := loc in
  match i with
  | C.Set (lv, e, _) ->
      let tsub, sub, sto = constrain_lval tgr loc et tsub sub sto lv in
      let tsub, sub, sto = constrain_exp tgr loc et fs tsub sub sto e in
      let ct1      = et#ctype_of_lval loc lv in
      let ct2      = et#ctype_of_exp loc e in
      let tsub     = unify_tvars tsub ct1 ct2 in
      let _        = assert_store_type_correct tgr loc lv e (TVarInst.apply tsub ct2) in
      let sto, sub, tsub = UStore.unify_ctype_locs sto sub tsub ct1 ct2 in
        ([] :: bas, tsub, sub, sto)
  | C.Call (None, C.Lval (C.Var f, C.NoOffset), args, _) when CM.isVararg f.C.vtype ->
      let _ = CM.g_errorLoc !Cs.safe !C.currentLoc "constrain_instr cannot handle vararg call: %a@!@!" CM.d_var f |> CM.g_halt !Cs.safe in
      let _, tsub, sub, sto = constrain_args tgr loc et fs tsub sub sto args in
        ([] :: bas, tsub, sub, sto)
  | C.Call (lvo, C.Lval lv, args, _) ->
      let cf           = find_function loc et fs tsub sub sto lv in
      let ba, tsub, sub, sto = constrain_app tgr i env et cf tsub sub sto lvo args in
        (ba :: bas, tsub, sub, sto)
  | i -> E.s <| C.bug "Unimplemented constrain_instr: %a@!@!" C.dn_instr i

let check_sub_injective_on_locset on_error sub locs =
  let tm = ref SLM.empty in
    List.iter begin fun l ->
      let tl = S.Subst.apply sub l in
        if SLM.mem tl !tm then
          E.s <| on_error l (SLM.find tl !tm)
        else tm := SLM.add tl l !tm
    end locs

let qloc_unification_error s1 s2 =
  C.error "Separate quantified locations unified in function body (%a, %a)"
  S.d_sloc_info s1 S.d_sloc_info s2 (* TODO: prettycil, render sloc-types *)

let gloc_unification_error s1 s2 =
  C.error "Separate Global locations unified in function body (%a, %a)"
  S.d_sloc_info s1 S.d_sloc_info s2 (* TODO: prettycil, render sloc-types *)

let gloc_qloc_unification_error s1 s2 =
  C.error "Separate global location and quantified location unified in function body (%a, %a)"
  S.d_sloc_info s1 S.d_sloc_info s2 (* TODO: prettycil, render sloc-types *)

let check_locs_disjoint_under_sub sub glocs qlocs gqlocs =
  check_sub_injective_on_locset qloc_unification_error sub qlocs;
  check_sub_injective_on_locset gloc_unification_error sub glocs;
  (* If global locs are disjoint and quantified locs are disjoint,
     then a pair that's not disjoint must consist of one global loc
     and one quantified loc *)
  check_sub_injective_on_locset gloc_qloc_unification_error sub gqlocs

let constrain_instr tgr glocs qlocs gqlocs env et annots i =
  try
    let bas, tsub, sub, sto = constrain_instr_aux tgr env et annots i in
      check_locs_disjoint_under_sub sub glocs qlocs gqlocs;
      (bas, tsub, sub, sto)
  with ex -> E.s <| C.error "Exception (%s) \nFailed constraining instruction:@!%a@!@!"
               (Printexc.to_string ex) (T.d_instr_reSugar tgr) i

let constrain_instrs tgr glocs qlocs gqlocs env et is tsub sub sto =
  let bas, tsub, sub, sto = List.fold_left (constrain_instr tgr glocs qlocs gqlocs env et) ([], tsub, sub, sto) is in
    (List.rev ([] :: bas), tsub, sub, sto)

let constrain_stmt tgr glocs qlocs gqlocs ((fs, _) as env) et rtv s tsub sub sto =
  let loc = C.get_stmtLoc s.C.skind in
  let _ = C.currentLoc := loc       in
    match s.C.skind with
      | C.Instr is          -> constrain_instrs tgr glocs qlocs gqlocs env et is tsub sub sto
      | C.If (e, _, _, _)   -> let tsub, sub, sto = constrain_exp tgr loc et fs tsub sub sto e in 
                               ([], tsub, sub, sto)
      | C.Break _           -> ([], tsub, sub, sto)
      | C.Continue _        -> ([], tsub, sub, sto)
      | C.Goto _            -> ([], tsub, sub, sto)
      | C.Block _           -> ([], tsub, sub, sto)       (* we'll visit this later as we iterate through blocks *)
      | C.Loop (_, _, _, _) -> ([], tsub, sub, sto)       (* ditto *)
      | C.Return (rexp, _)  -> constrain_return tgr loc et fs tsub sub sto rtv rexp
      | _                   -> E.s <| C.bug "Unimplemented constrain_stmt: %a@!@!" C.dn_stmt s

let constrain_phi_defs tgr ve (tsub, sub, sto) (vphi, vdefs) =
  let _ = C.currentLoc := vphi.C.vdecl in
       vdefs
    |> List.fold_left begin fun (sto, sub, tsub) (_, vdef) ->
      let ct1,ct2 = VM.find vdef ve, VM.find vphi ve in
         UStore.unify_ctype_locs sto sub tsub (VM.find vdef ve) (VM.find vphi ve)
       end (sto, sub, tsub)
    |> (fun (sto, sub, tsub) -> (tsub, sub, sto))

let constrain_phis tgr ve phis tsub sub sto =
  Array.to_list phis 
  |> List.flatten 
  |> List.fold_left (constrain_phi_defs tgr ve) (tsub, sub, sto)

class exprMapVisitor et = object (self)
  inherit C.nopCilVisitor

  val em: ctemap ref = ref I.ExpMap.empty

  method get_em = !em

  method vexpr e =
    begin match e |> C.typeOf |> C.unrollType with
      | C.TFun _ -> () (* pmr: revisit - begging for an assert false here? *)
      | _        ->  
        em := I.ExpMap.add e (et#ctype_of_exp !C.currentLoc e) !em
    end;
    C.DoChildren

  method vlval lv =
    self#vexpr (C.Lval lv) |> ignore;
    C.DoChildren
end

let constrain_fun tgr gst fs cf ve sto {ST.fdec = fd; ST.phis = phis; ST.cfg = cfg} =
  let _             = C.currentLoc := fd.C.svar.C.vdecl in
  let sto, sub, tsub= List.fold_left2 begin fun (sto, sub, tsub) (_, fct) bv ->
                        let tsub = unify_tvars tsub (VM.find bv ve) fct in
                        UStore.unify_ctype_locs sto sub tsub (VM.find bv ve) fct
                      end (sto, S.Subst.empty, TVarInst.empty) cf.args fd.C.sformals in
  let tsub, sub, sto = constrain_phis tgr ve phis tsub sub sto in
  let et             = new exprTyper (ve,fs) in
  let blocks         = cfg.Ssa.blocks in
  let bas            = Array.make (Array.length blocks) [] in
  let qlocs          = CFun.quantified_locs cf in
  let glocs          = Store.domain gst in
  let gqlocs         = qlocs ++ glocs   in
  let tsub, sub, sto =
    M.array_fold_lefti begin fun i (tsub, sub, sto) b ->
      let ba, tsub, sub, sto = constrain_stmt tgr glocs qlocs gqlocs (fs, ve) et cf.ret b.Ssa.bstmt tsub sub sto in
        Array.set bas i ba;
        (tsub, sub, sto) 
    end (tsub, sub, sto) blocks
  in
  let emv = new exprMapVisitor (et) in
  let _   = C.visitCilFunction (emv :> C.cilVisitor) fd in
  (emv#get_em, bas, tsub, sub, sto)

let failure_dump sub ve sto =
  let _ = P.printf "@!Locals:@!" in
  let _ = P.printf "=======@!" in
  let _ = P.printf "%a@!" d_vartypes (ve |> VM.map (Ct.subs sub) |> VM.to_list) in
  let _ = P.printf "@!Store:@!" in
  let _ = P.printf "======@!" in
  let _ = P.printf "%a@!@!" Store.d_store (Store.subs sub sto) in
    C.error "Failed constrain_fun@!"

(* RJ: NUKING FOR BACKTRACE 
let constrain_fun fs cf ve sto sci =
  try
    constrain_fun fs cf ve sto sci
  with
    | UStore.UnifyFailure (sub, sto) -> E.s <| failure_dump sub ve sto
    | _                              -> E.s <| failure_dump S.Subst.empty ve sto
*)

(******************************************************************************)
(**************************** Local Shape Inference ***************************)
(******************************************************************************)

let all_slocs s = 
  Store.fold_fields (fun cts _ _ ct -> (Field.type_of ct)::cts) [] s
  |> Misc.map_partial Ct.sloc
  |> List.append (Store.domain s)
  |> Misc.sort_and_compact

let check_out_store_complete sto_out_formal sto_out_actual =
  let all_formal = all_slocs sto_out_formal in
     sto_out_actual
  |> Store.abstract_empty_slocs
  |> Store.fold_fields begin fun ok l i fld ->
    try
       if List.mem l all_formal && l |> Store.find sto_out_formal |> LDesc.find i = [] then begin
         ignore <| C.error "Location %a field %a: %a in actual store, but missing from spec."
                     S.d_sloc_info l 
                     Index.d_index i 
                     Field.d_field fld; (* TODO: prettycil *) 
         false
       end else
         ok
    with Not_found -> 
        C.error "Location %a not in the domain of the spec (perhaps it is a polymorphic location)."
                                    S.d_sloc_info l
      |> ignore; false
     end true
  |> fun x -> assert x
    
let check_slocs_distinct error sub slocs =
  try
    let s1, s2 = Misc.find_pair (fun s1 s2 -> M.map_pair (S.Subst.apply sub) (s1, s2) |> M.uncurry S.eq) slocs in
      halt <| C.error "%a\n\n" error (s1, s2)
  with Not_found -> ()
    
let check_tvars_distinct error tsub tvars = 
  try
    let t1, t2 = Misc.find_pair (fun t1 t2 -> M.map_pair (TVarInst.apply tsub) (TVar t1, TVar t2) |> M.uncurry (=)) tvars in
    halt <| C.error "%a\n\n" error (t1, t2)
  with Not_found -> ()
    
let partial_insts tsub t = match TVarInst.apply tsub (TVar t) with
  | TVar _ -> None
  | ct     -> Some (t, ct)

let check_tvars_uninst error tsub tvars =
  match Misc.map_partial (partial_insts tsub) tvars with 
    | [] -> ()
    | i::insts -> halt <| C.error "%a\n\n" error i
      
type soln = store * ctype VM.t * ctvemap * RA.block_annotation array

let fresh_sloc_of v = function
  | Ref (s, i) ->
      [ CM.srcinfo_of_var v (Some (v.C.vdecl))
      ; CM.srcinfo_of_type v.C.vtype (Some (v.C.vdecl))]
      |> Misc.flip S.copy_abstract s
      |> (fun s' -> Ref (s', i))
  | c          -> c

let fresh_local_slocs ve =
  VM.mapi (fun v ct -> if v.C.vglob then ct else fresh_sloc_of v ct) ve

exception LocationMismatch of S.t * LDesc.t * S.t * LDesc.t

let assert_location_inclusion l1 ld1 l2 ld2 =
  (* Polymorphism hack! *)
  if LDesc.is_empty ld2 then
    ()
  else
    LDesc.fold begin fun _ pl _ ->
      if LDesc.mem pl ld2 then () else raise (LocationMismatch (l1, ld1, l2, ld2))
    end () ld1
      
let revert_to_spec_locs sub whole_store sto em bas ve =
  let revsub = whole_store
            |> all_slocs
            |> Misc.sort_and_compact 
            |> List.fold_left (fun revsub s -> S.Subst.extend (S.Subst.apply sub s) s revsub) [] in
  let sto    = Store.subs revsub sto in
  let sub    = S.Subst.compose revsub sub in
  let ve     = VM.map (Ct.subs sub) ve in
  let em     = I.ExpMap.map (Ct.subs sub) em in
  let bas    = Array.map (RA.subs sub) bas in
  (sto, em, bas, ve)
    
let reverse_inst inst t = match TVarInst.apply inst (TVar t) with
  | TVar t' -> TVarInst.extend t' (TVar t)
  | ct -> (fun x -> x)

let inst_all_tvars cf tsub sto em ve = 
  let sto = Store.inst_tvar [] tsub sto in
  let  ve = VM.map (Ct.inst_tvar [] tsub) ve in
  let  em = I.ExpMap.map (Ct.inst_tvar [] tsub) em in
  (sto, em, ve)

let assert_call_no_physical_subtyping fe f store gst annots =
  let cf, _ = VM.find f fe in
  List.iter begin function
    | RA.New (scallee, scaller) ->
      if Store.mem cf.sto_out scallee then
        let sto = if Store.mem store scaller then store else gst in
        assert_location_inclusion
          scaller (Store.find sto scaller)
          scallee (Store.find cf.sto_out scallee)
    | _ -> ()
  end annots

let assert_no_physical_subtyping fe cfg anna sub ve store gst =
  try
    Array.iteri begin fun i b ->
      match b.Ssa.bstmt.C.skind with
        | C.Instr is ->
          List.iter2 begin fun i annots ->
            let _  = C.currentLoc := C.get_instrLoc i in
            match i with
              | C.Call (_, C.Lval (C.Var f, _), _, _) -> assert_call_no_physical_subtyping fe f store gst annots
              | _                                     -> ()
          end is anna.(i)
        | _ -> ()
    end cfg.Ssa.blocks
  with LocationMismatch (l1, ld1, l2, ld2) ->
    let _ = failure_dump sub ve store in
    let _ = flush stdout; flush stderr in
    E.s <| C.error "Location mismatch between %a and %a:\n%a \nis not included in\n%a @!@!"
        S.d_sloc_info l1 
        S.d_sloc_info l2 
        LDesc.d_sloc_ldesc (l1, ld1) 
        LDesc.d_sloc_ldesc (l2, ld2) (* TODO: prettycil *)
        
let replace_formal_tvar ct = function
  | (TVar _) as t -> t
  | _ -> ct

let ref_lookup args v = function
  | (FRef _) as t -> 
    (try List.assoc v args with Not_found -> t)
  | (Ref _) as t -> 
    (try List.assoc v args |> replace_formal_tvar t with Not_found -> t)
  | ct -> ct

(* This seems to be necessary, since we dump the formal parameters
   into the body of the function. These expressions may have a Ref type
   when they should really have a TVar type *)
let replace_formal_refs {args = args} vm =
  vm
  |> VM.to_list
  |> List.map (fun (v,t) -> (v, ref_lookup args v.Cil.vname t))
  |> VM.of_list
      
let tvar_q_error () (t1, t2) =
  C.error "Quantified variables %a and %a get unified in function body"
    d_tvar t1 d_tvar t2
    
let quant_tvar_inst_error () (t,ct) = 
  C.error "Quantified variable %a less polymorphic than expected [%a]"
    d_tvar t Ct.d_ctype ct
    
let generalize_fref = function
  | FRef (f, r) -> FRef (CFun.generalize f, r) 
  | ct -> ct
    
let generalize_frefs fdec =
  let tl = CM.top_level_fn_assgns fdec in
  VM.mapi (fun v ct -> if CM.VarSet.mem v tl then generalize_fref ct else ct)

let infer_shape tgr fe ve gst scim (cf, sci, vm) =
  let vm                    = replace_formal_refs cf vm in
  let ve                    =  VM.extend vm ve
                            |> fresh_local_slocs
                            |> generalize_frefs sci.ST.fdec in
  let sto                   = Store.upd cf.sto_out gst             in
  let em, bas, tsub, sub, sto = constrain_fun tgr gst fe cf ve sto sci in
  let _                     = C.currentLoc := sci.ST.fdec.C.svar.C.vdecl in
  let whole_store           = Store.upd cf.sto_out gst in
  let _ =                     CFun.quantified_tvars cf 
                              |> check_tvars_distinct tvar_q_error tsub in
  let _ =                     CFun.quantified_tvars cf 
                              |> check_tvars_uninst quant_tvar_inst_error tsub in
  (* let _                     = check_sol cf ve gst em bas tsub sub sto whole_store in *)
  let sto, em, ve           = inst_all_tvars cf tsub sto em ve in
  let sto, em, bas, vtyps   = revert_to_spec_locs sub whole_store sto em bas ve in
  let _                     = check_out_store_complete whole_store sto in
  let sto                   = List.fold_left Store.remove sto (Store.domain gst) in
  let vtyps                 = VM.filter (fun vi _ -> not vi.C.vglob) vtyps in 
  let annot, conca, theta   = RA.annotate_cfg sci.ST.cfg (Store.domain gst) em bas in
  let _                     = assert_no_physical_subtyping fe sci.ST.cfg annot sub ve sto gst in
  let nasa                  = NotAliased.non_aliased_locations sci.ST.cfg em conca annot in
  {Sh.vtyps   = VM.to_list vtyps; (* CM.vm_to_list vtyps; *)
   Sh.etypm   = em;
   Sh.store   = sto;
   Sh.anna    = annot;
   Sh.conca   = conca;
   Sh.theta   = theta;
   Sh.nasa    = nasa;
   Sh.ffmsa   = Array.create 0 (SLM.empty, []); (* filled in by finalFields *)}

let declared_funs cil =
  C.foldGlobals cil begin fun fs -> function
    | C.GFun (fd, _)                                      -> fd.C.svar :: fs
    | C.GVarDecl (vi, _) when C.isFunctionType vi.C.vtype -> vi :: fs
    | _                                                   -> fs
  end []

let print_shape fname cf gst {Sh.vtyps = locals; Sh.store = st; Sh.anna = annot; Sh.ffmsa = ffmsa} =
  let _ = P.printf "%s@!" fname in
  let _ = P.printf "============@!@!" in
  let _ = P.printf "Signature:@!" in
  let _ = P.printf "----------@!@!" in
  let _ = P.printf "%a@!@!" CFun.d_cfun cf in
  let _ = P.printf "Locals:@!" in
  let _ = P.printf "-------@!@!" in
  let _ = P.printf "%a@!@!" d_vartypes_long locals in
  let _ = P.printf "Store:@!" in
  let _ = P.printf "------@!@!" in
  let _ = P.printf "%a@!@!" Store.d_store st in
  let _ = P.printf "Global Store:@!" in
  let _ = P.printf "------@!@!" in
  let _ = P.printf "%a@!@!" Store.d_store gst in
  let _ = P.printf "Annotations:@!" in
  let _ = P.printf "------@!@!" in
  let _ = P.printf "%a@!@!" RA.d_block_annotation_array annot in
  let _ = P.printf "Final Fields:@!" in
  let _ = P.printf "------@!@!" in
  let _ = P.printf "%a@!@!" FinalFields.d_final_fields ffmsa in
    ()

let print_shapes spec shpm =
  let funspec, storespec = CSpec.funspec spec, CSpec.store spec in
    if !Cs.verbose_level >= Cs.ol_ctypes || !Cs.ctypes_only then
      SM.iter (fun fname shp -> print_shape fname (SM.find fname funspec |> fst) storespec shp) shpm

let _DEBUG_ADD vi ct ve = 
  let _   = _DEBUG_print_ve "DEBUG ADD: BEFORE" ve in
  let _   = P.printf "DEBUG ADD: bind %s := %a \n" vi.Cil.vname Ct.d_ctype ct in
  let ve' = VM.add vi ct ve in
  let _   = _DEBUG_print_ve "DEBUG ADD: AFTER" ve' in
  ve'


let globalVarSpec spec v loc =
  try  
    CSpec.varspec spec |> SM.find v.C.vname |> fst
  with Not_found ->
    halt <| C.errorLoc loc "Could not find spec for global var %a\n" CM.d_var v
   
let globalFunEntryEnv spec v = 
  try
    CSpec.funspec spec |> SM.find v.C.vname |> fst |> funenv_entry_of_cfun
  with Not_found ->
    halt <| C.errorLoc v.C.vdecl "Could not find spec for function %a\n" CM.d_var v

let globalVarEnv spec cil = 
  C.foldGlobals cil begin fun ve -> function
    | C.GVarDecl (vi, loc) | C.GVar (vi, _, loc) when not (C.isFunctionType vi.C.vtype) ->
       VM.add vi (globalVarSpec spec vi loc) ve
    | _ -> ve
  end VM.empty


(* API *)
let infer_shapes cil tgr spec scis =
  let ve = globalVarEnv spec cil in
  let fe = declared_funs cil 
           |>: (fun f -> (f, globalFunEntryEnv spec f))    
           |> VM.of_list in
  let xm = SM.range scis     
           |>: (fun (_,sci,_) -> (sci.ST.fdec.C.svar, sci)) 
           |> VM.of_list in
  scis |> SM.map (infer_shape tgr fe ve (CSpec.store spec) xm)
       |> FinalFields.infer_final_fields tgr spec scis 
       >> print_shapes spec

