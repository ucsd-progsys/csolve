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

module M   = Misc
module P   = Pretty
module C   = Cil
module Cs  = Constants
module E   = Errormsg
module ST  = Ssa_transform
module RA  = Refanno
module S   = Sloc
module SLM = S.SlocMap
module CM  = CilMisc
module VM  = CM.VarMap
module SM  = M.StringMap
module FI  = FixInterface
module Sh  = Shape
module FF  = FinalFields

open Ctypes
open ShapeInfra
open M.Ops

module LDesc  = I.LDesc
module Store  = I.Store
module UStore = I.Store.Unify
module Ct     = I.CType
module CFun   = I.CFun
module Field  = I.Field
module CSpec  = I.Spec 

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

let constrain_lval et sub sto = function
  | (C.Var v, C.NoOffset)       -> (sub, sto)
  | (C.Mem e, C.NoOffset) as lv ->
    begin match et#ctype_of_exp e with
      | Ref (s, i) ->
           lv
        |> et#ctype_of_lval
        |> Field.create Nonfinal dummy_fieldinfo
        |> UStore.add_field sto sub s i
        |> M.swap
      | _ -> E.s <| C.bug "constraining ref lval gave back non-ref type in constrain_lval@!@!"
    end
  | lv -> E.s <| C.bug "constrain_lval got lval with offset: %a@!@!" C.d_lval lv

class exprConstraintVisitor (et, fs, sub, sto) = object (self)
  inherit C.nopCilVisitor

  val sto = ref sto
  val sub = ref sub

  method get_sub_sto = (!sub, !sto)

  method private set_sub_sto (sub', sto') =
    sub := sub';
    sto := sto'

  method vexpr e =
    self#constrain_exp e;
    C.DoChildren

  method private constrain_constptr e = function
    | C.CInt64 _ -> ()
    | C.CStr _   ->
      begin match et#ctype_of_exp e with
        | Ref (s, _) ->
             Field.create Nonfinal dummy_fieldinfo (Int (1, Index.top))
          |> UStore.add_field !sto !sub s Index.nonneg
          |> M.swap
          |> self#set_sub_sto
        | _ -> assert false
      end
    | c -> E.s <| C.error "Cannot cast non-zero, non-string constant %a to pointer@!@!" C.d_const c

  method private constrain_addrof = function
    | (C.Var v, C.NoOffset) as lv ->
        begin match et#ctype_of_exp (C.AddrOf lv) with
          | Ref (l, _) ->
               fst (VM.find v fs)
            |> UStore.add_fun !sto !sub l
            |> M.swap
            |> self#set_sub_sto
          | _ -> assert false
        end
    | _ -> assert false

  method private constrain_mem ctmem e =
    match et#ctype_of_exp e with
      | Ref (s, i) ->
        let sto, sub = UStore.unify_overlap !sto !sub s i in
        let s        = S.Subst.apply sub s in
          begin match s |> Store.Data.find_or_empty sto |> LDesc.find i |>: (snd <+> Field.type_of) with
            | []   ->
              E.s <| C.error "Reading location (%a, %a) before writing data to it@!" S.d_sloc s Index.d_index i
            | [ct] ->
              if (ct, ctmem) |> M.map_pair Ct.refinement |> M.uncurry Index.is_subindex then
                UStore.unify_ctype_locs sto sub ctmem ct |> M.swap |> self#set_sub_sto
              else
                E.s <| C.error "In-heap type %a not a subtype of expected type %a@!"
                         Ct.d_ctype ct Ct.d_ctype ctmem
            | _ -> assert false
          end
      | _ -> E.s <| C.bug "constraining mem gave back non-ref type@!"

  method private constrain_exp = function
    | C.Lval ((C.Mem e, C.NoOffset) as lv) -> self#constrain_mem (et#ctype_of_lval lv) e
    | C.Lval lv | C.StartOf lv             -> lv |> constrain_lval et !sub !sto |> self#set_sub_sto
    | C.Const c                            -> ()
    | C.UnOp (uop, e, t)                   -> ()
    | C.BinOp (bop, e1, e2, t)             -> ()
    | C.CastE (C.TPtr _, C.Const c) as e   -> self#constrain_constptr e c
    | C.CastE (ct, e)                      -> ()
    | C.SizeOf t                           -> ()
    | C.AddrOf lv                          -> self#constrain_addrof lv
    | e                                    -> E.s <| C.error "Unimplemented constrain_exp: %a@!@!" C.d_exp e
end

let constrain_exp et fs sub sto e =
  let ecv = new exprConstraintVisitor (et, fs, sub, sto) in
  let _   = C.visitCilExpr (ecv :> C.cilVisitor) e in
    ecv#get_sub_sto

let constrain_args et fs sub sto es =
  List.fold_right begin fun e (cts, sub, sto) ->
    let sub, sto = constrain_exp et fs sub sto e in
      (et#ctype_of_exp e :: cts, sub, sto)
  end es ([], sub, sto)

let unify_and_check_subtype sto sub ct1 ct2 =
  let sto, sub = UStore.unify_ctype_locs sto sub ct1 ct2 in
    if not (Index.is_subindex (Ct.refinement ct1) (Ct.refinement ct2)) then begin
      C.error "Expression has type %a, expected a subtype of %a@!" Ct.d_ctype ct1 Ct.d_ctype ct2;
      raise (UStore.UnifyFailure (sub, sto))
    end;
    (sto, sub)

let constrain_app i (fs, _) et cf sub sto lvo args =
  let cts, sub, sto = constrain_args et fs sub sto args in
  let cfi, isub     = CFun.instantiate (CM.srcinfo_of_instr i (Some !C.currentLoc)) cf in
  let annot         = List.map (fun (sfrom, sto) -> RA.New (sfrom, sto)) isub in
  let sto           = cfi.sto_out
                   |> Store.domain
                   |> List.filter (Store.Data.mem cfi.sto_out)
                   |> List.fold_left Store.Data.ensure_sloc sto in
  let sto, sub      = Store.Data.fold_fields begin fun (sto, sub) s i fld ->
                        UStore.add_field sto sub s i fld
                      end (sto, sub) cfi.sto_out in
  let sto, sub      = List.fold_left2 begin fun (sto, sub) cta (_, ctf) ->
                        UStore.unify_ctype_locs sto sub cta ctf
                      end (sto, sub) cts cfi.args in
    match lvo with
      | None    -> (annot, sub, sto)
      | Some lv ->
        let ctlv     = et#ctype_of_lval lv in
        let sub, sto = constrain_lval et sub sto lv in
        let sto, sub = unify_and_check_subtype sto sub (Ct.subs isub cf.ret) ctlv in
          (annot, sub, sto)

let constrain_return et fs sub sto rtv = function
    | None ->
      if Ct.is_void rtv then
        ([], sub, sto)
      else
        E.s <| C.error "Returning void value for non-void function\n\n"
    | Some e ->
      let sub, sto = constrain_exp et fs sub sto e in
      let sto, sub = UStore.unify_ctype_locs sto sub (et#ctype_of_exp e) rtv in
        ([], sub, sto)

let assert_type_is_heap_storable heap_ct ct =
  assert (Index.is_subindex (Ct.refinement ct) (Ct.refinement heap_ct))

let assert_store_type_correct lv ct = match lv with
  | (C.Mem _, _) -> assert_type_is_heap_storable (lv |> C.typeOfLval |> fresh_heaptype) ct
  | _            -> ()

let find_function et fs sub sto = function
  | C.Var f, C.NoOffset -> fs |> VM.find f |> fst
  | C.Mem e, C.NoOffset ->
      match e |> et#ctype_of_exp |> Ct.subs sub |> Ct.sloc with
        | Some l -> Store.Function.find sto l
        | None   -> assert false

let constrain_instr_aux ((fs, _) as env) et (bas, sub, sto) i =
  let _ = C.currentLoc := C.get_instrLoc i in
  match i with
  | C.Set (lv, e, _) ->
      let sub, sto = constrain_lval et sub sto lv in
      let sub, sto = constrain_exp et fs sub sto e in
      let ct1      = et#ctype_of_lval lv in
      let ct2      = et#ctype_of_exp e in
      let _        = assert_store_type_correct lv ct2 in
      let sto, sub = UStore.unify_ctype_locs sto sub ct1 ct2 in
        ([] :: bas, sub, sto)
  | C.Call (None, C.Lval (C.Var f, C.NoOffset), args, _) when CM.isVararg f.C.vtype ->
      let _ = CM.g_errorLoc !Cs.safe !C.currentLoc "constrain_instr cannot handle vararg call: %a@!@!" CM.d_var f |> CM.g_halt !Cs.safe in
      let _, sub, sto = constrain_args et fs sub sto args in
        ([] :: bas, sub, sto)
  | C.Call (lvo, C.Lval lv, args, _) ->
      let cf           = find_function et fs sub sto lv in
      let ba, sub, sto = constrain_app i env et cf sub sto lvo args in
        (ba :: bas, sub, sto)
  | i -> E.s <| C.bug "Unimplemented constrain_instr: %a@!@!" C.dn_instr i

let constrain_instr env et annots i =
  try
    constrain_instr_aux env et annots i
  with ex -> E.s <| C.error "(%s) Failed constraining instruction:@!%a@!@!"
               (Printexc.to_string ex) C.d_instr i

let constrain_instrs env et is sub sto =
  let bas, sub, sto = List.fold_left (constrain_instr env et) ([], sub, sto) is in
    (List.rev ([] :: bas), sub, sto)

let constrain_stmt ((fs, _) as env) et rtv s sub sto =
  let _ = C.currentLoc := C.get_stmtLoc s.C.skind in
    match s.C.skind with
      | C.Instr is          -> constrain_instrs env et is sub sto
      | C.If (e, _, _, _)   -> let sub, sto = constrain_exp et fs sub sto e in ([], sub, sto)
      | C.Break _           -> ([], sub, sto)
      | C.Continue _        -> ([], sub, sto)
      | C.Goto _            -> ([], sub, sto)
      | C.Block _           -> ([], sub, sto)       (* we'll visit this later as we iterate through blocks *)
      | C.Loop (_, _, _, _) -> ([], sub, sto)       (* ditto *)
      | C.Return (rexp, _)  -> constrain_return et fs sub sto rtv rexp
      | _                   -> E.s <| C.bug "Unimplemented constrain_stmt: %a@!@!" C.dn_stmt s

let constrain_phi_defs ve (sub, sto) (vphi, vdefs) =
  let _ = C.currentLoc := vphi.C.vdecl in
       vdefs
    |> List.fold_left begin fun (sto, sub) (_, vdef) ->
         UStore.unify_ctype_locs sto sub (VM.find vdef ve) (VM.find vphi ve)
       end (sto, sub)
    |> M.swap

let constrain_phis ve phis sub sto =
  Array.to_list phis |> List.flatten |> List.fold_left (constrain_phi_defs ve) (sub, sto)

class exprMapVisitor (et) = object (self)
  inherit C.nopCilVisitor

  val em: ctemap ref = ref I.ExpMap.empty

  method get_em = !em

  method vexpr e =
    begin match e |> C.typeOf |> C.unrollType with
      | C.TFun _ -> () (* pmr: revisit - begging for an assert false here? *)
      | _        -> em := I.ExpMap.add e (et#ctype_of_exp e) !em
    end;
    C.DoChildren

  method vlval lv =
    self#vexpr (C.Lval lv) |> ignore;
    C.DoChildren
end

let constrain_fun fs cf ve sto {ST.fdec = fd; ST.phis = phis; ST.cfg = cfg} =
  let _            = C.currentLoc := fd.C.svar.C.vdecl in
  let sto, sub     = List.fold_left2 begin fun (sto, sub) (_, fct) bv ->
                       UStore.unify_ctype_locs sto sub (VM.find bv ve) fct
                     end (sto, S.Subst.empty) cf.args fd.C.sformals in
  let sub, sto     = constrain_phis ve phis sub sto in
  let et           = new exprTyper (ve) in
  let blocks       = cfg.Ssa.blocks in
  let bas          = Array.make (Array.length blocks) [] in
  let sub, sto     =
    M.array_fold_lefti begin fun i (sub, sto) b ->
      let ba, sub, sto = constrain_stmt (fs, ve) et cf.ret b.Ssa.bstmt sub sto in
        Array.set bas i ba;
        (sub, sto)
    end (sub, sto) blocks
  in
  let emv = new exprMapVisitor (et) in
  let _   = C.visitCilFunction (emv :> C.cilVisitor) fd in
  (emv#get_em, bas, sub, sto)

let failure_dump sub ve sto =
  let _ = P.printf "@!Locals:@!" in
  let _ = P.printf "=======@!" in
  let _ = P.printf "%a@!" d_vartypes (ve |> CM.VarMap.map (Ct.subs sub) |> CM.vm_to_list) in
  let _ = P.printf "@!Store:@!" in
  let _ = P.printf "======@!" in
  let _ = P.printf "%a@!@!" Store.d_store (Store.subs sub sto) in
    E.s <| C.error "Failed constrain_fun@!"

let constrain_fun fs cf ve sto sci =
  try
    constrain_fun fs cf ve sto sci
  with
    | UStore.UnifyFailure (sub, sto) -> failure_dump sub ve sto
    | _                              -> failure_dump S.Subst.empty ve sto

(******************************************************************************)
(**************************** Local Shape Inference ***************************)
(******************************************************************************)

let check_out_store_complete sto_out_formal sto_out_actual =
     sto_out_actual
  |> Store.Data.fold_fields begin fun ok l i fld ->
       if Store.mem sto_out_formal l && l |> Store.Data.find sto_out_formal |> LDesc.find i = [] then begin
         C.error "Actual store has binding %a |-> %a: %a, missing from spec for %a\n\n" 
           S.d_sloc_info l Index.d_index i Field.d_field fld S.d_sloc_info l |> ignore;
         false
       end else
         ok
     end true
  |> fun x -> assert x

let check_slocs_distinct error sub slocs =
  try
    let s1, s2 = Misc.find_pair (fun s1 s2 -> M.map_pair (S.Subst.apply sub) (s1, s2) |> M.uncurry S.eq) slocs in
      halt <| C.error "%a\n\n" error (s1, s2)
  with Not_found -> ()

(* pmr: should be obsoleted by same shape check *)

let revert_spec_names subaway st =
     st
  |> Store.domain
  |> List.fold_left (fun sub s -> S.Subst.extend (S.Subst.apply subaway s) s sub) []

type soln = store * ctype VM.t * ctvemap * RA.block_annotation array

let global_alias_error () (s1, s2) =
  C.error "Global locations %a and %a get unified in function body"
  S.d_sloc_info s1 S.d_sloc_info s2

let quantification_error () (s1, s2) =
  C.error "Quantified locations %a and %a get unified in function body" 
  S.d_sloc_info s1 S.d_sloc_info s2

let global_quantification_error () (s1, s2) =
  C.error "Global and quantified locations get unified in function body (%a, %a)" 
  S.d_sloc_info s1 S.d_sloc_info s2

let unified_instantiation_error () (s1, s2) =
  C.error "Call unifies locations which are separate in callee (%a, %a)" 
  S.d_sloc_info s1 S.d_sloc_info s2

let check_sol_aux cf vars gst em bas sub sto =
  let whole_store = Store.upd cf.sto_out gst in
  let _           = check_slocs_distinct global_alias_error sub (Store.domain gst) in
  let _           = check_slocs_distinct quantification_error sub (CFun.quantified_locs cf) in
  let _           = check_slocs_distinct global_quantification_error sub (Store.domain whole_store) in
  (* We check that instantiation annotations are WF as we check calls in consVisitor *)
  let revsub      = revert_spec_names sub whole_store in
  let sto         = Store.subs revsub sto in
  let sub         = S.Subst.compose revsub sub in
  let _           = check_out_store_complete whole_store sto in
    (sub, List.fold_left Store.remove sto (Store.domain gst))

let check_sol cf vars gst em bas sub sto =
  try check_sol_aux cf vars gst em bas sub sto with e ->
       halt
    <| C.error "Failed checking store typing:\nStore:\n%a\n\ndoesn't match expected type:\n\n%a\n\nGlobal store:\n\n%a\n\n"
        Store.d_store sto
        CFun.d_cfun cf
        Store.d_store gst

let fresh_sloc_of = function
  | Ref (s, i) -> Ref (s |> S.to_slocinfo |> S.fresh_abstract, i)
  | c          -> c

let fresh_local_slocs ve =
  VM.mapi (fun v ct -> if v.C.vglob then ct else fresh_sloc_of ct) ve

exception LocationMismatch of S.t * LDesc.t * S.t * LDesc.t

let assert_location_inclusion l1 ld1 l2 ld2 =
  (* Polymorphism hack! *)
  if LDesc.is_empty ld2 then
    ()
  else
    LDesc.fold begin fun _ pl _ ->
      if LDesc.mem pl ld2 then () else raise (LocationMismatch (l1, ld1, l2, ld2))
    end () ld1

let assert_call_no_physical_subtyping fe f store gst annots =
  let cf, _ = VM.find f fe in
    List.iter begin function
      | RA.New (scallee, scaller) ->
          let sto = if Store.mem store scaller then store else gst in
            if not (Store.Function.mem sto scaller) then
              assert_location_inclusion
                scaller (Store.Data.find sto scaller)
                scallee (Store.Data.find cf.sto_out scallee)
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
      E.s <| C.error "Location mismatch:\n%a |-> %a\nis not included in\n%a |-> %a\n"
               S.d_sloc_info l1 LDesc.d_ldesc ld1 S.d_sloc_info l2 LDesc.d_ldesc ld2

let infer_shape fe ve gst scim (cf, sci, vm) =
  let ve                    = vm |> CM.vm_union ve |> fresh_local_slocs in
  let sto                   = Store.upd cf.sto_out gst in
  let em, bas, sub, sto     = constrain_fun fe cf ve sto sci in
  let _                     = C.currentLoc := sci.ST.fdec.C.svar.C.vdecl in
  let sub, sto              = check_sol cf ve gst em bas sub sto in
  let vtyps                 = VM.map (Ct.subs sub) ve in
  let vtyps                 = VM.fold (fun vi vt vtyps -> if vi.C.vglob then vtyps else VM.add vi vt vtyps) vtyps VM.empty in
  let em                    = I.ExpMap.map (Ct.subs sub) em in
  let bas                   = Array.map (RA.subs sub) bas in
  let annot, conca, theta   = RA.annotate_cfg sci.ST.cfg (Store.domain gst) em bas in
  let _                     = assert_no_physical_subtyping fe sci.ST.cfg annot sub ve sto gst in
  let nasa                  = NotAliased.non_aliased_locations sci.ST.cfg em conca annot in
    {Sh.vtyps   = CM.vm_to_list vtyps;
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
  let _ = P.printf "%a@!@!" d_vartypes locals in
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

(* API *)
let infer_shapes cil spec scis =
  let ve = C.foldGlobals cil begin fun ve -> function
             | C.GVarDecl (vi, loc) | C.GVar (vi, _, loc) when not (C.isFunctionType vi.C.vtype) ->
                begin try
                     spec
                  |> CSpec.varspec
                  |> SM.find vi.C.vname
                  |> fst
                  |> Misc.flip (VM.add vi) ve
                with Not_found ->
                  halt <| C.errorLoc loc "Could not find spec for global var %a\n" CM.d_var vi
                 end
             | _ -> ve
           end VM.empty
  in
  let fe = declared_funs cil
           |> List.map (fun f -> (f, spec |> CSpec.funspec |> SM.find f.C.vname |> fst))
           |> List.fold_left (fun fe (f, cf) -> VM.add f (funenv_entry_of_cfun cf) fe) VM.empty in
  let xm = SM.fold (fun _ (_, sci, _) xm -> VM.add sci.ST.fdec.C.svar sci xm) scis VM.empty in
  scis
  |> SM.map (infer_shape fe ve (CSpec.store spec) xm)
  |> FinalFields.infer_final_fields spec scis 
  >> print_shapes spec
