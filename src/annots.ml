(*
 * Copyright © 1990-2011 The Regents of the University of California. All rights reserved. 
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

(* This file is part of the CSolve Project.*)
module H   = Hashtbl
module FA  = FixAstInterface
module Ct  = Ctypes
module Co  = Constants
module RCt = Ct.RefCTypes
module PP  = Pretty
module E   = Errormsg

module Misc = FixMisc 
module SS  = Misc.StringSet
module SM  = Misc.StringMap
module NM  = FA.NameMap
module SLM = Sloc.SlocMap
module IM  = Misc.IntMap
module ST  = Ssa_transform
module CM  = CilMisc
module VM  = CM.VarMap

open Misc.Ops

let mydebug = false


(*******************************************************************)
(****************** Random Printers for Debugging ******************)
(*******************************************************************)

let d_vartyp () (v, t) = 
  PP.dprintf "(%s [%a] :: %a)" v.Cil.vname Ct.I.CType.d_ctype t Cil.d_type v.Cil.vtype

let d_vartypes () vts = 
  PP.seq (PP.text ",") (d_vartyp ()) vts

let d_sloc_vartyps () (sloc, vts) = 
  PP.dprintf "[%a |-> %a]\n" Sloc.d_sloc sloc d_vartypes vts

let d_sloc_typ () (sloc, t) = 
  PP.dprintf "[%a |-> %a]\n" Sloc.d_sloc sloc Cil.d_type t

let d_sloc_typs () slocts = 
  PP.docList ~sep:(PP.dprintf "@!") (d_sloc_typ ()) () slocts

let d_vars () vs = 
  PP.docList ~sep:(PP.text ",") (fun v -> PP.dprintf "%s" v.Cil.vname) () vs

let d_typ_vars () (t, vs) = 
  PP.dprintf "%a %a;@!" Cil.d_type t d_vars vs

let d_typ_varss () tvss =
  PP.docList ~sep:(PP.dprintf "@!") (d_typ_vars ()) () tvss 

let d_sloc_typ_varss () (sloc, tvss) = 
  PP.dprintf "%a <<%d>> |-> @[%a@]" 
    Sloc.d_sloc sloc
    (List.length tvss)
    d_typ_varss tvss

(*******************************************************************)
(****************** Representation for Bindings ********************)
(*******************************************************************)

type rhs     = AsgnE of Cil.instr | AsgnV of Cil.varinfo 
type vbind   = Ct.binder * (Ct.refctype * Cil.typ)
type binding = TVar of vbind 
             | TFun of string      * (Ct.refcfun  * Cil.fundec)
             | TSto of string      * Ct.refstore 
             | TSSA of string      * ST.vmap_t
             | TAsg of Cil.varinfo * ((Cil.location * rhs) list)

let report_bad_binding = function 
  | TVar (Ct.N x, (cr, _)) ->
      E.warn "\nBad TVar for %s :: \n\n@[%a@]" (FA.string_of_name x) Ct.d_refctype cr
  | TFun (fn, (cf, _)) ->
      E.warn "\nBad TFun for %s ::\n\n@[%a@]" fn Ct.d_refcfun cf
  | TSto (fn, st) -> 
      E.error "\nBad TSto for %s ::\n\n@[%a@]" fn Ct.d_refstore st 

let apply_solution =
  let s_typ s = RCt.CType.map (Misc.app_snd (FixConstraint.apply_solution s)) in
  let s_fun s = RCt.CFun.map (s_typ s) <+> RCt.CFun.apply_effects (s_typ s) in
  let s_sto s = RCt.Store.map (s_typ s) in
  fun s a -> match a with 
    | TVar (n, (cr, z)) -> TVar (n, (s_typ s cr, z))
    | TFun (f, (cf, z)) -> TFun (f, (s_fun s cf, z))
    | TSto (f, st)      -> TSto (f, s_sto s st) 

let apply_solution s x = 
  Misc.do_catch_ret "Annots.apply_solution" (apply_solution s) x x

let apply_solution_qs qs s =
  let s' = fun v ->
    List.map begin function
      | (Ast.Bexp (Ast.App (sym,args),_),_) as p ->
        begin match Qualifier.expandPred sym args with
          | None    -> p
          | Some p' -> p'
        end
      | p -> p
    end (s v)
  in apply_solution s'

(*******************************************************************)
(********** Building Map from Fun -> (Sloc -> Cil.typ) *************)
(*******************************************************************)

let target_type_of_ptr t = match Cil.unrollType t with 
(*  | Cil.TPtr (Cil.TFun _ , _) -> 
      assertf "TBD: target_type_of_ptr : function pointer"
*)  | Cil.TPtr (t, _) | Cil.TArray (t,_,_) ->
      Some (Cil.unrollType t)
    | _ ->
      None

let biggest_type (vs : Cil.varinfo list) : Cil.typ = 
   vs |> Misc.map_partial (fun v -> target_type_of_ptr v.Cil.vtype)
      |> (function [] -> E.s <| E.error "Annots.biggest type: No pointers! %a"
                         (PP.d_list ", " (CM.d_var)) vs
                 | ts -> Misc.list_max_with "biggest_type" Cil.bitsSizeOf ts)
 
let sloc_typem_of_shape sh =
  sh.Shape.vtyps
  >> wwhen mydebug (E.log "Annots.sloc_typem_of_shape: %a \n" d_vartypes)
  |> List.filter (snd <+> (function Ct.Ref (_,_) -> true | _ -> false))
  |> Misc.kgroupby (snd <+> Ct.I.CType.sloc) 
  |> Misc.map_partial (function (Some x, y) -> Some (x, y) | _ -> None) 
  |> List.map (Misc.app_snd (List.map fst <+> biggest_type))
  (* >> (PP.printf "SLOCMAP BEGIN:\n%a\nSLOCMAP END.\n" d_sloc_typs) *)
  |> SLM.of_list

(*******************************************************************)
(*** Decorating refldesc, refstore, refcfun with Cil Information ***)
(*******************************************************************)

let decorate_refldesc slocm sloc ld =  
  match SLM.maybe_find sloc slocm with
  | Some ty -> RCt.LDesc.decorate sloc ty ld
  | _       -> begin Errormsg.log "WARNING: Annots.decorate_ldesc: unknown cil info for %a \n" 
                       Sloc.d_sloc sloc
                     |> ignore; ld
               end

let has_fldtype fld = 
  Misc.maybe_bool (RCt.Field.get_fieldinfo fld).Ct.ftype

let check_ld_bindings slocm l ld =
  let iflds = RCt.LDesc.bindings ld in
  if not (List.for_all (snd <+> has_fldtype) iflds) then
    if not (SLM.mem l slocm) then 
      E.warn "Annots.check_ld_bindings unknown sloc %a \n" Sloc.d_sloc l
    else
      E.warn "Annots.check_ld_bindings bad fields for %a |-> %a" 
      Sloc.d_sloc l RCt.LDesc.d_ldesc ld

let decorate_refldesc slocm l ld = 
  decorate_refldesc slocm l ld 
  >> check_ld_bindings slocm l 

let decorate_refstore slocm sto = 
  RCt.Store.map_ldesc (decorate_refldesc slocm) sto

let decorate_refcfun slocm f cf = 
  try
    RCt.CFun.map_ldesc (decorate_refldesc slocm) cf 
  with _ -> E.s <| E.error "Annots.decorate_refcfun %s" f

(*******************************************************************)
(***************** Rendering Annots (Refinements Only) *************)
(*******************************************************************)

let kts_of_bind = function
  | TVar (Ct.N n, _) ->
      let x    = FA.string_of_name n in
      [x, ("variable "^x)]
  | TFun (f, _) -> 
      [f, ("function "^f)]
  | TSto (f, st) -> 
      RCt.Store.domain st 
      |> List.map (Pretty.sprint ~width:80 <.> Sloc.d_sloc ())
      |> List.map (fun s -> (s, s^" |->")) 

let d_bind_orig () = function
  | TVar (Ct.N n, (cr,_)) ->
      Pretty.dprintf "variable %s ::\n\n@[%a@] " 
      (FA.string_of_name n) Ct.d_refctype cr
  | TFun (f, (cf,_)) -> 
      Pretty.dprintf "function %s ::\n\n@[%a@] " 
      f Ct.d_refcfun cf 
  | TSto (f, st) -> 
      Pretty.dprintf "funstore %s ::\n\n@[%a@] " f Ct.d_refstore st

(*******************************************************************)
(*********************** Rendering (Hybrid) ************************)
(*******************************************************************)


let stitch_args fn cf = function 
  | None -> 
      E.s <| E.error "Annots.stitch_args no args for %s" fn
  | Some yts -> 
      let m = SM.of_list <| List.map (fun (y,t,_) -> (y,t)) yts in
      Misc.map_partial begin fun (x, ct) ->
        try  Some (Ct.S x, (ct, SM.find x m)) 
        with Not_found -> None 
      end cf.Ct.args

let deconstruct_fun (f, cf, fd) =
  let _                = List.map FA.name_of_varinfo fd.Cil.sformals in 
  let ret, argso, _, _ = Cil.splitFunctionTypeVI fd.Cil.svar in
  let xoctts           = stitch_args f cf argso              in
  (Ct.Nil, (cf.Ct.ret, ret)) ::  xoctts

let d_ann_fun () (f, cf, fd) =
  let rt :: xoctts = deconstruct_fun (f, cf, fd) in 
  CM.concat_docs 
    [ PP.dprintf "function %s ::@!@!" f
    ; PP.dprintf "%a@!"     RCt.LDesc.d_vbind rt 
    ; PP.dprintf "%s %a @!" f (CM.d_many_parens true RCt.LDesc.d_vbind) xoctts 
    (* ; effects *)
    ]

let d_ann_stores () ((f: string), (stos: Ct.refstore list)) =
  stos 
  |> Misc.flap (Ct.RefCTypes.Store.bindings) (* ignore funptrs *)
  |> Misc.kgroupby (fst <+> Sloc.to_string)
  |> Misc.flap snd
  |> PP.dprintf "funstore %s ::@!@!%a" f (PP.d_list "\n\n" RCt.LDesc.d_sloc_ldesc)

let d_bind_hybrid () = function 
  | TVar (Ct.N x, (ct, t)) -> 
      PP.dprintf "variable %a ::@!@!@[%a@]@!@!" 
        FA.d_name x RCt.LDesc.d_vbind (Ct.N x, (ct, t))
  | TFun (f, (cf, fundec)) ->
      CM.concat_docs 
        [ d_ann_fun () (f, cf, fundec) 
        ; PP.text Co.annotsep_name
        ; d_ann_stores () (f, [cf.Ct.sto_in; cf.Ct.sto_out])]
  | TSto (f, st) ->
      PP.nil (* d_ann_stores () (f, [st]) *)

let d_bind = (* d_bind_orig *) d_bind_hybrid

let d_bind_raw () = function
  | TVar (Ct.N x, _) -> PP.dprintf "variable %a" FA.d_name x 
  | TFun (f, _) -> PP.dprintf "function %s" f
  | TSto (f, _) -> PP.dprintf "funstore %s" f

(*******************************************************************)
(************************ Write to File ****************************)
(*******************************************************************)

let generate_ispec gst bs = 
  let fn = !Co.csolve_file_prefix ^ ".infspec" in
  Misc.with_out_file fn begin fun oc -> 
    let _ = Ctypes.RefCTypes.Store.fold_locs (fun l ld _ -> 
      Pretty.fprintf oc "loc %a %a %a@!@!"
        Sloc.d_sloc l Ct.d_specTypeRel Ctypes.HasType Ctypes.RefCTypes.LDesc.d_ldesc ld |> ignore) () gst
    in	
    bs |> Misc.map_partial (function TFun (x,y) -> Some (x,y) | _ -> None)
       |> (fun bs -> PP.seq ~sep:(PP.text "\n\n") ~doit:(fun (fn, (cf, _)) ->
             PP.dprintf "%s ::\n@[%a@]" fn Ct.d_refcfun cf) ~elements:bs)
       |> PP.fprint ~width:80 oc
  end

let generate_annots d = 
  Misc.with_out_file (!Co.csolve_file_prefix ^ ".annot") begin fun oc ->
    Pretty.fprint ~width:80 oc d 
  end

let generate_tags kts =
  Misc.with_out_file (!Co.csolve_file_prefix ^ ".tags") begin fun oc -> 
    kts 
    |> List.sort (fun (k1,_) (k2,_) -> compare k1 k2) 
    |> List.iter (fun (k,t) -> ignore <| PP.fprintf oc "%s\t%s.annot\t/%s/\n" k !Co.csolve_file_prefix t) 
  end

let generate_vmap fssam =
  Misc.with_out_file (!Co.csolve_file_prefix^".vmap") begin fun oc -> 
    SM.iter begin fun fn vmap -> 
      vmap
      |> Misc.hashtbl_to_list
      |> Misc.sort_and_compact 
      |> List.iter begin fun ((vname, file, line), ssaname) ->
           let vname = CM.unrename_local (* fn *) vname
           in  Printf.fprintf oc "%s \t %s \t %d \t %s \n" vname file line ssaname
         end
    end fssam 
  end

(*******************************************************************)
(************** Hoarding Information About Bindings ****************)
(*******************************************************************)

class annotations = object (self)
  val vart          = H.create 37
  val funt          = H.create 37
  val stot          = H.create 37
  val mutable asgnm = VM.empty 
  val mutable fssam = (SM.empty : ST.vmap_t SM.t)
  val mutable flocm = (SM.empty : (Cil.typ SLM.t) SM.t)
  val mutable fdecm = (SM.empty : Cil.fundec SM.t)

  method private eq_var_of_reft = function
    | (vv, _, [FixConstraint.Conc p]) ->
      begin match Ast.Predicate.unwrap p with
        | Ast.Atom (e1, Ast.Eq, e2) ->
          begin match Ast.Expression.unwrap e1, Ast.Expression.unwrap e2 with
            | Ast.Var v1, Ast.Var v2 when v1 = vv -> Some v2
            | _                                   -> None
          end
        | _ -> None
      end
    | _ -> None

  method private telescope_binds () =
       vart
    |> Misc.hashtbl_to_list
    |> List.iter begin fun (v, ((rct, _) as vbind)) ->
         match rct |> Ctypes.reft_of_refctype |> self#eq_var_of_reft with
           | Some v' ->
             if Hashtbl.mem vart v' then
               Hashtbl.replace vart v (Hashtbl.find vart v')
             else ()
           | None -> ()
       end

  method get_binds () : binding list = 
    self#telescope_binds ();
    (   (List.map (fun (x,y) -> TFun (x, y))   (Misc.hashtbl_to_list funt))
     ++ (List.map (fun (x,y) -> TSto (x, y))   (Misc.hashtbl_to_list stot))
     ++ (List.map (fun (x,y) -> TVar (Ct.N x, y)) (Misc.hashtbl_to_list vart))
     ++ (List.map (fun (x,y) -> TSSA (x, y))   (SM.to_list fssam))
     ++ (List.map (fun (x,y) -> TAsg (x, y))   (VM.to_list asgnm))
    ) >> wwhen mydebug (List.length <+> E.log "\n\nAnnots.dump_annots (%d)\n\n" (*PP.d_list "\n" d_bind_raw*))


  method private get_flocm (f: string) : (Cil.typ SLM.t) option =
    try Some (SM.find f flocm) with Not_found ->     
      let _ = E.log "Annots: Missing Location-Types for %s \n" f 
      in None

  method private get_fun_dec  (f: string) : Cil.fundec =
    try SM.find f fdecm with Not_found ->
      E.s <| E.error "Annots.get_fun_dec, unknown function %s\n" f

  method set_shape (shm : Shape.t SM.t) (scim : ST.t SM.t) (cfm : Ct.refcfun SM.t) : unit =
    fssam <- SM.map (fun sci -> sci.ST.vmapt) scim ;
    fdecm <- SM.map (fun sci -> sci.ST.fdec) scim  ;
    flocm <- SM.map (sloc_typem_of_shape) shm;
    SM.iter self#add_fun cfm

  method add_asgn x l z =
    asgnm <- VM.adds x [(l, z)] asgnm
  
  method add_var x ct =
    (if mydebug then E.log "Annots.add_var(a) %a \n" FA.d_name x); 
    Misc.maybe_iter begin fun v ->
      (if mydebug then E.log "Annots.add_var(b) %s \n" v.Cil.vname); 
      H.replace vart x (ct, v.Cil.vtype);
    end (FA.varinfo_of_name x)

  method private add_fun f cf = 
    Misc.maybe_iter begin fun locm ->
      let _   = if mydebug then E.log "ADD FUN %s \n" f  in
      let fd  = self#get_fun_dec f                in
      let cf  = decorate_refcfun locm f cf        in
      let _   = deconstruct_fun (f, cf, fd)       
                |>: (function (Ct.N n, (ct, _)) -> self#add_var n ct
                            | (Ct.S x, (ct, _)) -> self#add_var (FA.name_of_string x) ct
                            | _              -> ()
                    )
      in H.replace funt f (cf, fd)
    end (self#get_flocm f)

  method add_sto f st = 
    Misc.maybe_iter begin fun locm ->
      H.replace stot f (decorate_refstore locm st)
    end (self#get_flocm f)

  method dump_annots so =
    self#get_binds () 
    |> Misc.filter (function TSSA (_, _) | TAsg (_,_) -> false | _ -> true)
    |> Misc.maybe_apply (Misc.map <.> apply_solution) so
    |> (PP.d_list Co.annotsep_name d_bind () <*> Misc.flap kts_of_bind)
    |> (generate_annots <**> generate_tags)
    |> (fun _ -> generate_vmap fssam)

  method dump_infspec gst (decs:CM.dec list) (qs:Qualifier.t list) s =
    let ds = decs |> Misc.map_partial (function CM.FunDec (fn,_,_) -> Some fn | _ -> None) |>  SS.of_list in
    let binds = Misc.hashtbl_to_list funt
             |> Misc.filter (fst <+> Misc.flip SS.mem ds)
             |> Misc.map (fun (x,y) -> apply_solution_qs qs s (TFun (x,y)))
    in
    let TSto (_,gst') = apply_solution_qs qs s (TSto ("global", gst)) in
    generate_ispec gst' binds
end


(*******************************************************************)
(******************************* API *******************************)
(*******************************************************************)

let annr = ref (new annotations)

(* API *)
let annot_shape   = fun x y z   -> (!annr)#set_shape x y z 
let annot_sto     = fun x y     -> (!annr)#add_sto x y
let annot_var     = fun x y     -> (!annr)#add_var x y
let annot_asgn    = fun x l z   -> (!annr)#add_asgn x l z
let clear         = fun _       -> annr := new annotations 
let dump_infspec  = fun gst decs qs s  -> (!annr)#dump_infspec gst decs qs s
let dump_annots   = fun so      -> (!annr)#dump_annots so
let dump_bindings = fun ()      -> (!annr)#get_binds ()
