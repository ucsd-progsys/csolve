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


module Misc= FixMisc

(* Data Types and Printers for the JSON representation of Csolve results *)
module Co  = Constants
module PP  = Pretty
module A   = Ast
module Sy  = A.Symbol
module SM  = Sy.SMap
module SSM = Misc.StringMap
module IM  = Misc.IntMap
module C   = FixConstraint
module Q   = Qualifier
module An  = Annots
module Ct  = Ctypes

open Misc.Ops

type qdef  = string
type pred  = string 
type expr  = string
type act   = string
type ctyp  = string
type varid = string

type srcLoc = 
  { line : int
  ; file : string 
  }

type vardef  = 
  { varId   : varid
  ; varName : string 
  ; varLoc  : srcLoc option
  ; varExpr : expr   option
  ; varDeps : varid list 
  }

type qarg = 
  { qargname : string 
  ; qargid   : varid option
  } 

type qual  =
  { qname : string
  ; qargs : qarg list
  ; qfull : A.pred option
  ; qurl  : act
  }

type annotv = 
  { vname  : Ct.binder
  ; ctype  : ctyp
  ; quals  : qual list
  ; conc   : A.pred list 
  }

type annotf = 
  { fname  : Ct.binder 
  ; args   : annotv list
  ; ret    : annotv
  }

type json = 
  { errors   : srcLoc list
  ; qualDef  : qdef SSM.t
  ; varDef   : vardef SSM.t 
  ; genAnnot : annotv
  ; varAnnot : (annotv IM.t) SSM.t 
  ; funAnnot : annotf SSM.t
  ; cones    : (srcLoc Ast.Cone.t) list
  }

let junkUrl   = ""
let junkAnnot = { vname = Ct.Nil; ctype = "(void *)"; quals = []; conc = [] }

(*******************************************************************)
(************* Render JSON as Pretty.doc ***************************)
(*******************************************************************)

let d_str () s       = PP.dprintf "\"%s\"" s 
let d_many f () xs   = PP.d_list ", " f () xs
let d_kv f () (k, v) = PP.dprintf "%a : %a" d_str k f v


(***************** Serializing Maps and Arrays **************************)

let d_kvs f () kvs = PP.dprintf "{ @[%a@] }" (d_many (d_kv f)) kvs 
let d_array f ()   = PP.dprintf "[ @[%a@] ]" (d_many f) 
let d_sm f ()      = SSM.to_list <+> d_kvs f ()
let d_im f ()      = IM.to_list <+> Misc.map (Misc.app_fst string_of_int) <+> d_kvs f ()
let d_opt f ()     = function None -> PP.text "null" | Some x -> PP.dprintf "%a" f x

(***************** Serializing String Aliases ***************************)

let d_qdef  = d_str 
let d_expr  = d_str 
let d_act   = d_str
let d_ctype = d_str
let d_varid = d_str

(***************** Serializing Specialized Records **********************) 

let d_binder () b = PP.dprintf "\"%a\"" Ct.d_binder b

let d_srcLoc () e = 
  PP.dprintf "{ line : %d }" e.line

let d_vardef () d = 
  PP.dprintf "{ varLoc  : %a
              , varId   : %a
              , varName : %a
              , varExpr : %a
              , varDeps : %a 
              }"
  (d_opt d_srcLoc)  d.varLoc
  (d_varid)         d.varId
  d_str             d.varName
  (d_opt d_expr)    d.varExpr
  (d_array d_varid) d.varDeps

let d_qarg () a = 
  PP.dprintf
  "{ qargname : %a
   , qargid   : %a
   }" 
   d_str           a.qargname
   (d_opt d_varid) a.qargid

let d_pred () p =
  PP.dprintf "\"%s\"" (A.Predicate.to_string p)

let d_qual () q =
  PP.dprintf 
  "{ qname : %a 
   , qargs : @[%a@] 
   , qfull : @[%a@]
   , qurl  : @[%a@]
   }" 
    d_str            q.qname 
    (d_array d_qarg) q.qargs
    (d_opt d_pred)   q.qfull
    d_act            q.qurl

let d_annotv () a =
  PP.dprintf 
  "{ vname  : @[%a@]
   , ctype  : @[%a@]
   , quals  : @[%a@]
   , conc   : @[%a@] 
   }"
    d_binder         a.vname
    d_ctype          a.ctype
    (d_array d_qual) a.quals
    (d_array d_pred) a.conc

let d_annotf () f =
  PP.dprintf 
  "{ fname : @[%a@]
   , args  : @[%a@]
   , ret   : @[%a@] 
   }"
   d_binder           f.fname
   (d_array d_annotv) f.args
   d_annotv           f.ret


let rec d_cone () = function
  | A.Cone.Empty    
  -> PP.text "null"
  | A.Cone.Cone xcs 
  -> d_array begin fun () (l, c) ->
       PP.dprintf "{ line : %d, file : \"%s\", cone : %a }" l.line l.file d_cone c
     end () xcs

let d_json () x = 
  PP.dprintf 
  "{ errors   : @[%a@]
   , qualDef  : @[%a@]
   , cones    : @[%a@]
   , varDef   : @[%a@]
   , genAnnot : @[%a@]
   , varAnnot : @[%a@] 
   , funAnnot : @[%a@]
   }"
    (d_array d_srcLoc)     x.errors
    (d_sm d_qdef)          x.qualDef
    (d_array d_cone)       x.cones
    (d_sm d_vardef)        x.varDef
    (d_annotv)             x.genAnnot
    (d_sm (d_im d_annotv)) x.varAnnot
    (d_sm d_annotf)        x.funAnnot

(*******************************************************************)
(************* Conversions *****************************************) 
(*******************************************************************)

(* TODO: rename locals to drop SSA/fun-name *)
let d_cilexp_tidy = Cil.d_exp

let doc_of_instr = function
  | Cil.Set ((Cil.Var v, Cil.NoOffset), e, _) -> 
     PP.dprintf "%a" (* v.Cil.vname *) d_cilexp_tidy e
  | Cil.Call (Some (Cil.Var v, Cil.NoOffset), fe, es, _) ->
     PP.dprintf "%a(%a)" (* v.Cil.vname *) d_cilexp_tidy fe  (d_many d_cilexp_tidy) es

let expr_of_instr = PP.sprint ~width:80 <.> doc_of_instr

let srcLoc_of_location l = { line = l.Cil.line; file = l.Cil.file }

let srcLoc_of_constraint tgr c = 
  c |> FixConstraint.tag_of_t 
    |> CilTag.t_of_tag
    |> CilTag.loc_of_t tgr
    |> srcLoc_of_location 
 
(*******************************************************************)
(************* Build Map from var-line -> ssavar *******************)
(*******************************************************************)

let abbrev_binder abbrev = function
  | Ct.S x -> Ct.S (abbrev x)
  | b      -> b

let abbrev_expr abbrev = function
  | A.Var x, _ -> x |> Sy.to_string |> abbrev |> Sy.of_string |> A.eVar
  | e -> e 

let qarg_of_expr abbrev e =
  let s = A.Expression.to_string e  in
  let s' = abbrev s                 in
  { qargname = abbrev s
  ; qargid   = if s = s' then None else Some s 
  } 

let mkCtype  = CilMisc.pretty_to_string (fun () t -> Cil.d_type () (CilMisc.typStripAttrs t))

let mkQual a = fun (f, es) -> 
  { qname = Sy.to_string f
  ; qargs = List.map (qarg_of_expr a) es
  ; qfull = Q.expandPred f (List.map (abbrev_expr a) es)
  ; qurl  = junkUrl 
  }

let deconstruct_pApp = function
  | A.Bexp (A.App (f, es), _), _ -> Some (f, es)
  | _                            -> None


let annot_of_vbind abbrev s (x, (cr, ct)) =
  let cs, ks  = cr |> Ct.reft_of_refctype 
                   |> thd3
                   |> Misc.either_partition (function C.Conc p -> Left p | C.Kvar (su,k) -> Right (su, k)) in
  let qs, cs' = ks |> Misc.flap (fun (su, k) -> (s k) |> List.map (Misc.flip A.substs_pred su))
                   |> Misc.either_partition (fun p -> match deconstruct_pApp p with Some z -> Left z | _ -> Right p) in
  let cs      = cs ++ cs'
                   |> List.filter (not <.> A.Predicate.is_tauto)
                   |> (fun cs -> if qs = [] && cs = [] then [A.pTrue] else cs) in
  { vname = abbrev_binder abbrev x
  ; ctype = mkCtype ct 
  ; quals = List.map (mkQual abbrev) qs
  ; conc  = cs
  }

let annot_of_finfo abbrev s (fn, (cf, fd)) =
  (fn, cf, fd) 
  |> Annots.deconstruct_fun 
  |> List.map (annot_of_vbind abbrev s) 
  |> (function (ret::args) -> { fname = Ct.S fn; args = args; ret = ret })

let mkVarLineSsavarMap bs : (Sy.t IM.t) SSM.t =
  let get x m     = if SSM.mem x m then (SSM.find x m) else IM.empty in 
  let put x n y m = SSM.add x (IM.add n y (get x m)) m               in
  bs |> Misc.flap begin function 
          | Annots.TSSA (fn, t) -> 
             t |>  Misc.hashtbl_to_list 
               |>: Misc.app_fst (Misc.app_fst3 (CilMisc.unrename_local fn))
          | _ -> []
        end
     |> List.fold_left begin fun m ((x, file, line), xssa) ->
          put x line (FixAstInterface.name_of_string xssa) m 
        end SSM.empty

let mkAbbrev bs =
  bs |> Misc.flap begin function
          | An.TSSA (fn, t) -> 
              t |> Misc.hashtbl_to_list 
                |> Misc.map (fun ((s,_,_), s') -> (s', CilMisc.unrename_local fn s))
          | _ -> []
        end
     |> Misc.hashtbl_of_list
     |> (fun t -> (fun s -> try Hashtbl.find t s with Not_found -> s))

let mkSyInfoMap bs =
  bs |> Misc.map_partial (function An.TVar (Ct.N x, y) -> Some (x, y) | _ -> None)
     |> SM.of_list

let varid_of_varinfo v = v.Cil.vname

let vardef_of_rhss abbrev v rs =
  let vid   = varid_of_varinfo v in
  let vs_es = Misc.either_partition begin function 
                | (_, An.AsgnV v') -> Left v'
                | (l, An.AsgnE e)  -> Right (l, e)
              end rs                                 
  in match vs_es with
  | ([], [(l, e)]) ->
    { varId   = vid
    ; varName = abbrev vid 
    ; varLoc  = Some (srcLoc_of_location l)
    ; varExpr = Some (expr_of_instr e)
    ; varDeps = [] 
    }
  | (vs, []) ->
    { varId   = vid
    ; varName = abbrev vid 
    ; varLoc  = None
    ; varExpr = None 
    ; varDeps = List.map varid_of_varinfo vs 
    }
  | _ -> 
      Errormsg.s <| Errormsg.error "vardef_of_rhss: v=%s" v.Cil.vname

let mkVarDef abbrev (bs : An.binding list) =
  bs |> Misc.map_partial (function An.TAsg (v, rs) -> Some (v, rs) | _ -> None)
     |> Misc.map (fun (v, rs) -> (v.Cil.vname, vardef_of_rhss abbrev v rs))
     |> SSM.of_list

(*******************************************************************)
(************* Convert Bindings to JSON ****************************)
(*******************************************************************)

  
let mkErrors tgr = 
  List.map (srcLoc_of_constraint tgr)

let mkQualdef q = 
  let qn = Sy.to_string <| Q.name_of_t q in
  let qd = A.Predicate.to_string  <| Q.pred_of_t q in
  (qn, qd)

let mkQualdefm qs =
  SSM.of_list <| List.map mkQualdef qs

let mkVarAnnot abbrev s bs =
  let xm = mkSyInfoMap bs in
  bs |> mkVarLineSsavarMap
     |> SSM.map begin IM.map_partial (fun x ->
          Misc.maybe_map 
            (fun z -> annot_of_vbind abbrev s (Ct.N x, z)) 
            (SM.maybe_find x xm)
          )
        end 

let mkFunAnnot abbrev s bs =
  bs |> Misc.map_partial (function Annots.TFun (f, x) -> Some (f, x) | _ -> None)  
     |> SSM.of_list
     |> SSM.mapi (fun fn x -> annot_of_finfo abbrev s (fn, x))

let mkCones = List.map (Ast.Cone.map srcLoc_of_location)

let bindsToJson qs tgr s cs cones binds =
  let abbrev = mkAbbrev binds in
  { errors   = mkErrors tgr cs
  ; qualDef  = mkQualdefm qs
  ; varDef   = mkVarDef abbrev binds
  ; genAnnot = junkAnnot
  ; varAnnot = mkVarAnnot abbrev s binds
  ; funAnnot = mkFunAnnot abbrev s binds
  ; cones    = mkCones cones  
  }

(*******************************************************************)
(************* API *************************************************)
(*******************************************************************)

let dump_annots qs tgr s' cs' cones binds : unit =
  let f = !Co.csolve_file_prefix^".json" in
  let d = d_json () <| bindsToJson qs tgr s' cs' cones binds in
  Misc.with_out_file f (fun oc -> Pretty.fprint ~width:80 oc d)

let dump_html qs tgr s' cs' cones binds : unit =
  (* 1. Dump JSON Annots *)
  dump_annots qs tgr s' cs' cones binds;
  (* 2. Render HTML *)
  Sys.command (Printf.sprintf "%s %s" (Co.get_c2html ()) (!Co.csolve_file_prefix))
  |> (function 0 -> Errormsg.log  "DONE: Generated Annotated HTML" 
             | e -> Errormsg.warn "WARNING: Failed To Generate Annotated Html %d" e)

(* API *)
let dump_html qs tgr s' cs' cones binds : unit = 
  if Misc.is_suffix ".c" !Co.csolve_file_prefix then
    dump_html qs tgr s' cs' cones binds
  else
    ignore <| Errormsg.log "SKIP: Multiple Files: Not generating Annotated HTML."

