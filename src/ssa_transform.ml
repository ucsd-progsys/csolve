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

(* This file is part of the CSolve Project.*)


module E  = Errormsg
module S  = Ssa
module IH = Inthash
module VS = Usedef.VS
module H  = Hashtbl
module Co = Constants
module CM = CilMisc

open Cil
module Misc = FixMisc 
module IM = Misc.IntMap

open Misc.Ops
let mydebug = false 

(**************************************************************************
 * out_t : (block * reg, regindex) H.t                                    *
 * out_t.(b,r) = Undef      if r is unmodified (no write/phi) in b        *
 *             = Phi b      if r is only phi-written in b                 *
 *             = Def (b, k) if r is written k times in block              *
 *                                                                        *
 * dramatis personae:                                                     *
 * doms  : int list array;                                                *
 * v2r   : varinfo -> S.reg;                                              *
 * r2v   : S.reg   -> varinfo;                                            *
 * out_t : (int * int, regindex) H.t;           block,reg     |-> ri      *
 * var_t : (string * regindex, varinfo) H.t;    var,ri        |-> varinfo *
 * vmap_t: (string * string * int, string) H.t; var,file,line |-> ssavar  *
 **************************************************************************)


type regindex = Def of int * int                (* block, position *)
              | Phi of int                      (* block *)

type vmap_t   = (string * string * int, string) Hashtbl.t

type t = { 
    fdec  : fundec
  ; cfg   : S.cfgInfo
  ; phis  : (varinfo * (int * varinfo) list) list array (*  block |-> (var, (block, var) list) list *) 
  ; ifs   : Guards.ginfo array
  ; gdoms : (int * (bool option)) array
  ; edoms : ((int * int), bool) Hashtbl.t
  ; vmapt : vmap_t
}

let ssas = "__SSA__"

let mk_ssa_name s = function
  | Def (b, k) -> Printf.sprintf "%s%sblk_%d_%d" s ssas b k
  | Phi b      -> Printf.sprintf "%s%sphi_%d" s ssas b

let regindex_of_string s = 
  try match Misc.chop s "_" with
      | ["blk"; bs; ks] -> Def (int_of_string bs, int_of_string ks)
      | ["phi"; bs]     -> Phi (int_of_string bs)
  with _ -> assertf "regindex_of_string"

let deconstruct_ssa_name s =
  match Misc.chop s ssas with
  | []  -> assertf "chop returns empty list"
  | [_] -> None
  | xs  -> try xs |> Misc.list_snoc 
                  |> Misc.app_snd (String.concat ssas) 
                  |> Misc.app_fst regindex_of_string
                  |> (fun (ri, b) -> Some (b, ri))
           with _ -> None


let is_ssa_renamable v =
  not (v.vglob || CM.is_cil_tempvar v.vname)

(******************************* Printers *******************************)

let lvars_to_string cfg lvs =
    String.concat ","
     (List.map (fun (i,j) -> Printf.sprintf "%s(%d) def at %d" 
                             cfg.S.regToVarinfo.(i).vname i j) lvs) 

let print_blocks cfg =
    Array.iteri begin fun i b -> 
      ignore (E.log "\n ------> \n block %d [preds: %s, succs: %s] livevars: %s \n"
      i
      (Misc.map_to_string string_of_int cfg.S.predecessors.(i))
      (Misc.map_to_string string_of_int cfg.S.successors.(i))
      (lvars_to_string cfg b.S.livevars));
      E.log "statement = %a \n" Cil.d_stmt b.S.bstmt
      (* Cil.dumpStmt Cil.defaultCilPrinter stdout 0 b.S.bstmt *)
    end cfg.S.blocks
 
let print_out_t r2v out_t = 
  H.iter 
    (fun (b,r) ri ->
      let vn  = (r2v r).vname in
      let ssn = mk_ssa_name vn ri in
      ignore (E.log "out_t: block = %d : var = %s : reginfo = %s \n" b vn ssn))
    out_t

let print_phis phis = 
  Array.iteri begin fun i xs ->
    List.iter begin fun (v,vs) -> 
      ignore (E.log "block %d: %s <- phi(%s) \n" i v.vname 
      (String.concat "," (List.map (fun (j,v) -> Printf.sprintf "%d: %s" j v.vname) vs)))
    end xs
  end phis
 
let print_instrs i (us, ds) = 
    let vs2s vs = String.concat "," (List.map (fun v -> v.vname) (VS.elements vs)) in
    ignore (E.log "block = %d :uses= %s :defines= %s \n " i (vs2s us) (vs2s ds))

(******************************* create CFG *******************************)

let mk_var_reg_maps () = 
  let n_r       = ref 0 in
  let var_reg_m = IH.create 117 in
  let reg_var_m = IH.create 117 in
  ((fun () -> !n_r),
   (fun r  -> IH.find reg_var_m r), 
   (fun v  -> 
     asserti (is_ssa_renamable v) "v2r: on non-renamable %s" v.vname; 
     try IH.find var_reg_m v.vid with Not_found -> 
       let n = !n_r in
       let _ = incr n_r in
       let _ = IH.add reg_var_m n v in
       let _ = IH.add var_reg_m v.vid n in n))

let vs_to_regs v2r vs = 
  VS.fold begin fun v acc -> 
    if is_ssa_renamable v then (v2r v) :: acc else acc
  end vs []

let proc_stmt cfg v2r s = 
  cfg.S.successors.(s.sid)   <- List.map (fun s' -> s'.sid) s.succs;
  cfg.S.predecessors.(s.sid) <- List.map (fun s' -> s'.sid) s.preds;
  cfg.S.blocks.(s.sid)       <- 
    let uds =  
      match s.skind with 
      | Instr instrs -> 
          List.map Usedef.computeUseDefInstr instrs 
      | Return (Some e, _) | If (e,_,_,_) | Switch (e,_,_,_) ->
          [(Usedef.computeUseExp e, VS.empty)]  
      | Break _ | Continue _ | Goto _ | Block _ | Loop _ | Return _ -> 
          []
      | TryExcept _ | TryFinally _ -> 
          assert false in
    let ins = List.map (fun (us,ds) -> (vs_to_regs v2r ds, vs_to_regs v2r us)) uds in
    { S.bstmt     = s;
      S.instrlist = ins;
      S.livevars  = [];   (* set later *)
      S.reachable = true; (* set later *)}

let init_cfgInfo fdec = 
  let n  = List.fold_left (fun c s -> s.sid <- c; c+1) 0 fdec.sallstmts in
  let s0 = try List.hd fdec.sbody.bstmts 
           with _ -> E.s (E.bug "Function %s with no body" fdec.svar.vname) in
  let _  = asserts (s0.sid = 0) "ERROR: First block index nonzero" in
  { S.name         = fdec.svar.vname
  ; S.start        = s0.sid
  ; S.size         = n
  ; S.successors   = Array.make n []
  ; S.predecessors = Array.make n []
  ; S.blocks       = Array.make n { S.bstmt     = s0
                                  ; S.instrlist = []
                                  ; S.reachable = true
                                  ; S.livevars  = [] }
  ; S.nrRegs       = 0
  ; S.regToVarinfo = Array.make 0 dummyFunDec.svar
  ; }

let mk_cfg fdec = 
  let cfg  = init_cfgInfo fdec in
  let sz,r2v,v2r = mk_var_reg_maps () in
  List.iter (proc_stmt cfg v2r) fdec.sallstmts;
  cfg.S.regToVarinfo <- Array.init (sz ()) r2v;
  cfg.S.nrRegs <- (sz ()); 
  (cfg, r2v, v2r)

(*********************** renaming helpers ********************************)

let mk_out_name cfg =
  let out_t = H.create 117 in
  Array.iteri begin fun i b ->
    List.iter begin fun (r, j) -> 
      if j=i then H.replace out_t (i, r) (Phi i)
    end b.S.livevars;
    b.S.instrlist
    |> Misc.flap (fst <+> Misc.sort_and_compact)
    |> Misc.count_map
    |> IM.iter (fun r k -> H.replace out_t (i, r) (Def (i, k)))
  end cfg.S.blocks;
  out_t

let out_name cfg out_t k = 
  Misc.do_memo out_t 
    (fun (i, r) ->
      let b = cfg.S.blocks.(i) in  
      let j = List.assoc r b.S.livevars in
      H.find out_t (j, r))
    k k

let mk_renamed_var fdec var_t v ri =
  asserti (is_ssa_renamable v) "renaming non ssa-able var %s" v.vname; 
  try H.find var_t (v.vname, ri) with Not_found ->
    let v' = 
      match ri with 
      | Phi 0 -> v 
      | _     -> makeLocalVar fdec (mk_ssa_name v.vname ri) v.vtype in
    let _ = v'.vattr = v.vattr in
    let _ = H.replace var_t (v.vname, ri) v' in
    v'

let log_renamed_var vmap_t loc v v' = 
  (if mydebug then ignore <| E.log "SSA-RENAME: %s TO %s at %a \n" v.vname v'.vname d_loc loc);
  Hashtbl.add vmap_t (v.vname, loc.file, loc.line) v'.vname

(*********************** compute phi assignments **************************)

let mk_phi fdec cfg out_t var_t r2v i preds r =
  let v    = Misc.do_catch "mk_phi" r2v r in
  let vphi = mk_renamed_var fdec var_t v (Phi i) in
  preds |> List.map (fun j -> (j, out_name cfg out_t (j, r)))
        |> List.map (fun (j, ri) -> (j, mk_renamed_var fdec var_t v ri)) 
        |> fun z -> (vphi, z)

let add_phis fdec cfg out_t var_t r2v i b =
  match cfg.S.predecessors.(i) with
  | [] -> 
      []
  | (_::_) as ps -> 
      b.S.livevars                                                    (* take the livevars *)
      |> Misc.map_partial (fun (r,j) -> if i=j then Some r else None) (* filter the phi-vars *)
      |> Misc.map (mk_phi fdec cfg out_t var_t r2v i ps)              (* make phi-asgn *) 

(*********************** ssa rename visitor *******************************)

class ssaVisitor fdec cfg out_t var_t v2r vmap_t = object(self)
  inherit nopCilVisitor

  val sid   = ref 0
  val theta = ref IM.empty

  method private init_theta i = 
    let b = cfg.S.blocks.(i) in  
    List.fold_left 
      (fun m (r,j) -> 
        let ri = 
          if i = j then Phi i else 
            try out_name cfg out_t (j, r) 
            with _ -> E.s (E.bug "mk_init_theta") in
        IM.add r ri m)
      IM.empty b.S.livevars

  method private get_regindex read v =
    let r  = v2r v in
    let ri = IM.find r !theta in
    if read then ri else 
      let ri' = match ri with
                | Def (b,k) when b = !sid -> Def (!sid, k+1)
                | _ -> Def (!sid, 1) in
      let _   = theta := IM.add r ri' !theta in
      ri'

  method private rename_var read v =
    if not (is_ssa_renamable v) then v else
      try 
        self#get_regindex read v 
        |> mk_renamed_var fdec var_t v
        >> log_renamed_var vmap_t !currentLoc v 
     with e -> E.s (E.bug "rename_var fails: read = %b,  v = %s, def = %a, exn = %s" 
                           read v.vname d_loc v.vdecl (Printexc.to_string e)) 

  method vinst = function
    | Set (((Var v), NoOffset) , e, l) ->
        let e' = visitCilExpr (self :> cilVisitor) e in
        let v' = self#rename_var false v in
        let i' = Set (((Var v'), NoOffset), e', l) in
        ChangeTo [i']
    | Call (Some ((Var v), NoOffset), e, es, l) -> 
        let e' = visitCilExpr (self :> cilVisitor) e in
        let es'= List.map (visitCilExpr (self :> cilVisitor)) es in
        let v' = self#rename_var false v in
        let i' = Call (Some ((Var v'), NoOffset), e', es', l) in
        ChangeTo [i']
    | _ -> DoChildren
 
  method vvrbl (v: varinfo) : varinfo visitAction =
    ChangeTo (self#rename_var true v)

  method vstmt (s: stmt) : stmt visitAction = 
    theta := self#init_theta s.sid;
    sid   := s.sid;
    DoChildren
end

(********************* ssa-verifying visitor ******************************)

class checkSsaVisitor = object(self)
  inherit nopCilVisitor

  val mutable assigned = CM.VarSet.empty

  method vinst = function
    | Set ((Var v, NoOffset), _, loc)
    | Call (Some (Var v, NoOffset), _, _, loc) ->
      if not v.vglob && CM.VarSet.mem v assigned then
        E.s <| errorLoc loc "SSA Failure: double assingment to variable %a@!"
                 CM.d_var v
      else begin
        assigned <- CM.VarSet.add v assigned;
        DoChildren
      end
    | _ -> DoChildren
end

(**************************************************************************)

let mk_gdominators fdec cfg =
  let idom = S.compute_idom cfg in
  let _    = if mydebug then Array.iteri (fun i id -> ignore (E.log "idom of %d = %d \n" i id)) idom in 
  let n    = Array.length idom in
  let ifs  = Guards.mk_ifs n fdec in
  let gds  = Guards.mk_gdoms cfg.S.predecessors ifs idom in
  let eds  = Guards.mk_edoms cfg.S.predecessors ifs idom in
  (ifs, gds, eds)
  
let fdec_to_ssa_cfg fdec loc = 
  let (cfg,r2v,v2r) = mk_cfg fdec in
  let cfg           = S.prune_cfg cfg in
  let _             = S.add_ssa_info cfg in
  let _             = if mydebug then print_blocks cfg in
  let out_t         = mk_out_name cfg in
  let var_t         = H.create 117 in
  let vmap_t        = (H.create 37 : vmap_t) in
  let phis          = Array.mapi (add_phis fdec cfg out_t var_t r2v) cfg.S.blocks in
  let _             = visitCilFunction (new ssaVisitor fdec cfg out_t var_t v2r vmap_t) fdec in
  let ifs, gds, eds = mk_gdominators fdec cfg in
  let _             = visitCilFunction (new checkSsaVisitor) fdec in
  {fdec = fdec; cfg = cfg; phis = phis; ifs = ifs; gdoms = gds; edoms = eds; vmapt = vmap_t} 

(**************************************************************************)

let print_sci oco sci = 
  if mydebug then begin
    print_phis sci.phis;
    Guards.print_ifs sci.ifs;
    Guards.print_gdoms sci.gdoms;
    Guards.print_edoms sci.edoms;
  end;
  let oc = match oco with Some oc -> oc | None -> stdout in
  Cil.dumpGlobal Cil.defaultCilPrinter oc (GFun (sci.fdec,Cil.locUnknown))

let print_scis scis =
  Misc.with_out_file (!Co.csolve_file_prefix^".ssa.c") begin fun oc -> 
    List.iter (print_sci (Some oc)) scis
  end

(**************************************************************************)


(***********************************************************************)

(* API *)
let scim_of_decs decs = 
  decs
  |> Misc.map_partial (function CilMisc.FunDec (_,x,y) -> Some (x,y) | _ -> None)
  |> Misc.map (Misc.uncurry fdec_to_ssa_cfg)
  >> print_scis
  |> List.map (Misc.pad_fst (fun sci -> sci.fdec.svar.vname)) 
  |> Misc.StringMap.of_list


let rec reachable_blocks_aux sci marked blockss wklist =
  if wklist = [] then
    blockss |> List.concat |> List.rev
  else
    let blockss = wklist :: blockss in
    let marked  = List.fold_left (Misc.flip Misc.IntSet.add) marked wklist in
      wklist
   |> Misc.flap (fun i -> sci.cfg.S.successors.(i))
   |> List.filter (fun j -> not (Misc.IntSet.mem j marked))
   |> Misc.sort_and_compact
   |> reachable_blocks_aux sci marked blockss

(* API *)
let reachable_blocks_of_sci sci =
  reachable_blocks_aux sci Misc.IntSet.empty [] [0]
