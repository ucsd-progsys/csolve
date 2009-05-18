
open Cil
open Pretty
module E = Errormsg
module CG = Callgraph
module H = Hashtbl 
module U = Util
module IH = Inthash
module VS = Usedef.VS

module S = Ssa

let debug = false
let hasbeenconverted = ref false

let arithAbsOut = ref stdout
let setArithAbsFile (s: string) =
  try 
    arithAbsOut := open_out s
  with _ -> ignore (E.warn "Cannot open the output file %s" s)

(* Print out *)
let pd ?(ind=0) (d: doc) : unit = 
  Pretty.fprint !arithAbsOut 80 (indent ind d)

let p ?(ind=0) (fmt : ('a,unit,doc) format) : 'a = 
  let f d =  
    pd ~ind:ind d;
    nil 
  in
  Pretty.gprintf f fmt


(** Variables whose address is taken are ignores. Set this to true if you 
 * want references to the address of such variables to be printed as the only 
 * accesses of the variable *)
let treatAddressOfAsRead = true

(** The globals written, indexed by Id of the function variable. Each inner 
 * table is indexed by the global id *)
let globalsWritten: (varinfo IH.t) IH.t = IH.create 117
let currentGlobalsWritten: (varinfo IH.t) ref = ref (IH.create 117)


(** The transitive closure of the globals written *)
let globalsWrittenTransitive: (varinfo IH.t) IH.t = IH.create 117

let globalsRead: (varinfo IH.t) IH.t = IH.create 117
let currentGlobalsRead: (varinfo IH.t) ref = ref (IH.create 117)

let globalsReadTransitive: (varinfo IH.t) IH.t = IH.create 117


let getGlobalsWrittenTransitive (f: varinfo): varinfo list = 
  try
    let glob_written_trans = 
      IH.find globalsWrittenTransitive f.vid 
    in
    IH.fold
      (fun _ g acc -> g :: acc)
      glob_written_trans
      []
  with Not_found -> [] (* not a defined function *)

let getGlobalsReadTransitive (f: varinfo) = 
  try
    let glob_read_trans = 
      IH.find globalsReadTransitive f.vid 
    in
    IH.fold
      (fun _ g acc -> g :: acc)
      glob_read_trans
      []
  with Not_found -> []
  
let considerType (t: typ) : bool = 
  true

let considerVariable (v: varinfo) : bool = 
  true

class gwVisitorClass : cilVisitor = object (self) 
  inherit nopCilVisitor

  method vexpr = function
      Lval (Var v, _) when v.vglob && considerVariable v -> 
        IH.replace !currentGlobalsRead v.vid v;
        DoChildren

      (* We pretend that when we see the address of a global, we are reading 
       * from the variable. Note that these variables will not be among those 
       * that we "considerVariable" so, there will be no writing to them *)
    | StartOf (Var v, NoOffset) 
    | AddrOf (Var v, NoOffset) when treatAddressOfAsRead && v.vglob -> 
        IH.replace !currentGlobalsRead v.vid v;
        DoChildren 

    | _ -> DoChildren

  method vinst = function
      Set ((Var v, _), _, _) 
    | Call (Some (Var v, _), _, _, _) when v.vglob && considerVariable v -> 
        IH.replace !currentGlobalsWritten v.vid v;
        (* When we write a global, we also consider that we are reading it. 
         * This is useful if the global is not written on all paths *)
        IH.replace !currentGlobalsRead v.vid v; 
        DoChildren
    | _ -> DoChildren
end

let gwVisitor = new gwVisitorClass

(** Functions can be defined or just declared *)
type funinfo = 
    Decl of varinfo
  | Def of fundec

(* All functions indexed by the variable ID *)
let allFunctions: funinfo IH.t = IH.create 117

let nopVar = makeVarinfo false "nopVar" intType
  
let nopInstr = (Instr ((Set ((Var nopVar, NoOffset), Lval (Var nopVar, NoOffset), {line = -1; file = "nop"; byte = 0} )) :: []))
  
(** Compute the SSA form *)
let fundecToCFGInfo (fdec: fundec) : S.cfgInfo = 
  (* Go over the statments and make sure they are numbered properly *)
  let count = ref 0 in

  List.iter (fun s -> s.sid <- !count; incr count) fdec.sallstmts;

  let start: stmt = 
    match fdec.sbody.bstmts with
      [] -> E.s (E.bug "Function %s with no body" fdec.svar.vname)
    | fst :: _ -> fst
  in
  if start.sid <> 0 then 
    E.s (E.bug "The first block must have index 0");
  
  let ci = 
    { S.name  = fdec.svar.vname;
      S.start = start.sid;
      S.size  = !count;
      S.successors = Array.make !count [];
      S.predecessors = Array.make !count [];
      S.blocks = Array.make !count { S.bstmt = start; 
                                     S.instrlist = [];
                                     S.reachable = true;
                                     S.livevars = [] };
      S.nrRegs = 0;
      S.regToVarinfo = Array.make 0 dummyFunDec.svar;
    } 
  in
  (* Map a variable to a register *)
  let varToRegMap: S.reg IH.t = IH.create 117 in 
  let regToVarMap: varinfo IH.t = IH.create 117 in 
  let varToReg (v: varinfo) : S.reg = 
    try IH.find varToRegMap v.vid
    with Not_found -> 
      let res = ci.S.nrRegs in 
      ci.S.nrRegs <- 1 + ci.S.nrRegs;
      IH.add varToRegMap v.vid res;
      IH.add regToVarMap res v;
      res
  in
  (* For functions, we use the transitively computed set of globals and 
   * locals as the use/def *)
  Usedef.getUseDefFunctionRef := 
    (fun f ->
      match f with 
      |  Lval (Var fv, NoOffset) -> 
          let varDefs = ref VS.empty in 
          let varUsed = ref VS.empty in 
          (try 
            let gw = IH.find globalsWrittenTransitive fv.vid in
            IH.iter 
              (fun _ g -> varDefs := VS.add g !varDefs) gw
          with Not_found -> (* Do not have a definition for it *)
            ());
          (* Now look for globals read *)
          (try 
            let gr = IH.find globalsReadTransitive fv.vid in 
            IH.iter
              (fun _ g -> varUsed := VS.add g !varUsed) gr
          with Not_found -> ());
          !varUsed, !varDefs
      | _ -> VS.empty, VS.empty);

  Usedef.considerVariableUse := 
    (fun v -> considerVariable v);
  Usedef.considerVariableDef := 
    (fun v -> considerVariable v);
  Usedef.considerVariableAddrOfAsUse := 
    (fun v -> treatAddressOfAsRead);

  (* Filter out the variables we do not care about *)
  let vsToRegList (vs: VS.t) : int list = 
    VS.fold (fun v acc -> (varToReg v) :: acc) vs [] 
  in
  List.iter 
    (fun s -> 
      ci.S.successors.(s.sid) <- List.map (fun s' -> s'.sid) s.succs;
      ci.S.predecessors.(s.sid) <- List.map (fun s' -> s'.sid) s.preds;
      ci.S.blocks.(s.sid) <- begin
        let instrs: (S.reg list * S.reg list) list = 
          match s.skind with 
            Instr il -> 
              (* Each instruction is transformed independently *)
              List.map (fun i -> 
                let vused, vdefs = Usedef.computeUseDefInstr i in 
                (vsToRegList vdefs, vsToRegList vused)) il
                
          | Return (Some e, _) 
          | If (e, _, _, _) 
          | Switch (e, _, _, _) ->
              let vused = Usedef.computeUseExp e in 
              [ ([], vsToRegList vused) ]
                
          | Break _ | Continue _ | Goto _ | Block _ | Loop _ | Return _ -> [ ]
          | TryExcept _ | TryFinally _ -> assert false
        in
        { S.bstmt = s;
          S.instrlist = instrs;
          S.livevars = []; (* Will be filled in later *)
          S.reachable = true; (* Will be set later *)
        }
      end
    ) fdec.sallstmts;

  (* Set the mapping from registers to variables *)
  ci.S.regToVarinfo <-
    Array.make ci.S.nrRegs dummyFunDec.svar;
  IH.iter (fun rid v -> 
    ci.S.regToVarinfo.(rid) <- v) regToVarMap;
  ci

(* Compute strongly-connected components *)
let stronglyConnectedComponents (cfg: S.cfgInfo) : bool -> S.sccInfo = 
  S.stronglyConnectedComponents cfg 


let globalsDumped = IH.create 117


(** We print variable names in a special way *)
let variableName (v: varinfo) (freshId: int) = 
  (if freshId = 0 then 
    v.vname
  else
    v.vname ^ "____" ^ string_of_int freshId) 
  
(** Use a hash table indexed by varinfo *)
module VH = Hashtbl.Make(struct 
                           type t = varinfo 
                           let hash (v: varinfo) = v.vid
                           let equal v1 v2 = v1.vid = v2.vid
                         end)
(* should have been just a IH ? *)
module IHT = Hashtbl.Make(struct 
			   type t = int
			   let hash (v:int) = v
			   let equal v1 v2 = v1 = v2
			 end)

let vhToList (vh: 'a VH.t) : (varinfo * 'a) list = 
  VH.fold (fun v id acc -> (v, id) :: acc) vh []

let debugRename = false

(** We define a new printer *)
class absPrinterClass (callgraph: CG.callgraph) = 

  let lastFreshId= ref 0 in 
    
  (* freshVarId returns at least 1 *)
  let freshVarId () = incr lastFreshId; !lastFreshId in
    
    
object (self) 
  inherit defaultCilPrinterClass as super
        
    val mutable idomData: stmt option IH.t = IH.create 117

    val mutable cfgInfo: S.cfgInfo option = None 

    val mutable sccInfo: S.sccInfo option = None

    val mutable currentFundec = dummyFunDec

        (** For each block end, a mapping from IDs of variables to their ID 
         * at the end of the block *)
    val mutable blockEndData: int VH.t array = 
      Array.make 0 (VH.create 117)

    val mutable blockEndDataVar: (varinfo ref) VH.t array = 
      Array.make 0 (VH.create 117)

        (** For each block start, remember the starting newFreshId as we 
         * start the block *)
    val mutable blockStartData: int array = 
      Array.make 0 (-1) 

    val mutable varRenameState: int VH.t = VH.create 117

    val mutable varRenameVar: (varinfo ref) VH.t = VH.create 117

          (* All the fresh variables *)
    val mutable freshVars: string list = []

          (* The uninitialized variables are those that are live on input but 
           * not globals or formals. *)
    val mutable uninitVars: string list = []

    method private initVarRenameState (b: S.cfgBlock) =
      VH.clear varRenameState; 
      VH.clear varRenameVar; 
      let cfgi = 
        match cfgInfo with
          None -> assert false
        | Some cfgi -> cfgi
      in
        
      (* Initialize it based on the livevars info in the block *)
      List.iter 
        (fun (rid, defblk) -> 
          let v = cfgi.S.regToVarinfo.(rid) in
          if defblk = b.S.bstmt.sid then
            (* Is a phi variable or a live variable at start *)
            if defblk = cfgi.S.start then begin
              (* For the start block, use ID=0 for all variables, except the 
               * locals that are not function formals. Those are fresh 
               * variables. *)
              let isUninitializedLocal = 
                not v.vglob &&
                (not (List.exists (fun v' -> v'.vid = v.vid) 
                        currentFundec.sformals)) in
		VH.add varRenameState v 0;
		VH.add varRenameVar v (ref v); (* don't need it? *) 
              let vn = self#variableUse varRenameState v in
              if isUninitializedLocal then 
                uninitVars <- vn :: uninitVars;
            end else begin
	      let newID = freshVarId () in
		VH.add varRenameState v newID;
(*		printf "initVarRenameState: livevar if info adding %d %s\n" newID v.vname;  *)
		if (newID != 0) then 
		  begin
(*		    let nv =  makeLocalVar currentFundec (variableName v newID) v.vtype in   *)
		    let nv = copyVarinfo v (variableName v newID) in  
(*		      if (measureFun "VH.mem" VH.mem varRenameVar v) then VH.replace varRenameVar v (ref nv)
		      else VH.add varRenameVar v (ref nv); *)
		      VH.add varRenameVar v (ref nv);
		      ()
		  end;
	    	let vn = self#variableUse varRenameState v in
		  freshVars <- vn :: freshVars
            end
          else begin 
            let fid = 
              try  VH.find blockEndData.(defblk) v
              with Not_found -> 
(*                E.s (E.bug "In block %d: Cannot find data for variable %s in block %d"
                       b.S.bstmt.sid v.vname defblk) *)
		(Pretty.printf "DANGERIOUS: In block %d: Cannot find data for variable %s in block %d\n" b.S.bstmt.sid v.vname defblk;
		 100)
            in
              VH.add varRenameState v fid;
(*	      printf "else adding %d %s\n" fid v.vname; *)
	      flush stdout;
	      if (fid != 0) then 
		begin

(*		  let nv =  makeLocalVar currentFundec (variableName v fid) v.vtype in   *)
		  let nv = copyVarinfo v (variableName v fid) in 
		    (* if (measureFun "VH.mem" VH.mem varRenameVar v) then VH.replace varRenameVar v (ref nv)
		    else VH.add varRenameVar v (ref nv); *)
		    VH.add  varRenameVar v (ref nv);
		    ()
		end;
	  end)
        b.S.livevars;
      
      if debugRename then 
        ignore (E.log "At start of block %d:\n   @[%a@]\n"
                  b.S.bstmt.sid
                  (docList ~sep:line
                     (fun (v, id) -> 
                       dprintf "%s: %d" v.vname id))
                  (vhToList varRenameState));
      ()
    
    (** This is called for reading from a variable we consider (meaning that 
     * its address is not taken and has the right type) *)
    method private variableUse ?(print=true) 
                               (state: int VH.t) (v: varinfo) : string = 
      let freshId = 
        try  VH.find state v
        with Not_found -> 
	  (List.iter (fun (v,id) -> 
			(Pretty.printf "%s id: %d\n" v.vname id; ()))
	     (vhToList state);
	   E.s (E.bug "%a: varUse: varRenameState does not know anything about %s" 
                  d_loc !currentLoc v.vname ))
      in
      if debugRename && print then 
        ignore (E.log "At %a: variableUse(%s) : %d\n" 
                  d_loc !currentLoc v.vname freshId);
      variableName v freshId
        
    (* probably need to add some crap to globals *)
    method private variableDef (state: int VH.t) (v: varinfo) : string = 
      assert (not v.vaddrof);
      let newid = freshVarId () in 
      VH.replace state v newid;
(*	printf "replacing %d %s \n" newid v.vname;  *)
      if debugRename then 
        ignore (E.log "At %a: variableDef(%s) : %d\n" 
                  d_loc !currentLoc v.vname newid);
      let n = self#variableUse ~print:false state v in
      freshVars <- n :: freshVars;
      n

    method private variableLocalUse (state: int VH.t) 
      (stateLocal: (varinfo ref) VH.t) (v:varinfo) : varinfo =
      let intId = 
        try VH.find state v
        with Not_found -> 
	  ( Pretty.printf "********* STATE HASH ********* \n";
	    List.iter (fun (v,id) -> 
			(Pretty.printf "%s id: %d\n" v.vname id; ()))
	     (vhToList state);
	    Pretty.printf "********* END HASH ********* \n";
	    flush stdout;
	    E.s (E.bug "%a: varLocalUse: varRenameState does not know anything about %s" 
                   d_loc !currentLoc v.vname ))
	   in
      let freshId =    
	if (intId = 0) then (ref v)
	else 
	  try  VH.find stateLocal v
          with Not_found -> 
	    let printOnce v1 v2 = ignore (E.bug "%s %s" v1.vname (!v2).vname); () in 
	      (VH.iter printOnce stateLocal); 
              E.s (E.bug "%a: varUse: varLocalState does not know anything about %s id: %d stateLocal's length %d" 
                     d_loc !currentLoc v.vname intId (VH.length stateLocal) );
	      
      in
(*	printf "giving out %d %s for %s\n" intId (!freshId).vname v.vname;  *)
	!freshId

    (* generate new local variables ? *)
    method private variableLocalDef (state: int VH.t) (stateLocal: (varinfo ref) VH.t) 
      (v: varinfo) : varinfo = 
      assert (not v.vaddrof); 
      let newid = freshVarId () in
(*	let nv = makeLocalVar currentFundec (variableName v newid) v.vtype in  *)
	let nv = copyVarinfo v (variableName v newid) in 
	  VH.replace stateLocal v (ref nv);
	  VH.replace state v newid;
	  if debugRename then 
            ignore (E.log "At %a: variableDef(%s) : %d\n" 
                      d_loc !currentLoc v.vname newid);
	  let n = self#variableLocalUse state stateLocal v in
	    freshVars <- n.vname :: freshVars;
	    n
	    

            
    method convertExp e = 
      if debug then ignore (printf "in convert exp %a\n" d_exp e);
      match e with
	| Const _ as e1 -> e1
	| BinOp (bop, e1, e2, l) -> BinOp(bop, self#convertExp e1, self#convertExp e2, l)
	| UnOp (uop, e1, l) -> UnOp (uop, self#convertExp e1, l)
	| CastE (t, e) -> CastE (t, self#convertExp e)
	| Lval (Var v, off) (* when considerVariable v *)->
	    let nv = (self#variableLocalUse varRenameState varRenameVar v) in
	    Lval (Var nv, off)
	| Lval (Mem e2, off1) as e1 ->
(*	    printf "in mem access\n"; *)
	      Lval (Mem (self#convertExp e2), off1)
	| AddrOf (Var v, off) ->
	    let nv = (self#variableLocalUse varRenameState varRenameVar v) in
	      AddrOf (Var nv, off)
	| _ -> 
	    printf "not supported %a\n" d_exp e;
	    e (* ignoreing  Lval _ -> text "(@rand)" 
		      | AddrOf (Var v, NoOffset) 
		      | StartOf (Var v, NoOffset) *)     
            
    method convertExpAssign e = 
      match e with 
	  CastE (t, e) -> CastE (t, self#convertExpAssign e)
	| Lval (Var v, off) ->
	    let nv = (self#variableLocalDef varRenameState varRenameVar v) in
	      Lval (Var nv, off)
	| Lval (Mem e2, off) ->
	    Lval (Mem (self#convertExpAssign e2), off)
	| Const _
	| UnOp _
	| BinOp _ 
	| _ -> (printf "BAD ASSIGN: %a; possibly funky memory assign \n" d_exp e; e)

    method addSsaAttrib (v: varinfo) (n: string) : unit = 
      let a = Attr ("ssa", (AStr (n))::[])
      in
	v.vattr <- dropAttribute "ssa" v.vattr;
	v.vattr <- addAttribute a v.vattr

    method convertInstr (i: instr) : instr =
(*      printf "trying to convert %a \n" d_instr i; *)
      flush stdout;
      let convertCall  (dest: lval option) 
                    (f: varinfo) (args: exp list) (l: location) = 
	 currentLoc := l;
	let gwt: varinfo list = getGlobalsWrittenTransitive f in
        let grt: varinfo list = getGlobalsReadTransitive f in




	  (* 1. get SSA Names 
	     2. convert to expressions 
	     3. append to function arguments *)
(*	  printf "globals read by function "; *)

	let nArgs = List.map self#convertExp args in

	  let grtLval = List.map (function vi -> 
				       let grtNv = self#variableLocalUse varRenameState varRenameVar vi in
(*					 ignore(printf "%s %s" vi.vname grtNv.vname); *)
					 grtNv.vattr <- addAttribute (Attr ("gr", []))  grtNv.vattr;
					 let nVi = copyVarinfo vi (vi.vname) in
					   nVi.vattr <- addAttribute (Attr ("gr", []))  nVi.vattr;
					 (Lval (Var grtNv, NoOffset) :: Lval (Var nVi, NoOffset) :: [])
				    ) grt in 


	  (* get rid of functions read *)
	  let grtLval = List.filter (fun vi ->
				       (match vi with
					    (Lval (Var grtNv, NoOffset) :: Lval (Var nVi, NoOffset) :: []) ->
					      (match nVi.vtype with 
						   TFun _ -> false
						 | _ -> true)
					  | _ -> false)
				    ) grtLval in  

(*	  printf "\n globals written by function "; *)
	   let gwtLval = List.map (function vi -> 
				       let gwtNv = self#variableLocalDef varRenameState varRenameVar vi in
(*					 ignore(printf "%s %s" vi.vname gwtNv.vname); *)
					 gwtNv.vattr <- addAttribute (Attr ("gw", []))  gwtNv.vattr;
					 let nVi = copyVarinfo vi (vi.vname) in
					   nVi.vattr <- addAttribute (Attr ("gw", []))  nVi.vattr;
					 (Lval (Var gwtNv, NoOffset):: Lval (Var nVi, NoOffset) :: [])
				  ) gwt in
(*	     printf "\n"; *)
	     
	
	     (* check whether we allocate to a function *)
	     (* exceptions are allocators such as malloc / kmalloc *)
	     if debug then 
	       List.iter (fun vi ->
			    (match vi with
				 (Lval (Var grtNv, NoOffset) :: Lval (Var nVi, NoOffset) :: []) ->
				   (match nVi.vtype with 
					TFun _ ->  failwith ("ssaConvert: wrote into a function value "^nVi.vname)
				      | _ -> ())
			       | _ -> ())
			 ) gwtLval;
	
	     let gwtLval = List.flatten gwtLval in
	     let grtLval = List.flatten grtLval in

	(* don't rename f *)
	(* let f = (self#variableLocalDef varRenameState varRenameVar f) in  *)

	 let nDest = (match dest with
			  Some (Var v, NoOffset) ->
(*			    printf "localdef in call\n"; *)
			    let v = (self#variableLocalDef varRenameState varRenameVar v) in
			      Some (Var v, NoOffset)
			| None -> None
			| _ -> dest) in
	   Call (nDest, Lval (Var f, NoOffset) , nArgs @ grtLval @ gwtLval, l) in
	match i with
	  | Set ((Var v, NoOffset), e, l) when considerVariable v -> 
	      currentLoc := l;
	      let nE = self#convertExp e in 
	      let v = (self#variableLocalDef varRenameState varRenameVar v) in
		Set ((Var v, NoOffset), nE, l)
	  | Call (Some (Var v, NoOffset), 
		  Lval (Var f, NoOffset), args, l) when considerVariable v ->
	      convertCall (Some (Var v, NoOffset)) f args l
	  | Call (des, Lval (Var f, NoOffset), args, l) -> 
              convertCall des f args l
	  | Set ((Var v, (Field (fi, offs))), e, l) ->
	      (* field access; need to think about more *)
	      let nE = self#convertExp e in 
	      let v = (self#variableLocalDef varRenameState varRenameVar v) in
	      Set ((Var v, (Field (fi, offs))), nE, l)
	  | Set ((Mem e, offs), e1, l) -> 
	      (* memory access; need to think about more *)
(*	      let nE1 = self#convertExpAssign e in   *)
	      let nE1 = self#convertExp e in  
	      let nE2 = self#convertExp e1 in
		Set ((Mem nE1, offs), nE2, l)
	  | i1 -> 
	      printf "not supported %a\n" d_instr i1;
	      i1
	     
    method  convertStmt (s: stmt) : stmt = 
      (* printf "doing stmt: %d %a\n" s.sid d_stmt s; *)
      currentLoc := get_stmtLoc s.skind;
      (* Initialize the renamer for this statement *)
      if (s.sid = -1) then s
      else
	begin 

      lastFreshId := blockStartData.(s.sid); 
      if debugRename then 
        ignore (E.log "In ConvertStmt: Initialize the renamer for block %d to %d \n%a\n" 
                  s.sid !lastFreshId d_stmt s);
      assert (!lastFreshId >= 0);
       let cfgi = 
        match cfgInfo with
          Some cfgi -> cfgi
        | None -> assert false
      in
      let blk: S.cfgBlock = cfgi.S.blocks.(s.sid) in
      assert (blk.S.bstmt == s);
      self#initVarRenameState blk;

      let phivars: varinfo list = 
        List.fold_left
          (fun acc (i, defblk) -> 
            if defblk = s.sid then 
              cfgi.S.regToVarinfo.(i) :: acc
            else 
              acc)
          []
          blk.S.livevars 
      in
      (* do not emit phi for start block *)
      let phivars: varinfo list = 
        if s.sid = cfgi.S.start then 
          []
        else
          phivars
      in
	(* Get the predecessors information *)
      let getPhiAssignment (v: varinfo) : instr = 
        (* initVarRenameState has already set the state for the phi register *)
        let lhs: lval = (Var (self#variableLocalUse varRenameState varRenameVar v), NoOffset) in 
        let rhs: exp list = 
          List.map
            (fun p -> 
               let vn = self#variableLocalUse blockEndData.(p) blockEndDataVar.(p) v in
               Lval (Var vn, NoOffset))
            cfgi.S.predecessors.(s.sid) 
        in
	  Call (Some lhs, Const (CStr "phi"), rhs, !currentLoc)
      in
	

      (* Lookup its dominator *)
      let idom: doc = 
        match Dominators.getIdom idomData s with 
          Some dom -> num dom.sid
        | None -> nil
      in
      let succs = List.filter (fun s' -> cfgi.S.blocks.(s'.sid).S.reachable) s.succs in
      let preds = List.filter (fun s' -> cfgi.S.blocks.(s'.sid).S.reachable) s.preds in
      let phiCalls = List.map getPhiAssignment phivars in
      let newPhis = mkStmt (Instr phiCalls) in
	(match s.skind with 
	   | Instr il -> 
               if (cfgi.S.blocks.(s.sid).S.reachable) then begin
		 s.skind <- (Instr (List.map
				      (fun i -> self#convertInstr i)
				      il))
	       end
	   | Block b -> b.bstmts <- List.map (self#convertStmt) b.bstmts
	   | Goto (sG, lo) -> 
(*	       sG := self#convertStmt !sG; the statements in SG will handled elsewhere *)
	       s.skind <- Goto (sG, lo)
	   | Return (what, lo) -> 
	       let gwt: varinfo list = 
		 getGlobalsWrittenTransitive currentFundec.svar
	       in

	       let setSubInstr = List.map (fun vi -> 
					     let gwtNv =  self#variableLocalUse varRenameState varRenameVar vi in
					     let sublv = copyVarinfo vi (vi.vname ^ "__caller") in
					       Set ((Var sublv, NoOffset), Lval (Var gwtNv, NoOffset), lo)) gwt in

	       begin
		 ( match what with
		     None -> ()
		   | Some (e1) ->
		       s.skind <- Return (Some (self#convertExp e1), lo));
		 if ((List.length setSubInstr) > 0) then
		       begin
			 let sCopy = mkStmt (s.skind) in
			 let newInstr = mkStmt (Instr setSubInstr) in
			 let nBlock = {battrs = []; bstmts = newInstr::sCopy::[]} in
			   s.skind <- Block nBlock;
		       end
	       end
	   |  If(e, b1, b2, l) -> 
		if ((List.length b2.bstmts) = 0) then
		  (b2.bstmts <- (mkStmt nopInstr) :: []; 
		   (* need to do let's to force ordering! *)
		   let ne = self#convertExp e in
		   let nb1 =  self#convertBlock b1 in
		     s.skind <- If (ne, nb1, b2, l))
		else
		  let ne  = self#convertExp e in
		  let nb1 =  self#convertBlock b1 in
		  let nb2 = self#convertBlock b2 in
		    s.skind <- If (ne, nb1, 
				   nb2, l)
	   | Loop (b, l, co, br) ->
		 s.skind <- Loop (self#convertBlock b, l, co, br)
	   | _ -> E.s (E.unimp "try except"));
	let sCopy = mkStmt (s.skind) in

	let nBlock: block = {battrs = []; bstmts = newPhis::sCopy::[];} in
	if (phivars <> []) then (s.skind <- Block nBlock);
	s
    end

	    
    method convertBlock (b: block) =
      b.bstmts <- (List.map (self#convertStmt) b.bstmts);
      b
        
	  
                
    method dGlobal (out: out_channel) (g: global) : unit = 
      match g with 
        GFun (fdec, l) -> 
	  if debug then ignore (Pretty.printf "before ssa :%a \n" d_global g);
          currentFundec <- fdec;
          if debugRename then 
            ignore (E.log "Renaming for function %s\n" fdec.svar.vname);

          (* Make sure we use one return at most *)
          Oneret.oneret fdec;
          
          (* Now compute the immediate dominators. This will fill in the CFG 
           * info as well *)
          idomData <- Dominators.computeIDom fdec;
          
          (** Get the callgraph node for this function *)
          let cg_node: CG.callnode = 
            try H.find callgraph fdec.svar.vname
            with Not_found -> E.s (E.bug "Cannot find call graph info for %s"
                                     fdec.svar.vname)
          in
          
          (** Get the globals read and written *)
          let glob_read =
            (try IH.find globalsRead fdec.svar.vid
            with Not_found -> assert false) in
          let glob_read_trans = 
            (try IH.find globalsReadTransitive fdec.svar.vid
            with Not_found -> assert false) in 
          
          
          let glob_written = 
            (try IH.find globalsWritten fdec.svar.vid
            with Not_found -> assert false) in 
          let glob_written_trans = 
            (try IH.find globalsWrittenTransitive fdec.svar.vid
            with Not_found -> assert false) in 
          
          (* Compute the control flow graph info, for SSA computation *)

	   
            let cfgi = S.prune_cfg (fundecToCFGInfo fdec) in   
	      cfgInfo <- Some cfgi;
          (* Call here the SSA function to fill-in the cfgInfo *)
          S.add_ssa_info cfgi;
          (* Compute strongly connected components *)
          let scc: S.sccInfo = 
            stronglyConnectedComponents cfgi false in 
          sccInfo <- Some scc;

          (* Now do the SSA renaming. *)
          blockStartData <- Array.make cfgi.S.size (-1);
          blockEndData <- Array.make cfgi.S.size (VH.create 117);
          blockEndDataVar <- Array.make cfgi.S.size (VH.create 117);

          lastFreshId := 0;

          freshVars <- [];
          uninitVars <- [];
          
          if debugRename then 
            ignore (E.log "Starting renaming phase I for %s\n" 
                      fdec.svar.vname);

          Array.iteri (fun i (b: S.cfgBlock) -> 
            (* compute the initial state *)
            blockStartData.(i) <- !lastFreshId;

            if debugRename then 
              ignore (E.log "Save the rename state for block %d to %d\n"
                        i !lastFreshId);

            (* Initialize the renaming state *)
            self#initVarRenameState b; 
          
            (* Now scan the block and keep track of the definitions. This is 
             * a huge hack. We try to rename the variables in the same order 
             * in which we will rename them during actual printing of the 
             * block. It would have been cleaner to print the names of the 
             * variables after printing the function.  *)
            (match b.S.bstmt.skind with 
              Instr il -> begin
                List.iter
                  (fun i -> 
                    let doCall (dest: varinfo option) (f: varinfo) : unit = 
                      let gwt: varinfo list = 
                        getGlobalsWrittenTransitive f in
                      let gwt' = 
                        match dest with 
                          Some v -> 
                            gwt @ [ v ] 
                        | _ -> gwt 
                      in
                      List.iter (fun v -> 
(*				   printf "variable local def in iter for calls\n"; *)
                        ignore (self#variableLocalDef varRenameState varRenameVar v))
                        gwt'
                    in
                    match i with 
                      Set ((Var v, off), _, l)
                        when considerVariable v ->
                          currentLoc := l; (* originally it was no offset *)
(*			  printf "variable local def in iter for Sets %s\n" v.vname; *)
                          ignore (self#variableLocalDef varRenameState varRenameVar v)
                    | Call (Some (Var v, NoOffset), 
                            Lval (Var f, NoOffset), _, l) 
                      when considerVariable v ->
                        currentLoc := l;
                        doCall (Some v) f


                    | Call (_, 
                            Lval (Var f, NoOffset), _, l) -> 
                              currentLoc := l;
                              doCall None f

                    | _ -> ())
                  il
              end
                    
            | _ -> (* No definitions *)
                ()
            );
            
            if debugRename then 
              ignore (E.log "At end of block %d:\n   @[%a@]\n"
                        i
                        (docList ~sep:line
                           (fun (v, id) -> 
                             dprintf "%s: %d" v.vname id))
                        (vhToList varRenameState));
            
            blockEndData.(i) <- VH.copy varRenameState;
	    blockEndDataVar.(i) <-  VH.copy varRenameVar; 
		      ) (* Array.iteri (fun i (b: S.cfgBlock) -> *)
          cfgi.S.blocks;

          if debugRename then 
            ignore (E.log "Starting renaming phase II (printing) for %s\n" 
                      fdec.svar.vname);


          (** For each basic block *)
        

        (* The block *)
	self#convertBlock fdec.sbody;  
	if debug then ignore (Pretty.printf "after ssa :%a \n" d_global g);
        (* The end *)
	()
	  (* Emit the globals whose address is not taken *)
	| GVarDecl (vi, l) | GVar (vi, _, l) when 
            not vi.vaddrof && isIntegralType vi.vtype 
            && not (IH.mem globalsDumped vi.vid) 
	    -> 
            IH.add globalsDumped vi.vid ();
	| _ -> ()
  end


let arithAbs (absPrinter : cilPrinter) (g: global) = 
  dumpGlobal absPrinter !arithAbsOut g

(* drop the ssa naming 
   i.e. v____232 -> v
*)
(* we assume variables are named like blah@blah____3 *)
let getSsaNum (s:string) : (string * int) = 
  let r = Str.regexp_string "____" in
  let sl = Str.split r s in
    if (List.length sl = 2) then
      let strNum = List.hd (List.tl sl) in
	try (List.hd sl, int_of_string strNum)
	with _ -> 
	  begin
	    Pretty.printf "getSsaNum strNum = %s s = %s\n" strNum  s;
	    (List.hd sl, 0)
	  end
(*    else if (List.length sl > 2) then failwith "name has ____ in it" *)
    else (s, -1)

let convertToSsa = 
  (function (f : file) ->
     (* Compute the call graph *)
     let graph = CG.computeGraph f in
     if !hasbeenconverted then graph else 
     begin 
       hasbeenconverted := true;
       
       (* Compute the globals written by each function *)
      IH.clear globalsWritten;
      IH.clear globalsWrittenTransitive;
      IH.clear globalsRead;
      IH.clear allFunctions;


      (* Compute the globals read and written *)
      iterGlobals f
        (function
             GFun(fdec, _) -> 
               IH.replace allFunctions fdec.svar.vid (Def fdec);
               currentGlobalsRead := IH.create 117;
               IH.add globalsRead fdec.svar.vid !currentGlobalsRead;
               currentGlobalsWritten := IH.create 117;
               IH.add globalsWritten fdec.svar.vid !currentGlobalsWritten;
               ignore (visitCilBlock gwVisitor fdec.sbody)
		 
           | GVarDecl (vd, _) when isFunctionType vd.vtype &&
               not (IH.mem allFunctions vd.vid) 
               -> 
              IH.add allFunctions vd.vid (Decl vd)
          | _ -> ());

      (* Now do transitive closure of the globals written by each function *)
      (* Initialize each function with the globals it writes itself *)
      IH.iter 
        (fun fid gw -> 
          IH.add globalsWrittenTransitive fid (IH.copy gw))
        globalsWritten;

      IH.iter 
        (fun fid gr -> 
          IH.add globalsReadTransitive fid (IH.copy gr))
        globalsRead;

      (* A work list initialized with all functions, that are defined *)
      let worklist: int Queue.t = Queue.create () in
      IH.iter (fun fid finfo -> 
        match finfo with 
          Def _ -> Queue.add fid worklist
        | _ -> ())
        
        allFunctions;

      (* Now run until we reach a fixed point *)
      let rec fixedpoint () = 
        try 
          let next = Queue.take worklist in 
          (* Get the function info for this one *)
          let finfo = 
            try IH.find allFunctions next
            with Not_found -> 
              E.s (E.bug "Function id=%d not in allFunctions" next)
          in
          (* If this is just a declaration, we ignore *)
          (match finfo with 
            Decl _ -> ()
          | Def fdec -> begin
              (* Find the callnode for it *)
              let cnode: CG.callnode = 
                try H.find graph fdec.svar.vname
                with Not_found -> 
                  E.s (E.bug "Function %s does not have a call node" 
                         fdec.svar.vname)
              in
              (* Union in all the variables modified by the functions this 
               * calls. Remember if we made a change. If we do, we add to the 
               * worklist the callers of this one. *)
              let changeMade = ref false in 

              (* Our written *)
              let ourWritten = 
                try IH.find globalsWrittenTransitive fdec.svar.vid
                with Not_found -> 
                  E.s (E.bug "Function %s not in globalsWrittenTransitive"
                      fdec.svar.vname)
              in

              (* Our read *)
              let ourRead = 
                try IH.find globalsReadTransitive fdec.svar.vid
                with Not_found -> 
                  E.s (E.bug "Function %s not in globalsReadTransitive"
                      fdec.svar.vname)
              in
              H.iter
                (fun n cn -> 
                  (* Get the callee's written *)
                  (try 
                    let callee_written = 
                      IH.find globalsWrittenTransitive cn.CG.cnInfo.vid in
                    IH.iter 
                      (fun gwid gw -> 
                        if not (IH.mem ourWritten gwid) then begin
                          IH.add ourWritten gwid gw;
                          changeMade := true
                        end)
                      callee_written;
                  with Not_found -> (* Callee not defined here *)
                    ());

                  (* Get the callee's read *)
                  (try 
                    let callee_read = 
                      IH.find globalsReadTransitive cn.CG.cnInfo.vid in
                    IH.iter 
                      (fun grid gr -> 
                        if not (IH.mem ourRead grid) then begin
                          IH.add ourRead grid gr;
                          changeMade := true
                        end)
                      callee_read;
                  with Not_found -> (* Callee not defined here *)
                    ());


                )
                cnode.CG.cnCallees;

              if !changeMade then begin
                H.iter 
                  (fun _ caller -> Queue.add caller.CG.cnInfo.vid worklist)
                  cnode.CG.cnCallers
              end
          end);

          fixedpoint ();
                
        with Queue.Empty -> ()
      in
      fixedpoint ();

      let absPrinter :> cilPrinter = new absPrinterClass graph in 
      IH.clear globalsDumped;
      iterGlobals f (arithAbs absPrinter);

      (* compute SCC for the call-graph *)
      let nodeIdToNode: CG.callnode IH.t = IH.create 117 in
      let funidToNodeId: int IH.t = IH.create 117 in 
      let nrNodes = ref 0 in 
      let mainNode = ref 0 in 
      H.iter 
        (fun vn cn -> 
          if vn= "main" then mainNode := !nrNodes;
          IH.add nodeIdToNode !nrNodes cn;
          IH.add funidToNodeId cn.CG.cnInfo.vid !nrNodes;
          incr nrNodes) graph;

      let ci: S.cfgInfo = 
        { S.name = "call-graph";
          S.start = !mainNode;
          S.size = !nrNodes;
          S.successors = Array.make !nrNodes [];
          S.predecessors = Array.make !nrNodes [];
          S.blocks = Array.make !nrNodes { S.bstmt = mkEmptyStmt ();
                                           S.instrlist = [];
                                           S.livevars = [];
                                           S.reachable = true };
         S.nrRegs = 0;
         S.regToVarinfo = Array.create 0 dummyFunDec.svar;
        }
      in

      let ci = ci in
      nrNodes := 0;
      IH.iter (fun idx cn -> 
        let cnlistToNodeList (cnl: (string, CG.callnode) H.t) : int list = 
          List.map 
            (fun (_, sn) -> 
              try IH.find funidToNodeId sn.CG.cnInfo.vid
              with Not_found -> assert false
            )
            (U.hash_to_list cnl)
        in
        (* we want to construct the callee graph not the caller graph *)
        ci.S.successors.(idx) <- cnlistToNodeList cn.CG.cnCallers;
        ci.S.predecessors.(idx) <- cnlistToNodeList cn.CG.cnCallees; 
           
        ) nodeIdToNode;

      graph
    end
  )
     

(* returns (phiVar, phiArgs) from a phi statement *)
let getPhi s = 
  match s.skind with 
      Instr  ((Call (Some (Var v, NoOffset), Const (CStr "phi"), elist, _)) :: rest) -> 
	let vlist = List.map (fun e -> (match e with Lval (Var v1, NoOffset) -> v1 
	  | _ -> failwith "ssaconvert.getPhi : arguments are not just variables")) elist in
	  (v, vlist)
    | _ -> failwith "ssaConvert.getPhi : not a phi statement\n"  
	

