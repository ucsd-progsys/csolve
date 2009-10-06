(*
 * Modified by Pat Rondon to allocate distinct locals separately.
 *
 * Copyright (c) 2001-2002,
 *  George C. Necula    <necula@cs.berkeley.edu>
 *  Scott McPeak        <smcpeak@cs.berkeley.edu>
 *  Wes Weimer          <weimer@cs.berkeley.edu>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)

(*
 * Pheapify: a program transform that looks over functions, finds those
 * that have local (stack) variables that contain arrays, puts all such
 * local variables into a heap allocated structures, changes all
 * accesses to such variables into accesses to those structures.
 * Don't bother freeing them because we don't care.
 *)
open Cil
open Misc.Ops

let heapifyNonArrays = ref false

(* Does this local var contain an array? *)
let rec containsArray (t:typ) : bool =  (* does this type contain an array? *)
  match unrollType t with
    TArray _ -> true
  | TComp(ci, _) -> (* look at the types of the fields *)
      List.exists (fun fi -> containsArray fi.ftype) ci.cfields
  | _ ->
    (* Ignore other types, including TInt and TPtr.  We don't care whether
       there are arrays in the base types of pointers; only about whether
       this local variable itself needs to be moved to the heap. *)
   false

class heapifyModifyVisitor hvars = object(self)
  inherit nopCilVisitor  (* visit lvalues *)

  method private is_heapified vi =
    List.mem_assoc vi hvars

  method vexpr = function
    | StartOf (Var vi, NoOffset)
    | AddrOf (Var vi, NoOffset) when self#is_heapified vi -> ChangeTo (Lval (Var (List.assoc vi hvars), NoOffset))
    | _                                                   -> DoChildren

  method vlval = function (* should we change this one? *)
    Var(vi), vi_offset when self#is_heapified vi -> (* check list *)
      let hvi = List.assoc vi hvars in (* find corresponding heap var *)
        begin match unrollType vi.vtype with
          | TArray _ ->
              begin match vi_offset with
                | Index (e, o) -> ChangeDoChildrenPost ((Mem (BinOp (PlusPI, Lval (Var hvi, NoOffset), e, hvi.vtype)), o), id)
                | NoOffset     -> ChangeDoChildrenPost ((Var hvi, NoOffset), id)
                | _            -> assert false
              end
          | _ -> ChangeDoChildrenPost ((Mem (Lval (Var hvi, NoOffset)), vi_offset), id)
        end
  | _ -> DoChildren (* ignore other lvalues *)
end

let heapifiedType t =
  match unrollType t with
    | TArray (t, _, _) -> TPtr (t, [])
    | t                -> TPtr (t, [])

class heapifyAnalyzeVisitor f alloc = object
  inherit nopCilVisitor (* only look at function bodies *)

  method vglob = function
    GFun (fundec, funloc) ->
      let hvars = List.filter begin fun vi -> (* a list of the variables that must be on the heap *)
        (* find all local vars with arrays.  If the user requests it,
           we also look for non-array vars whose address is taken. *)
        (containsArray vi.vtype) || (vi.vaddrof && !heapifyNonArrays)
      end fundec.slocals in
      if (hvars <> []) then begin (* some local vars contain arrays *)
        let newvars = List.map (fun vi -> makeLocalVar fundec (vi.vname ^ "#heapify") (heapifiedType vi.vtype)) hvars in
        let varmap  = List.combine hvars newvars in
          fundec.sbody <- visitCilBlock (new heapifyModifyVisitor varmap) fundec.sbody; (* rewrite accesses to local vars *)
          let allocs = List.map (fun (vi, hvi) -> Call (Some (Var hvi, NoOffset), alloc, [SizeOf vi.vtype], funloc)) varmap in
            fundec.sbody.bstmts <- mkStmt (Instr allocs) :: fundec.sbody.bstmts;
            fundec.slocals <- List.filter (fun vi -> not (List.mem vi hvars)) fundec.slocals; (* remove local vars *)
            ChangeTo ([GFun (fundec, funloc)])  (* done! *)
      end else
	DoChildren	(* ignore everything else *)
  | _ -> DoChildren
end

let heapify (f : file) (alloc : exp) =
  visitCilFile (new heapifyAnalyzeVisitor f alloc) f;
  f

let default_heapify (f : file) =
  let alloc_fun = findOrCreateFunc f "malloc" voidType in
  let alloc_exp = Lval (Var alloc_fun, NoOffset) in
    ignore (heapify f alloc_exp)
