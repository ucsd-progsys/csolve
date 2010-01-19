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

open Cil
open Misc.Ops

class funVisitor fd = object(self)
  inherit nopCilVisitor

  val shadows = Hashtbl.create 17

  method vvrbl v =
    if v.vglob && not (isFunctionType v.vtype) then
      ChangeTo (Misc.do_memo shadows (fun glob -> makeTempVar fd glob.vtype) v v)
    else
      SkipChildren

  method revertShadows =
       Hashtbl.fold (fun glob shadow is -> Set (var glob, Lval (var shadow), locUnknown) :: is) shadows []
    |> self#queueInstr

  method vstmt = function
    | {skind = Return _} -> self#revertShadows; DoChildren
    | _                  -> DoChildren

  method addShadows =
    Instr (Hashtbl.fold (fun glob shadow is -> Set (var shadow, Lval (var glob), locUnknown) :: is) shadows [])

  method vfunc fd =
    ChangeDoChildrenPost (fd, fun fd -> fd.sbody.bstmts <- (mkStmt self#addShadows) :: fd.sbody.bstmts; fd)
end

class globVisitor = object(self)
  inherit nopCilVisitor

  method vglob = function
    | GFun (fd, _) -> visitCilFunction (new funVisitor fd :> cilVisitor) fd |> ignore; SkipChildren
    | _            -> SkipChildren
end

let copyGlobal = visitCilFile (new globVisitor)
