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

(******************************************************************************)
(******************* Assign Return Values to Local Vars Only ******************)
(******************************************************************************)

open Cil
module Misc = FixMisc open Misc.Ops

class returnVisitor fd = object(self)
  inherit nopCilVisitor

  method vinst = function
    | Call (None, _, _, _)
    | Call (Some (Var _, NoOffset), _, _, _) -> SkipChildren
    | Call (Some lv, e, es, loc) ->
        let tlv = (Var (makeTempVar fd (typeOfLval lv)), NoOffset) in
          ChangeTo ([Call (Some tlv, e, es, loc); Set (lv, Lval tlv, loc)]);
    | _ -> SkipChildren
end

let simpleret (f: file) =
  iterGlobals f begin function
    | GFun (fd, _) -> visitCilFunction (new returnVisitor fd) fd |> ignore
    | _            -> ()
  end
