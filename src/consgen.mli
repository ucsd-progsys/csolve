
(*
val mk_cons : Wrapper.cilenv -> 
              Ssa_transform.ssaCfgInfo list -> 
              (Ssa_transform.ssaCfgInfo * Wrapper.cilenv * Constraint.t list) Misc.StringMap.t
*)
val mk_cons :  Wrapper.cilenv 
            -> Ssa_transform.ssaCfgInfo list 
            -> (Constraint.wf list * Constraint.t list)
