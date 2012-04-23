PENDING
=======

* chgrp   -- undo, get decent error after removing OPTARG_LOC annot.

Multi-file HTML (branch: multihtml)
-----------------------------------

HEREHEREHEREHERE:
    
    Go through the "Raw String" and replace with meaningful errors
    corresponding to the function calls, derefs etc.

    Hard because of fun-call subtyping?

Nice error messages: line 223, dereference 

  *tmp78
  
  where reSugar (*tmp78) =====> x->foo->bar render error: 
  
    line 223, dereference x->foo->bar not safe.

- Persistent Windows on Variable Click (with same info as hover)

- Links to definitions (like CTAGS): for all globvars, funs, vars(?)


* TESTING

external/gnu-coreutils/src/make.csolve chgrp

./csolve tests/postests/ll0.c




Counterexamples (to be continued) 
---------------------------------

STATUS: alas, for c you get crap like this:
   Counterexamples:
     ERROR: (id 19) @ 19
     `-> s_string_succ: (k_122/Unchecked(VV)) @ 19
         `-> tmp___4_main__SSA__blk_0_1: (k_179/Unchecked(VV)) @ 187
             `-> VV: (k_131/Unchecked(VV)) @ 151
   because Unchecked is the ONLY singleton predicate that implies safety.
   ** could try cubes of size 2, 3, 4 but thats getting expensive
   ** probably want some interactive thing which lets you generate
      counterexamples for specific facts, which are navigated to ... sigh.

1. PRETTIFY (to be continued)

   >> render with C-Types (cf. GOAL below)
       >> field names instead of A90#Ix#6 etc.
       >> short var names   (pages instead of pages_env_alloc -- with links) 
       >> global (all? abstract) locations printed only once

   >> Suppress attributes which F-up the indices, print all quals without break

   >> Wrong type for locations (input loc = false, output = true, so print output.)
       >> e.g. funstore main for A8, A7@size etc.
   
   >> sort qualifiers by name/frequency?
   
   >> export cilenv to annots: 
       >> substitute copyprop & delete duplicates (?)
       >> print simple var names. 'x for old x for current, $x for input.
       >> print substs at end of annot (x := ..., 'x := ..., $x := ..., etc.)

   >> Rig parser to ignore stuff between /* ... */ then fix Ctypes.d_fieldinfo

   >> Dump BSTATS out as JSON, sorted by times.

   >> Error (associate string tags with constraints. "Dereference at line ...")

prunelive OR removing constant-propped vars from WF-envs kills:

../tests/postests/strcpy.c
../tests/postests/adpcm.c
../tests/postests/adpcminiptr.c
../tests/postests/cpj/idea_test.c

because some interesting constant term (e.g. n4_stale = n/4)
is required in a qualifier... all the above are pretty contrived
(and shift-related.)

***************************************************************************

Function Pointers

(a) fun-ptrs:
    -- Create a dummy type ? int/ref/fun
    -- fix pattern match errors
    -- fix callgraph code	[skip calls through funptrs]
    -- fix inferctypes code
    ../main.native funptr0.c
    bind "TRUE" or dummy refinement to lhs of fun-ptr-call assignment
    -- fix consgen code 	[""]

(b) SCALAR-CONSGEN:
    allows multiple finite arrays inside a struct, various other 
    “local” forms of pointer arithmetic refactors arith solving 
    inside inferctypes and elsewhere
	
	tests/multiarr.c

    how to do it _without_ any alias/pointer/shape information? 
    (over scalar variables only!) do i need to go over inferctypes code?
    
(c) cone-of-influence to eliminate bogus definitions...

-----------------------------------------------------------------------------

Constraint Simplification (FixSimplify.ml)

(1) separate const-pred extraction (moving constant bindings into guard)
    from andrey's equality-simplification, perhaps put constpred extraction 
    into "FixConstraint.make_t" ? (will impact strengthening optimization
    in LiquidC -- that should be moved into FixSimplify anyway...)

(2) fix termination problem in equality-simplification
	--> can be deferred and addressed later (just disable this step)

(3) debug k-elimination (run C/ML regrtests)

-- also think about splitting into direct/indirect flows


What is an "ebind" ? (fixpoint)

// This is useful for display too.
Q: Can we precompute a "qualifier implication graph" (Edges means Q1 => Q2)
   and use that to eliminate pointless queries? e.g. v>0 => v>=0 etc. ?

	Hooray! Qualifier.t is already abstract!
		add an explicit identifier? how to match free vars?

	Change "solution" 
		to make it abstract (FixConstraint.soln)
		then make soln map back to original qualifs 

	First, formalize the problem.
		aq = Abstract Qualifier (with template vars)
		cq = Concrete Qualifier (with no template vars)





		Minimization: Given 
		(1) an aq-implication-DAG D
		(2) a qualifier set Q = {(cq1,aq1),...,(cqn,aqn)} 
		Find a "minimal" qualifier set Q' \subseteq Q
		such that 
		(a) for each (cqi,aqi), (cqj,aqj) aqi -/-> aqj
		(b) for each (cqj,aqj) in Q\Q', 
		    there exists (cqi, aqi) in Q' such that
		    aqi -> aqj

		Generalize to hyper-edges in D. 
		Note that even the single-edge case is basically
		"SET-COVER", so can't get "best" solution.

Q: Can you build a "direct" influence graph K1 <: K2 and see what the SCCs are?
	But what is one supposed to do with such a graph?


Q: What is the equivalent of "cycle elimination" in our setting ?

Q: How far can we get by "statically" computing equalities? of course, this
   would go into "simplification" where each equality set
   is represented by a unique representative?

Q: What is the equivalent of cone of influence?

----------------------------------------------------------------------------------

INFRA-FOLD: CHECKER
-----------------------------------------------------------------------------------
BLOCK INVARIANT
For each al -> cl in concm (must be same set in ORIG, CHECK)
-----------------------------------------------------------------------------------
         CHECK			ORIG
	 A	A1		A	A2	OK if A=A1 <==> A=A2
	 T	T1		T	T2	OK if T1 = T2	
-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------
EDGE INVARIANT
For each al -> cl in concm (must be same set in ORIG, CHECK)
------------------------------------------------------------------------------------
         CHECK			ORIG
	 A1	A2		B1	B2	OK if A1=A2 <==> B1=B2

	 Rd	Rd		T	T	
	 Rd	Wr		_	Wr	
------------------------------------------------------------------------------------
- pretty printing annots using CTypes

Major Expressiveness Limitations:

- Function sigs must have conc locations (to allow delayed initialization).

- Constprop/Mods
	-- Want notion of a function being "pure" wrt abstract location
	-- Do not fold up all abs-locs if sent into "pure" function 
	-- Vanilla interproc (../postests/ll8.c)

- Heap Stratification: allow lower level blocks to "refer to" upper level
                        blocks. Currently, only dependencies are
			intra-block.

Efficiency:

How to find things for which you _dont_ want to generate Kvars 
(i.e. just use "true" refinement)

-------------------------------------------------------------------------

[Annot] Dump out the types for the entire store of a function (not just in/out?)

-------------------------------------------------------------------------

[validptr-fold] make validptr calls NOT fold up stuff...

[Ops]	get postests/hashfunc.c working...
	refactor CilInterface to deal with unop and binop 
        (make CilInterface.expr_of_cilexp return a refinement instead of expr)
	(make CilInterface.pred_of_cilexp handle !)

[mst]
Major Issues
1. Do ALL locations have to be abstract? Makes for weaker invariants.
   conc locations can be tracked strongly but our analysis doesn't give us 
   _any_ conc locations (except locally via unfold)
2. One null-check depends on tracking the "length" of a list. 

Issues
1. localmalloc -> malloc ... (no custom malloc) 
3. remove fun ptr
4. void * Polymorphism (generic hash-table -- "instantiated to int")
5. pesky int-to-ptr cast fails
7. wierd operations like >> and mask
6. ??? (go through JHALA comments to get a full list)

-------------------------------------------------------------------------
[Annot] Readable Annotations: 
Track source of SSA'ed vars to get better type viewing

(1) Generated .vmap file:
    ./main.native ../postests/loop.c
    more ../postests/loop.c.vmap

(2) hack vim (utils/csolve.py) to read above map as one level of
indirection before "tags"

-------------------------------------------------------------------------
[genspec]
Usage: 
1. grep "attribute" to see hint language
2. -typespec uses automatically generated spec
3. default is to use Pat's autogenerated spec

Pending issues (add attribs etc. to allow):
1. postests/stringexample.c (nested structures and ptr-arith)
2. non-aliasing (eg don't alias two int * by default)

-------------------------------------------------------------------------

[tpz32] 
1. how many distinct environments are there ? [grep "Constraint Clusters"]
	- about 261 for pmap

2. [predefvars] instead of lazy vardef, which has bizarre set/filter API
	- TPZ32 -- if its ok/stable. Change solve.ml to point to TPZ32.

3. [envList]	
	- make the environment a list instead of a map
	- [fixCons2] make env a list instead of a map
	- hookup with [solve2] and see if it works

4. [cluster] 	constraints by env in fixpoint
		- design simple (collapsed) tree structure
		- see how many bindings are there in (collapsed) tree structure

5. nuke the ghastly .bnd state threaded through theoremProverZ3.ml
-------------------------------------------------------------------------

- [absLSub]	simplify constraints, to minimize TP calls.
		see tag 11, 14 in postests/incp.c -- both can be "simpl"ified
		in general, absloc subtyping has unnecessary indirection

- [concSpec]	make postests/incp.c work with "Conc" spec, 
		throws an error now -- possibly to do with pat's code

- [inline] 	boolean guards

- [specgen]	use C types to generate a "spec" file for each program,
		that is, create a candidate input type for each function.
		can be refined later.

- [arrays]	add "memset" spec ?
- [globals]	
- [unions]

------------------------------ STALE ------------------------------

[depgraph]
	(a) delete back edges (where src is):
	    	- Return Store		[ok]
		- Generalization-Sub	[tried, makes things worse]

	(b) [did not do] add control dependency edges 
		- intra block,
		- block dominator 
		  (i prec j then add edges from "i" constraints to "j" constraints)
	    both can be done by first clustering constraints by block


[Abs-Conc]

Note: Let [f] be a function in the code. [f] requires either  
  (1) a "set" of locations (use [forall [A0...]] in spec), or
  (2) a "single" location (use [forall [C0...]] in spec).
	//either way, Pat's annotation will mark the target as: 
	//[New(_,Ai)]
	//i.e. the instantiated location will be abstract

  if sig of [f] has
  (1) then body of [f] can un/fold locations from that set as before
  (2) then body of [f] cannot un/fold lcoations from the set. 
  
  To enforce the above, need heap wf to ensure that any given sig 
  can either quantify over an absloc OR over a concloc, but not both.
 
  How to instantiate concrete locations?
  One option, consider a callsite [f(x)]. If
  (2a) [x] is the current owner, 
           do nothing i.e. don't fold
  (2b) [x] is not the current owner, 
           fold up current owner and make [x] the owner.
  This is useless, cannot just use the calling param, as can pass
  concrete locations in as
  	x->f1->f2 etc. 
  hard to determine if thats really conc loc. This fanciness must
  be done at Pat's level. i.e. the "New" annot must have a target 
  concrete location. Right now, it doesn't and its too hard to 
  recreate.

  Instead, use "location instantiation" i.e. [New] annots at callsite.
  That is, site is [[New(ci,li)]... f(...)]. 
  Simply generate fresh conc loc [cli'] for each [li] and "instantiate"
  the "li" with "cli'" (i.e. roll up existing instances etc.)

  Proper solution is to do some matching at Pat's level. This requires
  rethinking his algorithm.

  Now, implement the above and try [postests/incp.c]
  This won't work, because in effect, all locations are treated as abstract
  at a function callsite... Need to do a "deeper" heap-matching in the way
  that pat generates the New annots. i.e. the 

  In short, [refanno] needs to be merged with pat's analysis.


-------------------------------------------------------------------------------
C-Level Specifications
-------------------------------------------------------------------------------

struct node {
  data *contents;
  struct node *left;
  struct node *right;
}

node *head;

void jog(node *tree1, node *tree2);


//Parameterized on all references: 
//each "contents" node points to SAME concrete cell
struct node <A0, C0> {
  data * <C0> contents;
  struct node * <A0> left;
  struct node * <A0> right;
}

node *<A1, C1> head; 

void jog(node *<A2, C2> tree1, node *<A2,C2> tree2);

//Parameterized on all references: 
//each "contents" node points to one in a COLLECTION of cells

struct node <A0, A1> {
  data * <A1> contents;
  struct node * <A0> left;
  struct node * <A0> right;
}

node *<A101, A102> head; 

//indicating sharing
void jog(node *<A201, A202> tree1, node *<A201, 202> tree2);

//indicating non-sharing
void jog(node *<A201, A202> tree1, node *<A203, 204> tree2);

//OR, to get some fresh, distinct param (i.e. non-sharing)
node *<?,?> head;

//indicating non-sharing
void jog(node *<?,?> tree1, node *<?,?> tree2);

//C-Definition
struct data { 
  char *name;
  char *addr;
}
struct node {
  data *contents;
  struct node *left;
  struct node *right;
}

//C-Spec (PARAM)
struct data <A0, A1> { 
  char *<A1> name;
  int  *<A2> age;
}
struct node<A0,A1,A2> {
  data *<A0,A1> contents;
  node *<A0> left;
  node *<A0> right;
} 

//C-Spec (EXIST) ? are just fresh locations distinct from others
struct data { 
  char *<?> name;
  int  *<?> age;
} 
struct node<A0> {
  data     *<?> contents;
  node<A0> *<A0> left;
  node<A0> *<A0> right;
}

//To use a combination?
struct data as data_param<A0, A1> { 
  char *<A0> name;
  int  *<A1> age;
} 
struct node as node_param<A0,A1,A2,A3> {
  data_param<A0,A1>     *<A2> contents;
  node_param<A3>        *<A3> left;
  node_param<A3>        *<A3> right;
}

struct data as data_ex { 
  char *<?> name;
  int  *<?> age;
} as data_ex 
struct node as node_ex<A0> {
  data_ex       *<?> contents;
  node_ex<A0>   *<A0> left;
  node_ex<A0>   *<A0> right;
} 

typedef node<A0> *<A0> node_ex_ptr<A0>;

Now you can define things as 

//(A) with distinct fresh names
void jog(node_ex_ptr<?> tree1, node_ex_ptr<?> tree2);
void jog(node_ex<A0> *<A0> tree1, node<A1> *<A1> tree2);
void jog(node_ex<A0> *<A0> tree1, node<A1> *<A1> tree2);
void jog(node_ex_ptr<A0> tree1, node_ex_ptr<A1> tree2);

//(B) with shared names
void jog(node_ex_ptr<A0> tree1, node_ex_ptr<A0> tree2);
void jog(node_ex<A0> *<A0> tree1, node_ex<A0> *<A0> tree2);

C-Source ---> C-Spec(Auto) ---> Liquid-Spec ---> ShapeInfra etc.
                                ^
                                |
              C-Spec(Hand) -----'

//Parameterized on all references: 
//each "contents" node points to SAME concrete cell
struct node <A0, C0> {
  data * <C0> contents;
  struct node * <A0> left;
  struct node * <A0> right;
}

CoreUtils Notes
---------------





dump vdescr from varinfo (what do they hold? can we get nicer errors than:

tests/cpj/kmeans/normal.c

--------------------------------------------------------------------------
TOTALLY INSCRUTABLE!!!

normal.c:186: Error: Cannot unify ref(A11, {0}) and ref(A242, {0 + 4*})

normal.c:186: Error: Can't fit 
{0 + 4*}: ref(A11, {0})
  in location
A241[instr: #line 186
     work(args__csolve_heapify___normal_exec__SSA__blk_1_1, i___1_normal_exec__SSA__blk_61_1); at normal.c:186] |-> {0 + 4*}: ref(A242, {0 + 4*})

normal.c:186: Error: Can't fit 
{0}: ref(A10, {0 + 4*})
  in location
A240[instr: #line 186
     work(args__csolve_heapify___normal_exec__SSA__blk_1_1, i___1_normal_exec__SSA__blk_61_1); at normal.c:186] |-> {0}: ref(A241, {0 + 4*}),
                                                                                                                    {4}: int(4, {true}),
                                                                                                                    {8}: int(4, {true}),
                                                                                                                    {12}: int(4, {true}),
                                                                                                                    {16}: ref(A243, {0 + 4*}),
                                                                                                                    {20}: ref(A244, {0 + 4*}),
                                                                                                                    {24}: ref(A246, {0 + 4*}),
                                                                                                                    {28}: ref(A248, {0 + 4*})


--------------------------------------------------------------------------

normal.c:154: Error: Expression (int *)__cil_tmp86_normal_exec has type ref(A164, {0 + 1*}), expected type ref(A219, {0})

normal.c:154: Error: Exception (Errormsg.Error) 
Failed constraining instruction:
#line 154
*((int **)mem_41_normal_exec) = (int *)__cil_tmp86_normal_exec;

--------------------------------------------------------------------------

normal.c:154: Error: Failed constrain_fun


tests/cpj/kmeans/normal.c
 
     line 46:   float * ARRAY * ARRAY feature         = args->feature;

have already declared the ARRAY ARRAY in args_t why again?!

--------------------------------------------------------------------------

normal.c:186: Error: Failed constrain_fun

normal.c:186: Error: Location mismatch:
A12[] |-> {0 + 4*}: int(4, {true})
is not included in
A16[] |-> {0}: int(4, {true})




Tedious Error Messages.

chgrp.c:166: Error: Global locations A9[] and A16[] get unified in function body
    
    > Name at least one "owning reference" for each A9, A16

chgrp.c:311: Error: Call unifies non-final locations which are distinct in callee ([A63 -> A14, A59 -> A14]):
    
    > big dump at physical-type level, we want C type level.



chown_files ::
  arg       (files : ref(A58, {0 + 4*} , {VV | [true]}),
             bit_flags : int(4, {true} , {VV | [true]}),
             uid : int(4, {true} , {VV | [true]}),
             gid : int(4, {true} , {VV | [true]}),
             required_uid : int(4, {true} , {VV | [true]}),
             required_gid : int(4, {true} , {VV | [true]}),
             chopt : ref(A60, {0} , {VV | [&& [ ((VV != 0) => ((BLOCK_END([VV]) - VV) >= 24))
                                              ; (VV > 0)
                                              ; (VV = BLOCK_BEGIN([VV]))]]}))
  ret       int(1, {true} , {VV | [true]})
  global    []
  store_in  [A58 |-> {0 + 4*}: ref(A59, {0 + 1*} , {VV | [&& [ (VV < BLOCK_END([VV]))
                                                             ; (BLOCK_BEGIN([VV]) <= VV)
                                                             ; (VV > 0)]]});
             A59 |-> {0 + 1*}: int(1, {true} , {VV | [true]});
             A60 |-> {0}: int(4, {true} , {VV | [true]}),
                     {4}: int(1, {true} , {VV | [true]}),
                     {8}: ref(A61, {0} , {VV | [&& [ ((VV != 0) => ((BLOCK_END([VV]) - VV) >= 12))
                                                   ; (VV > 0)
                                                   ; (VV = BLOCK_BEGIN([VV]))]]}),
                     {12}: int(1, {true} , {VV | [true]}),
                     {13}: int(1, {true} , {VV | [true]}),
                     {16}: ref(A62, {0 + 1*} , {VV | [&& [ (VV < BLOCK_END([VV]))
                                                         ; (BLOCK_BEGIN([VV]) <= VV)
                                                         ; (VV > 0)]]}),
                     {20}: ref(A63, {0 + 1*} , {VV | [&& [ (VV < BLOCK_END([VV]))
                                                         ; (BLOCK_BEGIN([VV]) <= VV)
                                                         ; (VV > 0)]]});
             A61 |-> {0}: int(4, {true} , {VV | [true]}),
                     {4}: int(8, {true} , {VV | [true]});
             A62 |-> {0 + 1*}: int(1, {true} , {VV | [true]});
             A63 |-> {0 + 1*}: int(1, {true} , {VV | [true]})]
  store_out [A58 |-> {0 + 4*}: ref(A59, {0 + 1*} , {VV | [&& [ (VV < BLOCK_END([VV]))
                                                             ; (BLOCK_BEGIN([VV]) <= VV)
                                                             ; (VV > 0)]]});
             A59 |-> {0 + 1*}: int(1, {true} , {VV | [true]});
             A60 |-> {0}: int(4, {true} , {VV | [true]}),
                     {4}: int(1, {true} , {VV | [true]}),
                     {8}: ref(A61, {0} , {VV | [&& [ ((VV != 0) => ((BLOCK_END([VV]) - VV) >= 12))
                                                   ; (VV > 0)
                                                   ; (VV = BLOCK_BEGIN([VV]))]]}),
                     {12}: int(1, {true} , {VV | [true]}),
                     {13}: int(1, {true} , {VV | [true]}),
                     {16}: ref(A62, {0 + 1*} , {VV | [&& [ (VV < BLOCK_END([VV]))
                                                         ; (BLOCK_BEGIN([VV]) <= VV)
                                                         ; (VV > 0)]]}),
                     {20}: ref(A63, {0 + 1*} , {VV | [&& [ (VV < BLOCK_END([VV]))
                                                         ; (BLOCK_BEGIN([VV]) <= VV)
                                                         ; (VV > 0)]]});
             A61 |-> {0}: int(4, {true} , {VV | [true]}),
                     {4}: int(8, {true} , {VV | [true]});
             A62 |-> {0 + 1*}: int(1, {true} , {VV | [true]});
             A63 |-> {0 + 1*}: int(1, {true} , {VV | [true]})]


// last field of chopt
// some cell of files
