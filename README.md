README
======

**NOTE:** csolve can only be compiled on Linux at the moment.

Prerequisites
=============

`csolve` requires a recent OCaml compiler, the GNU multiprecision library,
and the CamlIDL library.


Checking Out Of Git
===================

To clone csolve from git:

git clone goto.ucsd.edu:/git/csolve
cd csolve

Configuring and Compiling
=========================

To begin building csolve, run the following commands in the root
directory of the source distribution:

    1. ./configure 
    2. ./build.sh

On MACOS, use `configure.mac` instead.

To build csolve after this step, 

    3. cd src/ && make 

Testing csolve
==============

To ensure that everything compiled successfully and csolve is properly
configured, change into the src/ directory and run

    ./regrtest.py

All tests should pass, though some warnings may be displayed.
The regression tests may take some time to run. If you have 
multiple cores, run

    ./regrtest.py -t N 
    
to run the tests in N separate threads.

Running csolve
==============

The csolve executable, `csolve`, is used *just like* `gcc`.

There are two common use cases:

Running on a single file 
------------------------

To run on a single file `code.c`, simply run

    csolve code.c

To typecheck and compile `code.c`. csolve will read qualifiers from
  
    code.c.hquals.

`csolve` will return one of the following:

1. Output **SAFE** and compile the file
   
   The program is memory- and assertion-safe: all pointer
   dereferences (including array accesses) are within the bounds of
   an allocated region and all assertions pass.  Additionally, all
   function calls satisfy the specified preconditions.

2. Output **UNSAFE** and halt

   The program may contain memory safety, assertion, or function
   precondition violations.  The locations of the errors are
   printed, as well as the constraints that fail.

`csolve` takes the same arguments as `gcc`.  In particular, if you do not
  want to fully compile code.c - for example, if code.c would not link
  because it contains references to "dummy" functions - then add the
  -c flag to stop before linking:

    csolve -c code.c

  Additionally, csolve-specific options can be found by

    csolve --help

Compiling an entire source tree
-------------------------------


To use `csolve` to compile an entire project -- first compiling C source
to object files, then compiling those object files into an executable -- 
invoke `csolve` using the --merge flag:

    csolve --merge -c foo.c
    csolve --merge -c boo.c
    csolve --merge -o fooboo foo.o boo.o

`csolve` will read qualifiers from fooboo.hquals.

Important Options
-----------------

* Verbosity can be set using the "--v=n" flag, where n is a numeric
  verbosity level.  See the output of --help.


Writing Specifications
======================

Specifying Function and Value Types 
-----------------------------------

csolve uses specifications for functions and values to determine

1. the layout of both the global heap and the heap within each
   function contained in the program,
2. what invariants the programmer wishes to enforce about functions
   and values contained in the program, and
3. what invariants are true of external functions and variables,
   i.e., those declared with the "extern" qualifier, as well as
   the heap layout of external functions and the global heap
   segment reachable from an external global variable.

In the absence of external function and variable declarations, csolve
attempts to guess reasonable specifications for the layout of the
global and function-specific heaps and infers function invariants,
covering cases 1 and 2 above. In the event that the heap layout
guesses are unsatisfactory, if additional invariants need to be
stated, or there are external definitions, the programmer must provide
csolve with annotations that override its guesses or state explicit
invariants.

To use the explicit specification mechanism, include `csolve.h` in every
file that contains specifications, like so:

    #include <csolve.h>


Specifying Heap Layout
----------------------

In this section, we discuss annotations which affect *only* how csolve
determines the layout of a (global or function-specific) heap; some
annotations in the next section on specifying invariants also affect
how csolve determines the layout of the heap.

When determining the layout of a function-specific heap, csolve
assumes that all arguments and the return value, if they are
pointers, occupying disjoint sections of the heap. For example,
given the function prototype

    int *swap (int *x, int *y)

csolve assumes that x, y, and the return value all occupy distinct
heap locations, i.e., they can never be aliased. To indicate that x
and y may alias, use the "LOC" annotation with a location name:

    int *swap (int * LOC(L) x, int * LOC(L) y)

By default, csolve assumes that the location L is local to swap's
heap. To indicate that L is a global location (i.e., may alias global
pointers), use the GLOBAL annotation on the function's signature:

    int *swap (int * LOC(L) x, int * LOC(L) y) GLOBAL(L)

Similarly, in structure declarations, csolve assumes that each pointer
within a structure points to a disjoint location in memory. For
example, in

    typedef struct {
      int *x;
      int *y;
    } pair;

the integers x and y are presumed to reside in disjoint regions of
memory and may never alias. As is the case with function parameters,
we can explicitly name the locations x and y point to to indicate
potential aliasing:

    struct pair {
      int * LOC(L) x;
      int * LOC(L) y;
    };

The location L is now a parameter of the struct definition. In
the function prototype

    int setx (struct pair *p, int *x)

the location L in the defintion of pair will be instantiated to a
fresh location, i.e., one distinct from all other locations.  We can
instantiate L with a specific location to indicate potential aliasing,
for example, between x and p->x and p->y:

    int setx (struct pair INST(L, K) *p, int * LOC(K) x)

Above, the locations of p->x and p->y, which are L inside the
structure defintion, are instantiated to location K, which is the
location given to the contents of parameter x.

Similarly, types specified with typedefs are parameterized on their
locations, so that in the definition

    typedef int * LOC(L) intptr;

the location L is a paraemeter of the type intptr.

By default, csolve assumes that a pointer to a location points to a
single element of the pointer's base type. For example, the function

    int *swap (int * LOC(L) x, int * LOC(L) y) GLOBAL(L)

is assumed to take pointers x and y to single integers. To indicate
that a pointer points to an array of elements, use the ARRAY annotation:

    int quicksort (int * ARRAY ns)

The exception to this rule is the type char *, which is assumed by
default to point to an array of characters:

    int strlen (char *str) // str is assumed to point to an array

If you actually mean that a character pointer should point to a single
character, use the SINGLE annotation:

    void setchar (char * SINGLE c, char d)

Almost always, every use of a structure or typedef'd type instantiates
the locations contained within the type. The sole exception is when a
structure type is used in a typedef, as in

    typedef struct foo foo;

In this case, whatever locations are quantified in struct foo will
also be quantified in foo. This is syntactic sugar which keeps you
from having to "re-export" all the locations L in struct foo using
INST(L, L) in the typedef, i.e.,

    typedef struct foo INST(L, L) foo;

csolve attempts to take explicitly-provided array bounds into account
when determining the layout of a heap. In some cases this may be
undesirable; for example, in the structure

    typedef struct {
      int  len;
      char str[0];
    } string;

we do not want csolve to assume that string really has a zero-length
array at its end. In this case, we use the SHAPE_IGNORE_BOUND
annotation:

    typedef struct {
      int  len;
      char (SHAPE_IGNORE_BOUND str)[0];
    } string;

In some cases --- for example, with external function declarations ---
we may need to indicate that a function does not modify the contents
of a heap location. We do this with a FINAL annotation on the pointer
base type or field in question. For example, the function

    void readPointer (int FINAL *p)

is annotated to indicate that it does not modify the data pointed to
by P.

In some cases, the declared base type of a pointer does not match
the actual contents of memory at that location. For example, suppose
we have the function

    typedef struct _intlist intlist;

    struct _intlist {
      int     i;
      intlist *next;
    };

    int *getNextInt (int *i) {
      return (int *) ((intlist *) i)->next;
    }

While the declared type of i is int *, i's location actually contains
an intlist structure. To indicate this fact to csolve, use the LAYOUT
annotation with the actual type of the store's contents at that
location:

    int *getNextInt (int * LAYOUT(intlist) i) {
      return (int *) ((intlist *) i)->next;
    }

Specifying Invariants
---------------------

The annotation REF(p) is used to refine a type to values that satisfy
both the base type and predicate p. For example, the declaration of a
positive integer is written

  int REF(V > 0) i;

By default, refinement predicates in locally-defined function and
global variable types are ignored; the types of these entities are
inferred and no checking against the user-provided predicates is
performed. To override this, attach the CHECK_TYPE attribute to
the declaration.

For example, consider the declaration

  int REF(&& [V >= x; v >= 0]) abs (int x) CHECK_TYPE {
    return x < 0 ? -x : x;
  }

csolve will verify that this function takes an integer x and returns a
nonnegative integer that is at least as large as x.

csolve will conjoin all refinements attached to a type, so that an
alternate prototype for the above function is

  int REF(V >= x) REF(v >= 0) abs (int x)

The file lib/csolve.h contains a number of convenient macros for
defining refinement predicates as well as shorthands for refinements, e.g.,

  #define NONNEG REF(V >= 0)

The annotation ROOM_FOR(t) indicates that a pointer points to the
start of a region which has enough space to fit an object of type t:

  void fclose (FILE * ... ROOM_FOR(FILE) f)

The annotation NNROOM_FOR(t) is the same as ROOM_FOR(t), but allows
the pointer to optionally be NULL.

Pointer types t * without annotations will get the default annotation

  NONNULL NNROOM_FOR(t) REF(V = BLOCK_BEGIN([V]))

Specifying Types for External Declarations
------------------------------------------

Note that the function declaration

  extern int divide (int x, int y);

is a valid specification to csolve; it simply says that there exists a
function, divide, which takes two integer parameters and returns an
integer. However, csolve cannot see the definition of divide and so
cannot ensure that this liberal specification is enough to ensure the
safety of all uses of divide and, indeed, if divide is defined in the
obvious way then this specification is too liberal.

This illustrates a common situation with library functions: they are
declared extern, but the trivial preconditions guessed by csolve do
not ensure that they are used safely, and yet their absence from the
source code means that csolve is unable to detect this inconsistency.
Thus, we must somehow indicate that an extern function declaration
corresponds to a valid specification for that function, i.e., that the
programmer has somehow verified that type-conforming uses of the
function are safe.  This is accomplished using the OKEXTERN attribute.

Thus, a proper declaration for the divide function would be

  extern int divide (int x, int REF(V != 0) y) OKEXTERN;

The above discussion applies equally to extern variable declarations.

More Examples
-------------

Useful example annotations can be found in 

- `lib/csolve.h` 
- `external/include32/stdlib.h`

Writing Logical Qualifiers 
==========================

To typecheck programs, csolve uses logical qualifier files that
contain "hints" about how to typecheck the program, called logical
qualifiers, or qualifiers for short.  Each qualifier is a predicate
expressing some property of program values.  To make these qualifiers
easier to express, they may contain wildcards that range over the
names of variables in the program.  

Examples
--------

We begin with examples before defining the syntax of qualifiers.  
Further examples can be found in `src/lib.hquals`.

    qualif EQZ(v: int): v = 0

A qualifier named `EQZ` which expresses that "this" value, denoted
by `v` is an integer which equal to `0`.

    qualif GT(v: int): v > x

A qualifier named `GT` which expresses that "this" value `v` is an
integer which is greater than the value of the variable named `x`.

    qualif GT(v: int): v > @prefix

A qualifier named `GT` which expresses that "this" value `v` is an
integer which is greater than the value of some variable whose name
begins with `prefix`.

    qualif GT(v: int): v > ~a

A qualifier named `GT` which expresses that "this" value `v` is an
integer which is greater than the value of some program variable.

    qualif NONNULL(v: ptr): v != 0

A qualifier named `NONNULL` which expresses that "this" value (v) is a
pointer which is not `NULL` (i.e., equal to `0`).

    qualif UB(v:ptr): v < BLOCK_END([v])

A qualifier named `UB` which expresses that "this" value `v` is a
pointer which is less than the pointer that points to the end of the
block that v points to `BLOCK_END([v])`.

Syntax
------

Qualifiers are of the form

    qualif NAME(v: TYPE): PREDICATE

NAME can be any alphanumeric string; it has no semantic
interpretation.  TYPE is either "ptr" or "int", and determines whether
this predicate applies to values v which are pointers or integers,
respectively.  PREDICATE is a logical predicate over the value
variable v, the program variables, and wildcard variables, containing
the following constructs:

* Integer constants (0, -2, etc.)

* Program variables (x, y, i, etc.)

* Metavariables ranging over program variables (~a, ~b, etc.)

* Anonymous wildcards over program variables (_)

* Metavariables ranging over program variables with a prefix
  @prefix instantiates to prefix1, prefix2, etc.

* Binary comparisons x # y where # in {<, <=, =, !=, =>, >}

* Arithmetic expressions x # y where # in {+, -, *}
  Arithmetic expressions must be linear.

* Conjunctions && [p; q] where p and q are predicates
  Generally the brackets may contain a semicolon-separated list
  of predicates.

* Disjunctions || [p; q] where p and q are predicates
  Generally the brackets may contain a semicolon-separated list
  of predicates.

* Implications p => q where p and q are predicates

* Parenthesized expressions




Incremental Checking 
====================

To make interactive use pleasant, csolve has an incremental checking
option, in which a subset of target functions is analyzed using the 
previously inferred types for the rest of the program. (The mode ALSO
checks that the target functions behave as inferred previously -- thus 
ensuring that the target functions are analyzed under appropriate 
preconditions.)

1. Run the whole program check as usual, e.g.

        csolve file.c 

2. Now you can tweak individual functions say, and just have them be
   re-checked against the specifications ("summaries" for all
   the other functions) inferred in Step 1:

        csolve --inccheck=FUN1 ... --inccheck=FUNk file.c

   Will only re-analyze FUN1,...,FUNk (the C functions you wish to re-check.)

For example,

        csolve pmapdistilled.c

takes about 50s to finish the analysis, but

        csolve --inccheck=page_getfree pmap_distilled.c

returns in a couple of seconds.


VIM csolve Mode
===============

There is a *rudimentary* vim mode for viewing the output of a csolved-file.

1. Add the following to your .vimrc

    pyfile /path/to/csolve/utils/csolve.py
    map <C-s>  :python printLiquidType("normal")<CR>
    vmap <C-s> :python printLiquidType("visual")<CR>
    map <C-p>  :python parseLiquidType()<CR>

2. You must run csolve in the directory where the file foo.c is.
   
    path/to/csolve foo.c

3. Now, in that directory, open foo.c (in Vim)

3. Hit Ctrl-p to parse the annotations file

4. Hit Ctrl-s when your mouse is above identifier `foo`
   to see the refinements for `foo`. 
   
5. Hit Ctrl-] (tag-style) to jump into a definition 
   (once you are in the types-window)

6. Hit Ctrl-t (tag-style) to pop back from a definition.


