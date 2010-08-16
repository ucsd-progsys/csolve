%{
module A  = Ast
module So = Ast.Sort
module Sy = A.Symbol
module SM = Misc.StringMap
module FI = FixInterface

open Misc.Ops

let parse_error msg =
  Errorline.error (symbol_start ()) msg

let store_of_slocbinds sbs = 
  List.fold_left (fun slm (x,y) -> Sloc.SlocMap.add x y slm) Sloc.SlocMap.empty sbs

let ldesc_of_plocbinds pbs = 
  List.fold_left (fun ld (x,y) -> Ctypes.LDesc.add x y ld) Ctypes.LDesc.empty pbs

let sloctable = Hashtbl.create 17

let mk_sloc id sty =
  Misc.do_memo sloctable Sloc.fresh sty (id, sty)

let mk_funspec fn public qslocs args ist ret ost =
  (fn, (FI.mk_refcfun qslocs args ist ret ost, public))

let add_funspec ((_, _, storespec) as spec) (fn, (rcf, public)) =
  if Ctypes.prestore_closed storespec then
    if Ctypes.precfun_well_formed storespec rcf then
      Ctypes.PreSpec.add_fun fn (rcf, public) spec
    else begin
      Format.printf "Error: %s has ill-formed spec\n\n" fn |> ignore;
      raise Parse_error
    end
  else begin
    Format.printf "Error: global store not closed\n\n" |> ignore;
    raise Parse_error
  end

let add_varspec ((_, _, storespec) as spec) (var, (ty, public)) =
  if Ctypes.prectype_closed ty storespec then
    Ctypes.PreSpec.add_var var (ty, public) spec
  else begin
    Format.printf "Error: %s has ill-formed spec\n\n" var |> ignore;
    raise Parse_error
  end

%}

%token DIV 
%token <string> Id
%token <int> Num
%token <int> ABS 
%token <int> CONC
%token LPAREN  RPAREN LB RB LC RC
%token EQ NE GT GE LT LE
%token AND OR NOT IMPL FORALL COMMA SEMI COLON PCOLON DCOLON MAPSTO MID LOCATION
%token ARG RET ST INST OUTST
%token TRUE FALSE
%token EOF
%token PLUS
%token MINUS
%token TIMES 
%token QM DOT ASGN
%token INT BOOL PTR FUNC
%token SRT AXM CST WF SOL QUL
%token ENV GRD LHS RHS REF

%start specs 

%type <FixInterface.refspec>    specs

%%
specs:
                                        { Hashtbl.clear sloctable; Ctypes.PreSpec.empty }
  | specs funspec                       { add_funspec $1 $2 }
  | specs varspec                       { add_varspec $1 $2 }
  | specs locspec                       { let lc, sp = $2 in Ctypes.PreSpec.add_loc lc sp $1 }
  ;

funspec:
    Id publ 
    FORALL slocs
    ARG    argbinds 
    RET    reftype
    INST   refstore
    OUTST  refstore {
      mk_funspec $1 $2 $4 $6 $10 $8 $12
    }
  | Id publ
    FORALL slocs
    ARG    argbinds
    RET    reftype
    ST     refstore
    {
      mk_funspec $1 $2 $4 $6 $10 $8 $10
    }
    ;

varspec:
  Id publ reftype
  {
    ($1, ($3, $2))
  }
  ;

locspec:
  LOCATION slocbind                     { $2 }
  ;

publ:
  | DCOLON                              {false}
  | PCOLON                              {true}
  ;


slocs:
    LB RB                               { [] }
  | LB slocsne RB                       { $2 }
  ;

slocsne:
    sloc                                { $1 :: [] }
  | sloc SEMI slocsne                   { $1 :: $3 }
  ;

sloc:
    ABS                                 { mk_sloc $1 Sloc.Abstract }
  | CONC                                { mk_sloc $1 Sloc.Concrete }
  ;

refstore:
  LB RB                                 { Sloc.SlocMap.empty }
  | LB slocbindsne RB                   { store_of_slocbinds $2 }
  ;

slocbindsne:
    slocbind                            { [$1] } 
  | slocbind SEMI slocbindsne           { $1 :: $3 }
  ;

slocbind:
  sloc MAPSTO indbinds                  { ($1, Ctypes.LDesc.create $3) } 
  ;

indbinds:
                                        { [] }
  | indbindsne                          { $1 }
  ;

indbindsne:
    indbind                             { [$1] }
  | indbind COMMA indbindsne             { $1 :: $3 }
  ;

indbind:
    index COLON reftype                 { ($1, $3) }
  ;

reftype:
    INT LPAREN Num COMMA index COMMA LC Id MID pred RC RPAREN 
                                        { let ct = Ctypes.CTInt ($3, $5) in
                                          let v  = Sy.of_string $8 in 
                                          FI.t_pred ct v $10 
                                        }
  
  | REF LPAREN sloc COMMA index COMMA LC Id MID pred RC RPAREN
                                        { let ct = Ctypes.CTRef ($3, $5) in
                                          let v  = Sy.of_string $8 in 
                                          FI.t_pred ct v $10 
                                        }

  | ctype                               { FI.t_true $1 }
  ;

ctype:
    INT LPAREN Num COMMA index RPAREN   { Ctypes.CTInt ($3, $5) }
  | REF LPAREN sloc COMMA index RPAREN  { Ctypes.CTRef ($3, $5) }
  ;

index:
    Num                                 { Ctypes.Index.IInt $1 }
  | Num LB Num RB                       { Ctypes.Index.ISeq ($1, $3, Ctypes.Pos) }
  | Num LC Num RC                       { Ctypes.Index.ISeq ($1, $3, Ctypes.PosNeg) }
  | TRUE                                { Ctypes.Index.top }
  | FALSE                               { Ctypes.Index.IBot }
  ;

argbinds:
    LPAREN RPAREN                       { [] }
  | LPAREN argbindsne RPAREN            { $2 }
  ;

argbindsne:
    argbind                             { [$1] }
  | argbind COMMA argbindsne            { $1::$3 }
  ;

argbind:
  Id COLON reftype                      { ($1, $3) }
  ;


preds:
    LB RB                               { [] }
  | LB predsne RB                       { $2 }
  ;

predsne:
    pred                                { [$1] }
  | pred SEMI predsne                   { $1 :: $3 }
;

pred:
    TRUE				{ A.pTrue }
  | FALSE				{ A.pFalse }
  | AND preds   			{ A.pAnd ($2) }
  | OR  preds 	        		{ A.pOr  ($2) }
  | NOT pred				{ A.pNot ($2) }
  | pred IMPL pred			{ A.pImp ($1, $3) }
  | expr brel expr                      { A.pAtom ($1, $2, $3) }
  | LPAREN pred RPAREN			{ $2 }
  ;

exprs:
    LB RB                               { [] }
  | LB exprsne RB                       { $2 }
  ;

exprsne:
    expr                                { [$1] }
  | expr SEMI exprsne                   { $1 :: $3 }
  ;

expr:
    Id				        { A.eVar (Sy.of_string $1) }
  | Num 				{ A.eCon (A.Constant.Int $1) }
  | expr bop expr                       { A.eBin ($1, $2, $3) }
  | Id LPAREN exprs RPAREN              { A.eApp ((Sy.of_string $1), $3) }
  | pred QM expr COLON expr             { A.eIte ($1,$3,$5) }
  | LPAREN expr RPAREN                  { $2 }
  ;

brel:
    EQ                                  { A.Eq }
  | NE                                  { A.Ne }
  | GT                                  { A.Gt }
  | GE                                  { A.Ge }
  | LT                                  { A.Lt }
  | LE                                  { A.Le }
  ;

bop:
    PLUS                                { A.Plus }
  | MINUS                               { A.Minus }
  | TIMES                               { A.Times }
  | DIV                                 { A.Div }
  ;

sorts:
    LB RB                               { [] }
  | LB sortsne RB                       { $2 }
  ;

sortsne:
    sort                                { [$1] }
  | sort SEMI sortsne                   { $1 :: $3 }
  ;

sort:
  | INT                                 { So.t_int }
  | PTR                                 { So.t_ptr (So.Lvar 0) }
  | PTR LPAREN Num RPAREN               { So.t_ptr (So.Lvar $3) }
  ;


