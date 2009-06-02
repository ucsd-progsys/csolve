%{
module A  = Ast
module Sy = A.Symbol
module SM = Misc.StringMap
module Ct = Ctypes
module FI = FixInterface

let parse_error msg =
  Errorline.error (symbol_start ()) msg

let store_of_slocbinds sbs = 
  List.fold_left (fun slm (x,y) -> Ct.SLM.add x y slm) Ct.SLM.empty sbs

let ldesc_of_plocbinds pbs = 
  List.fold_left (fun ld (x,y) -> Ct.LDesc.add x y ld) Ct.LDesc.empty pbs

%}

%token <string> Id
%token <int> Num
%token LPAREN  RPAREN LB RB LC RC
%token EQ NE GT GE LT LE
%token AND OR NOT IMPL FORALL SEMI COLON MID
%token TRUE FALSE
%token EOF
%token PLUS
%token MINUS
%token TIMES 
%token DIV 
%token QM DOT ASGN
%token INT BOOL UNINT FUNC
%token SRT AXM CST WF SOL QUL
%token ENV GRD LHS RHS REF

%start specs 

%type <Constraint.deft list>    specs 

%%
specs:
                                        { [] }
  | spec specs                          { $1 :: $2 }
  ;

spec:
  | Id DCOLON 
    FORALL slocs
    ARG    binds 
    RET    reftype
    INS    refstore
    OUTS   refstore                     { ($1, (mk_cfun $4 $6 $8 $10 $12)) }
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
  Num                                   { $1 }
  ;

refstore:
    LB RB                               { Ct.SLM.empty }
  | LB slocbindsne RB                   { store_of_slocbinds $2 }
  ;

slocbindsne:
    slocbind                            { [$1] } 
  | slocbind SEMI slocbindsne           { $1 :: $3 }
  ;

slocbind:
  sloc MAPSTO plocbinds                 { ($1, ldesc_of_plocbinds $3) } 
  ;

plocbinds:
                                        { [] }
  | plocbindsne                         { $1 }
  ;

plocbindsne:
    plocbind                            { [$1] }
  | plocbind COMMA plocbindsne          { $1 :: $3 }
  ;

plocbind:
    ploc COLON reftype                  { ($1, $3) }
  ;

reftype: 
  LC Id COLON ctype MID pred RC         { FI.t_pred (Sy.of_string $2) $4 $6 }
  ;

ctype:
    INT LPAREN Num COMMA index RPAREN   { CTInt ($3, $5) }
  | REF LPAREN sloc COMMA index RPAREN  { CTRef ($3, $5) }
  ;

ploc:

index:
    Num                                 { Ct.IInt $1 }
  | TOP                                 { Ct.ITop }
  ;

binds:
    LB RB                               { [] }
  | LB bindsne RB                       { $2 }
  ;

bindsne:
    bind                                { [$1] }
  | bind SEMI bindsne                   { $1::$3 }
  ;

bind:
  Id COLON reftype                      { ((Sy.of_string $1), $3) }
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
  | FORALL binds DOT pred               { A.pForall ($2, $4) }
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
  | Id LPAREN exprs RPAREN		{ A.eApp ((Sy.of_string $1), $3) }
  | pred QM expr COLON expr             { A.eIte ($1,$3,$5) }
  | expr DOT Id                         { A.eFld ((Sy.of_string $3), $1) }
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
