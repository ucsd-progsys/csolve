%{
module A   = Ast
module So  = Ast.Sort
module Sy  = A.Symbol
module SM  = Misc.StringMap
module FI  = FixInterface
module Ct  = Ctypes
module RCt = Ct.RefCTypes
module N   = Index
module E   = Errormsg

open Misc.Ops

let parse_error msg =
  Errorline.error (symbol_start ()) msg

let assert_location_unbound l sto loc =
  if RCt.Store.mem sto l then
    E.s <| Cil.errorLoc loc "Duplicate binding for store location"

type slocbind_contents =
  | SData   of RCt.LDesc.t
  | SFun    of RCt.CFun.t

let store_of_slocbinds sbs =
  List.fold_left begin fun sto (x, y, loc) ->
    let _ = assert_location_unbound x sto loc in
      match y with
        | SData ld  -> RCt.Store.Data.add sto x ld
        | SFun f    -> RCt.Store.Function.add sto x f
  end RCt.Store.empty sbs

let ldesc_of_plocbinds pbs =
  List.fold_left (fun ld (x,y) -> RCt.LDesc.add x y ld) RCt.LDesc.empty pbs

let abs_sloctable = Hashtbl.create 17
let cnc_sloctable = Hashtbl.create 17

let mk_sloc_abstract id =
  Misc.do_memo abs_sloctable Sloc.fresh_abstract ([CilMisc.srcinfo_of_string (string_of_int id)]) id

let mk_sloc_concrete id absid =
  Misc.do_memo cnc_sloctable Sloc.fresh_concrete (mk_sloc_abstract absid) id

let mk_funspec fn cf public =
  (fn, (cf, public))

exception InvalidStoredSpecType

let add_funspec spec (fn, (rcf, public)) =
  let storespec = RCt.Spec.store spec in
  if RCt.Store.closed RCt.Store.empty storespec then
    if RCt.CFun.well_formed storespec rcf then
      RCt.Spec.add_fun true fn (rcf, public) spec
    else begin
      Format.printf "Error: %s has ill-formed spec\n\n" fn |> ignore;
      raise Parse_error
    end
  else begin
    Format.printf "Error: global store not closed\n\n" |> ignore;
    raise Parse_error
  end

let add_varspec spec (var, (ty, public)) =
  let storespec = RCt.Spec.store spec in
  if RCt.Store.ctype_closed ty storespec then
    RCt.Spec.add_var true var (ty, public) spec
  else begin
    Format.printf "Error: %s has ill-formed spec\n\n" var |> ignore;
    raise Parse_error
  end

let depreference_regex = Str.regexp ("^A\\([0-9]+\\)#" ^ N.repr_prefix ^ "\\([0-9]+\\)$")

let rename_depreference s =
  if not (Str.string_match depreference_regex s 0) then s else
    let slocnum = s |> Str.matched_group 1 |> int_of_string in
    let sloc    = slocnum |> Hashtbl.find abs_sloctable |> Sloc.to_string in
    let idx     = s |> Str.matched_group 2 in
      sloc ^ "#" ^ N.repr_prefix ^ idx

let currentLoc () =
  let p = symbol_start_pos () in
    {Cil.file = p.Lexing.pos_fname;
     Cil.line = p.Lexing.pos_lnum;
     Cil.byte = p.Lexing.pos_cnum}

%}

%token DIV 
%token <string> Id
%token <int> Num
%token <int> ABS 
%token <int> CONC
%token LPAREN  RPAREN LB RB LC RC
%token EQ NE GT GE LT LE
%token AND OR NOT IMPL FORALL COMMA SEMI COLON PCOLON DCOLON HASTYPE MAPSTO MID LOCATION
%token ARG RET ST GLOBAL INST OUTST
%token TRUE FALSE
%token EOF
%token MOD 
%token PLUS
%token MINUS
%token TIMES 
%token QM DOT ASGN
%token INT BOOL PTR FUNC
%token SRT AXM CST WF SOL QUL
%token ENV GRD LHS RHS REF TOP
%token FINAL

%start specs 
%start pred

%type <Ctypes.refspec>    specs
%type <Ast.pred>          pred

%%
specs:
                                        { Hashtbl.clear abs_sloctable;
                                          Hashtbl.clear cnc_sloctable;
                                          RCt.Spec.empty }
  | specs funspec                       { add_funspec $1 $2 }
  | specs varspec                       { add_varspec $1 $2 }
  | specs locspec                       { let l, st, sp, loc = $2 in
                                          let _             = assert_location_unbound l (RCt.Spec.store $1) loc in
                                            match sp with
                                              | SData sp -> RCt.Spec.add_data_loc l (sp, st) $1
                                              | SFun sp  -> RCt.Spec.add_fun_loc l (sp, st) $1
                                        }
  ;

funspec:
    Id publ funtyp { mk_funspec $1 $3 $2 }
    ;

funtyp:
    ARG    argbinds
    RET    reftype
    GLOBAL slocs
    INST   refstore
    OUTST  refstore {
      Ct.RefCTypes.CFun.make $2 $6 $8 $4 $10
    }
  | ARG    argbinds
    RET    reftype
    INST   refstore
    OUTST  refstore {
      Ct.RefCTypes.CFun.make $2 [] $6 $4 $8
    }
  | ARG    argbinds
    RET    reftype
    GLOBAL slocs
    ST     refstore
    {
      Ct.RefCTypes.CFun.make $2 $6 $8 $4 $8
    }
  | ARG    argbinds
    RET    reftype
    ST     refstore
    {
      Ct.RefCTypes.CFun.make $2 [] $6 $4 $6
    }
    ;

varspec:
  Id publ reftype
  {
    ($1, ($3, $2))
  }
  ;

locspec:
  LOCATION globalslocbind               { $2 }
  ;  

publ:
    DCOLON                              { Ct.HasShape }
  | PCOLON                              { Ct.IsSubtype }
  | HASTYPE                             { Ct.HasType }
  ;

globalslocbind:
    sloc publ indbinds {
      ($1, $2, SData (RCt.LDesc.create Ct.dummy_structinfo $3), currentLoc ())
    }
  | sloc publ funtyp {
      ($1, $2, SFun $3, currentLoc ())
    }
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
    ABS                                 { mk_sloc_abstract $1 }
  | CONC LB ABS RB                      { mk_sloc_concrete $1 $3 }
  ;

refstore:
    LB RB                               { RCt.Store.empty }
  | LB slocbindsne RB                   { store_of_slocbinds $2 }
  ;

slocbindsne:
    slocbind                            { [$1] } 
  | slocbind SEMI slocbindsne           { $1 :: $3 }
  ;

slocbind:
    sloc MAPSTO indbinds                { ($1, SData (RCt.LDesc.create Ct.dummy_structinfo $3), currentLoc ()) }
  | sloc MAPSTO funtyp                  { ($1, SFun $3, currentLoc ()) }
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
    index COLON reftype {
      ($1, Ct.RefCTypes.Field.create Ct.Nonfinal Ct.dummy_fieldinfo $3)
    }
  | index COLON FINAL reftype {
      ($1, Ct.RefCTypes.Field.create Ct.Final Ct.dummy_fieldinfo $4)
    }
  ;

reftype:
    INT LPAREN Num COMMA index COMMA LC Id MID pred RC RPAREN 
                                        { let ct = Ctypes.Int ($3, $5) in
                                          let v  = Sy.of_string $8 in 
                                          FI.t_pred ct v $10 
                                        }
  
  | REF LPAREN sloc COMMA index COMMA LC Id MID pred RC RPAREN
                                        { let ct = Ctypes.Ref ($3, $5) in
                                          let v  = Sy.of_string $8 in 
                                          FI.t_pred ct v $10 
                                        }

  | ctype                               { FI.t_true $1 }
  ;

ctype:
    INT LPAREN Num COMMA index RPAREN   { Ctypes.Int ($3, $5) }
  | REF LPAREN sloc COMMA index RPAREN  { Ctypes.Ref ($3, $5) }
  ;

index:
    Num                                 { N.IInt $1 }
  | Num LB Num RB                       { N.mk_sequence $1 $3 (Some $1) None }
  | Num LB Num LT Num RB                { N.mk_sequence $1 $3 (Some $1) (Some ($5 - $3)) }
  | Num LC Num RC                       { N.mk_sequence $1 $3 None None }
  | TRUE                                { N.top }
  | FALSE                               { N.IBot }
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
    Id				        { A.eVar ($1 |> rename_depreference |> Sy.of_string) }
  | Num 				{ A.eCon (A.Constant.Int $1) }
  | LPAREN expr MOD Num RPAREN          { A.eMod ($2, $4) }
  | expr bop expr                       { A.eBin ($1, $2, $3) }
  | Id LPAREN exprs RPAREN              { A.eApp ((Sy.of_string $1), $3) }
  | pred QM expr COLON expr             { A.eIte ($1,$3,$5) }
  | expr COLON sort                     { A.eCst ($1,$3) }
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


