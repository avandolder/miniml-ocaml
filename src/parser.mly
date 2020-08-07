%token <int> INT
%token <string> ID
%token LET IN
%token CASE OF
%token IF THEN ELSE
%token FN
%token WILDCARD "_"
%token TYBOOL TRUE FALSE
%token TYINT
%token LEFT_PARAN "(" RIGHT_PARAN ")"
%token COLON ":"
%token COMMA ","
%token BAR "|"
%token EQUAL "="
%token FAT_ARROW "=>"
%token THIN_ARROW "->"
%token EOF

%right THIN_ARROW

%{
  open Syntax
%}

%start <term option> prog
%%

prog:
  | t=term? EOF { t }

term:
  | LET p=pattern ":" ty=ty "=" t1=term IN t2=term { TLet (p, ty, t1, t2) }
  | CASE t=term OF "|"? cs=cases                   { TCase (t, cs) }
  | IF t1=term THEN t2=term ELSE t3=term           { TIf (t1, t2, t3) }
  | FN p=pattern ":" ty=ty "=>" t=term             { TFn (p, ty, t) }
  | t=tuple(term)                                  { TTuple t }
  | t=appterm                                      { t }

appterm:
  | t1=term t2=atomterm { TApply (t1, t2) }
  | t=atomterm          { t }

atomterm:
  | id=ID          { TId id }
  | TRUE           { TBool true }
  | FALSE          { TBool false }
  | i=INT          { TInt i }
  | "(" t=term ")" { t }

cases:
  cs=separated_list("|", case) { cs }

case:
  p=pattern "=>" t=term { (p, t) }

pattern:
  | "_"              { PWildcard }
  | id=ID            { PId id }
  | TRUE             { PBool true }
  | FALSE            { PBool false }
  | i=INT            { PInt i }
  | t=tuple(pattern) { PTuple t }

ty:
  | TYBOOL           { TyBool }
  | TYINT            { TyInt }
  | t=tuple(ty)      { TyTuple t }
  | t1=ty "->" t2=ty { TyFn (t1, t2) }
  | "(" t=ty ")"     { t }

tuple(T):
  | "(" ")"                                        { [] }
  | "(" t=T "," ts=separated_list(",", T) ","? ")" { t::ts }
