%{
let make_app = function
  | [] -> assert false
  | [t] -> t
  | t::ts ->
      List.fold_left (fun x y -> Term.App (x, y)) t ts

let make_fun args body =
  List.fold_right (fun arg expr -> Term.Lam (arg, expr)) args body
%}

%token EOF
%token IF THEN ELSE
%token FUN ARROW
%token LET REC IN
%token EQL
%token SEMI
%token DOT
%token LBRACE RBRACE
%token LPAREN RPAREN
%token <string> ID
%token <int> INT

%start program

%type<(bool * string * Term.t) list> program
%type<bool * string * Term.t> declaration
%type<Term.t> expr prim_expr record
%type<string * Term.t> field

%%

program:
  declaration* EOF
    { $1 }

declaration:
    LET; is_rec = boption(REC); name = ID; params = ID*; EQL; body = expr; SEMI?
    { (is_rec, name, make_fun params body) }

expr:
  | FUN; params = ID+; ARROW; body = expr
    { make_fun params body }
  | LET; is_rec = boption(REC); name = ID; params = ID*; EQL; rhs = expr; IN; body = expr
    { Term.LetIn (is_rec, name, make_fun params rhs, body) }
  | IF; c = expr; THEN; t = expr; ELSE; e = expr
    { Term.O.(v"if" $ c $ t $ e) }
  | app = prim_expr+
    { make_app app }

prim_expr:
    r = record
    { r }
  | record = prim_expr; DOT; field = ID
    { Term.Selection (record, field) }
  | v = ID
    { Term.Var v }
  | i = INT
    { Term.Int i }
  | LPAREN; e = expr; RPAREN
    { e }

record:
  LBRACE; fs = separated_list(SEMI, field); RBRACE
    { Term.Record fs }

field:
    field = ID; EQL; value = expr
    { (field, value) }
