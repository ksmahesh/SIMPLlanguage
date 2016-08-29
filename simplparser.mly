%{
  open Simpltypes
  open Parsing
  open Lexing
  let linfo () =
    ((let p = symbol_start_pos () in (p.pos_lnum, p.pos_cnum - p.pos_bol + 1)),
     (let p = symbol_end_pos () in (p.pos_lnum, p.pos_cnum - p.pos_bol)));;
  let linfo_range x y =
    ((let p = rhs_start_pos x in (p.pos_lnum, p.pos_cnum - p.pos_bol + 1)),
     (let p = rhs_end_pos y in (p.pos_lnum, p.pos_cnum - p.pos_bol)));;
  let report_error pos1 pos2 msg =
    failwith ("Line "^(string_of_int pos1.pos_lnum)^", char"^
      (if pos1.pos_cnum = pos2.pos_cnum then
         " "^(string_of_int (pos1.pos_cnum - pos1.pos_bol + 1))
       else
         ("s "^(string_of_int (pos1.pos_cnum - pos1.pos_bol + 1))^
          "-"^(string_of_int (pos2.pos_cnum - pos1.pos_bol + 1))))^": "^msg);;
  let unmatched opening_name opening_num closing_name =
    report_error (rhs_start_pos opening_num) (rhs_end_pos opening_num)
      ("Found "^opening_name^" with no matching "^closing_name^".");;
  let invalid num = report_error (rhs_start_pos num) (rhs_end_pos num)
  let parse_error msg =
    report_error (symbol_start_pos ()) (symbol_end_pos ()) msg;;
  let missing num =
    report_error (rhs_end_pos num) (rhs_end_pos num) "missing semicolon";;
%}

%token LPAREN RPAREN SEMICOLON ASSIGN LEQ OR AND NOT PLUS MINUS TIMES EOF
%token SKIP IF THEN ELSE WHILE DO TRUE FALSE INT BOOL FUN MAPSTO COMMA
%token LBRACE RBRACE
%token<int> NUM
%token<string> VAR

%right SEMICOLON MAPSTO COMMA
%nonassoc SKIP ASSIGN IF THEN ELSE WHILE DO INT BOOL FUN

%left OR
%left AND
%right NOT
%nonassoc TRUE FALSE LEQ

%left PLUS MINUS
%left TIMES
%nonassoc NUM VAR

%right LPAREN RPAREN LBRACE RBRACE
%nonassoc EOF

%start parse_cmd
%type<Simpltypes.icmd> parse_cmd

%%

parse_cmd: cmd { $1 }
         | cmd SEMICOLON parse_cmd { Seq ($1,$3,linfo()) }
         | cmd SEMICOLON { invalid 2 "superfluous semicolon" }
         | cmd simplecmd { missing 1 }
;

cmd: simplecmd { $1 }
   | LPAREN parse_cmd RPAREN { $2 }
   | LPAREN parse_cmd error { unmatched "(" 1 ")" }
;

simplecmd: SKIP { Skip (linfo()) }
         | VAR ASSIGN parse_expr { Assign ($1,$3,linfo()) }
         | IF parse_expr THEN cmd ELSE cmd { Cond ($2,$4,$6,linfo()) }
         | IF parse_expr THEN cmd error { unmatched "if" 1 "else" }
         | IF parse_expr error { unmatched "if" 1 "then" }
         | WHILE parse_expr DO cmd { While ($2,$4,linfo()) }
         | WHILE parse_expr error { unmatched "while" 1 "do" }
         | INT VAR { Decl (TypInt,$2,linfo()) }
         | BOOL VAR { Decl (TypBool,$2,linfo()) }
         | FUN LPAREN arg_typs MAPSTO arg_typ RPAREN VAR
             { Decl (TypFunc ($3,$5), $7, linfo()) }
         | FUN LPAREN arg_typs MAPSTO arg_typ error { unmatched "(" 2 ")" }
         | FUN LPAREN arg_typs error { unmatched "fun" 1 "->" }
;

arg_typ: INT { TypInt }
       | BOOL { TypBool }
       | FUN LPAREN arg_typs MAPSTO arg_typ RPAREN { TypFunc ($3,$5) }
       | FUN LPAREN arg_typs MAPSTO arg_typ error { unmatched "(" 2 ")" }
       | FUN LPAREN arg_typs error { unmatched "fun" 1 "->" }
       | LPAREN arg_typ RPAREN { $2 }
       | LPAREN arg_typ error { unmatched "(" 1 ")" }
;

arg_typs: { [] }
        | arg_typ { [$1] }
        | arg_typ TIMES arg_typs { $1::$3 }
;

parse_expr: TRUE { True (linfo()) }
          | FALSE { False (linfo()) }
          | parse_expr LEQ parse_expr { Leq ($1,$3,linfo()) }
          | parse_expr AND parse_expr { Conj ($1,$3,linfo()) }
          | parse_expr OR parse_expr { Disj ($1,$3,linfo()) }
          | NOT parse_expr { Neg ($2,linfo()) }
          | NUM { Const ($1,linfo()) }
          | MINUS NUM { Const (-$2,linfo()) }
          | VAR { Var ($1,linfo()) }
          | parse_expr PLUS parse_expr { Plus ($1,$3,linfo()) }
          | parse_expr MINUS parse_expr { Minus ($1,$3,linfo()) }
          | parse_expr TIMES parse_expr { Times ($1,$3,linfo()) }
          | FUN LPAREN formals RPAREN LBRACE parse_cmd RBRACE
              { Abstraction ($3,$6,linfo()) }
          | FUN LPAREN formals RPAREN LBRACE error { unmatched "{" 5 "}" }
          | FUN LPAREN formals error { unmatched "(" 2 ")" }
          | parse_expr LPAREN expr_list RPAREN { Apply ($1,$3,linfo()) }
          | LPAREN parse_expr RPAREN { $2 }
          | LPAREN parse_expr error { unmatched "(" 1 ")" }
;

formals: { [] }
       | arg_typ VAR { [($2,$1,linfo())] }
       | arg_typ VAR COMMA formals { ($2,$1,linfo_range 1 2)::$4 }
;

expr_list: { [] }
         | parse_expr { [$1] }
         | parse_expr COMMA expr_list { $1::$3 }
;

