type token =
  | LPAREN
  | RPAREN
  | SEMICOLON
  | ASSIGN
  | LEQ
  | OR
  | AND
  | NOT
  | PLUS
  | MINUS
  | TIMES
  | EOF
  | SKIP
  | IF
  | THEN
  | ELSE
  | WHILE
  | DO
  | TRUE
  | FALSE
  | INT
  | BOOL
  | FUN
  | MAPSTO
  | COMMA
  | LBRACE
  | RBRACE
  | NUM of (int)
  | VAR of (string)

open Parsing;;
let _ = parse_error;;
# 1 "simplparser.mly"

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
# 61 "simplparser.ml"
let yytransl_const = [|
  257 (* LPAREN *);
  258 (* RPAREN *);
  259 (* SEMICOLON *);
  260 (* ASSIGN *);
  261 (* LEQ *);
  262 (* OR *);
  263 (* AND *);
  264 (* NOT *);
  265 (* PLUS *);
  266 (* MINUS *);
  267 (* TIMES *);
    0 (* EOF *);
  268 (* SKIP *);
  269 (* IF *);
  270 (* THEN *);
  271 (* ELSE *);
  272 (* WHILE *);
  273 (* DO *);
  274 (* TRUE *);
  275 (* FALSE *);
  276 (* INT *);
  277 (* BOOL *);
  278 (* FUN *);
  279 (* MAPSTO *);
  280 (* COMMA *);
  281 (* LBRACE *);
  282 (* RBRACE *);
    0|]

let yytransl_block = [|
  283 (* NUM *);
  284 (* VAR *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\002\000\002\000\002\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\005\000\005\000\005\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\007\000\
\007\000\007\000\008\000\008\000\008\000\000\000"

let yylen = "\002\000\
\001\000\003\000\002\000\002\000\001\000\003\000\003\000\001\000\
\003\000\006\000\005\000\003\000\004\000\003\000\002\000\002\000\
\007\000\006\000\004\000\001\000\001\000\006\000\006\000\004\000\
\003\000\003\000\000\000\001\000\003\000\001\000\001\000\003\000\
\003\000\003\000\002\000\001\000\002\000\001\000\003\000\003\000\
\003\000\007\000\006\000\004\000\004\000\003\000\003\000\000\000\
\002\000\004\000\000\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\008\000\000\000\000\000\000\000\000\000\
\000\000\000\000\054\000\000\000\005\000\000\000\000\000\000\000\
\000\000\030\000\031\000\000\000\036\000\038\000\000\000\000\000\
\015\000\016\000\000\000\000\000\000\000\004\000\007\000\006\000\
\000\000\000\000\037\000\000\000\012\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\014\000\000\000\000\000\
\020\000\021\000\000\000\000\000\000\000\000\000\002\000\047\000\
\046\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\013\000\000\000\000\000\019\000\
\000\000\000\000\000\000\044\000\000\000\000\000\045\000\011\000\
\000\000\026\000\025\000\000\000\000\000\029\000\000\000\000\000\
\053\000\010\000\024\000\000\000\018\000\000\000\050\000\043\000\
\000\000\000\000\017\000\042\000\023\000\022\000"

let yydgoto = "\002\000\
\011\000\012\000\013\000\060\000\052\000\053\000\059\000\061\000"

let yysindex = "\004\000\
\076\255\000\000\076\255\000\000\092\255\092\255\244\254\008\255\
\048\255\068\255\000\000\058\255\000\000\027\255\092\255\092\255\
\035\255\000\000\000\000\072\255\000\000\000\000\134\255\116\255\
\000\000\000\000\030\255\092\255\076\255\000\000\000\000\000\000\
\149\255\174\255\000\000\030\255\000\000\092\255\092\255\092\255\
\092\255\092\255\092\255\092\255\076\255\000\000\076\255\030\255\
\000\000\000\000\075\255\009\255\073\255\160\255\000\000\000\000\
\000\000\059\255\081\255\014\255\088\255\055\255\167\255\174\255\
\002\255\002\255\093\255\022\255\000\000\099\255\030\255\000\000\
\030\255\030\255\071\255\000\000\082\255\092\255\000\000\000\000\
\076\255\000\000\000\000\010\255\103\255\000\000\030\255\047\255\
\000\000\000\000\000\000\030\255\000\000\084\255\000\000\000\000\
\098\255\106\255\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\091\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\011\255\000\000\118\000\000\000\000\000\000\000\
\000\000\082\000\000\000\113\255\000\000\127\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\012\255\190\000\000\000\000\000\
\000\000\000\000\000\000\129\255\000\000\109\000\163\000\136\000\
\028\000\055\000\001\000\000\000\000\000\000\000\011\255\000\000\
\000\000\011\255\128\255\000\000\000\000\127\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\113\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\253\255\215\255\120\000\002\000\239\255\222\255\050\000\060\000"

let yytablesize = 474
let yytable = "\014\000\
\041\000\058\000\038\000\068\000\001\000\069\000\023\000\024\000\
\072\000\091\000\027\000\028\000\044\000\070\000\038\000\025\000\
\033\000\034\000\039\000\040\000\041\000\080\000\042\000\043\000\
\044\000\055\000\031\000\039\000\032\000\054\000\048\000\073\000\
\092\000\027\000\028\000\026\000\081\000\078\000\085\000\090\000\
\062\000\063\000\064\000\065\000\066\000\067\000\096\000\003\000\
\027\000\049\000\050\000\051\000\058\000\084\000\040\000\038\000\
\086\000\098\000\004\000\005\000\029\000\035\000\006\000\042\000\
\043\000\044\000\007\000\008\000\009\000\004\000\005\000\028\000\
\036\000\006\000\010\000\071\000\003\000\007\000\008\000\009\000\
\076\000\035\000\077\000\074\000\097\000\010\000\075\000\004\000\
\005\000\079\000\001\000\006\000\015\000\038\000\087\000\007\000\
\008\000\009\000\082\000\016\000\083\000\017\000\093\000\010\000\
\094\000\101\000\088\000\102\000\032\000\018\000\019\000\099\000\
\048\000\020\000\048\000\046\000\038\000\003\000\021\000\022\000\
\039\000\040\000\041\000\100\000\042\000\043\000\044\000\049\000\
\051\000\049\000\052\000\030\000\047\000\037\000\038\000\033\000\
\095\000\089\000\039\000\040\000\041\000\000\000\042\000\043\000\
\044\000\000\000\000\000\045\000\056\000\038\000\057\000\000\000\
\000\000\039\000\040\000\041\000\000\000\042\000\043\000\044\000\
\038\000\000\000\034\000\000\000\039\000\040\000\041\000\038\000\
\042\000\043\000\044\000\039\000\000\000\041\000\038\000\042\000\
\043\000\044\000\039\000\000\000\000\000\000\000\042\000\043\000\
\044\000\000\000\000\000\000\000\000\000\009\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\041\000\000\000\041\000\041\000\000\000\041\000\041\000\041\000\
\000\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
\041\000\041\000\000\000\000\000\041\000\041\000\041\000\000\000\
\041\000\000\000\041\000\039\000\041\000\039\000\039\000\000\000\
\039\000\039\000\039\000\000\000\039\000\039\000\000\000\039\000\
\039\000\039\000\039\000\039\000\039\000\000\000\000\000\039\000\
\039\000\039\000\000\000\039\000\000\000\039\000\040\000\039\000\
\040\000\040\000\000\000\040\000\040\000\040\000\000\000\040\000\
\040\000\000\000\040\000\040\000\040\000\040\000\040\000\040\000\
\000\000\000\000\040\000\040\000\040\000\000\000\040\000\000\000\
\040\000\035\000\040\000\035\000\035\000\000\000\000\000\035\000\
\035\000\000\000\001\000\000\000\001\000\035\000\035\000\035\000\
\035\000\035\000\035\000\000\000\000\000\035\000\035\000\035\000\
\000\000\035\000\000\000\035\000\032\000\035\000\032\000\032\000\
\000\000\000\000\032\000\032\000\001\000\003\000\000\000\003\000\
\032\000\032\000\032\000\032\000\032\000\032\000\000\000\000\000\
\032\000\032\000\032\000\000\000\032\000\000\000\032\000\033\000\
\032\000\033\000\033\000\000\000\000\000\033\000\033\000\003\000\
\000\000\000\000\000\000\033\000\033\000\033\000\033\000\033\000\
\033\000\000\000\000\000\033\000\033\000\033\000\000\000\033\000\
\000\000\033\000\034\000\033\000\034\000\034\000\000\000\000\000\
\034\000\000\000\000\000\000\000\000\000\000\000\034\000\034\000\
\034\000\034\000\034\000\034\000\000\000\000\000\034\000\034\000\
\034\000\000\000\034\000\000\000\034\000\009\000\034\000\009\000\
\009\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\009\000\009\000\000\000\009\000\009\000\000\000\000\000\
\000\000\009\000\009\000\009\000\000\000\000\000\000\000\009\000\
\000\000\009\000"

let yycheck = "\003\000\
\000\000\036\000\001\001\045\000\001\000\047\000\005\000\006\000\
\000\001\000\001\000\001\000\001\011\001\048\000\001\001\028\001\
\015\000\016\000\005\001\006\001\007\001\000\001\009\001\010\001\
\011\001\029\000\000\001\000\000\002\001\028\000\001\001\023\001\
\023\001\023\001\023\001\028\001\015\001\024\001\073\000\081\000\
\039\000\040\000\041\000\042\000\043\000\044\000\000\001\001\001\
\001\001\020\001\021\001\022\001\087\000\071\000\000\000\001\001\
\074\000\092\000\012\001\013\001\003\001\027\001\016\001\009\001\
\010\001\011\001\020\001\021\001\022\001\012\001\013\001\004\001\
\001\001\016\001\028\001\001\001\001\001\020\001\021\001\022\001\
\000\001\000\000\002\001\011\001\088\000\028\001\028\001\012\001\
\013\001\002\001\000\000\016\001\001\001\001\001\024\001\020\001\
\021\001\022\001\000\001\008\001\002\001\010\001\000\001\028\001\
\002\001\000\001\025\001\002\001\000\000\018\001\019\001\028\001\
\000\001\022\001\002\001\000\001\001\001\000\000\027\001\028\001\
\005\001\006\001\007\001\026\001\009\001\010\001\011\001\000\001\
\002\001\002\001\002\001\012\000\017\001\000\001\001\001\000\000\
\087\000\078\000\005\001\006\001\007\001\255\255\009\001\010\001\
\011\001\255\255\255\255\014\001\000\001\001\001\002\001\255\255\
\255\255\005\001\006\001\007\001\255\255\009\001\010\001\011\001\
\001\001\255\255\000\000\255\255\005\001\006\001\007\001\001\001\
\009\001\010\001\011\001\005\001\255\255\007\001\001\001\009\001\
\010\001\011\001\005\001\255\255\255\255\255\255\009\001\010\001\
\011\001\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\000\001\255\255\002\001\003\001\255\255\005\001\006\001\007\001\
\255\255\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\255\255\255\255\020\001\021\001\022\001\255\255\
\024\001\255\255\026\001\000\001\028\001\002\001\003\001\255\255\
\005\001\006\001\007\001\255\255\009\001\010\001\255\255\012\001\
\013\001\014\001\015\001\016\001\017\001\255\255\255\255\020\001\
\021\001\022\001\255\255\024\001\255\255\026\001\000\001\028\001\
\002\001\003\001\255\255\005\001\006\001\007\001\255\255\009\001\
\010\001\255\255\012\001\013\001\014\001\015\001\016\001\017\001\
\255\255\255\255\020\001\021\001\022\001\255\255\024\001\255\255\
\026\001\000\001\028\001\002\001\003\001\255\255\255\255\006\001\
\007\001\255\255\000\001\255\255\002\001\012\001\013\001\014\001\
\015\001\016\001\017\001\255\255\255\255\020\001\021\001\022\001\
\255\255\024\001\255\255\026\001\000\001\028\001\002\001\003\001\
\255\255\255\255\006\001\007\001\026\001\000\001\255\255\002\001\
\012\001\013\001\014\001\015\001\016\001\017\001\255\255\255\255\
\020\001\021\001\022\001\255\255\024\001\255\255\026\001\000\001\
\028\001\002\001\003\001\255\255\255\255\006\001\007\001\026\001\
\255\255\255\255\255\255\012\001\013\001\014\001\015\001\016\001\
\017\001\255\255\255\255\020\001\021\001\022\001\255\255\024\001\
\255\255\026\001\000\001\028\001\002\001\003\001\255\255\255\255\
\006\001\255\255\255\255\255\255\255\255\255\255\012\001\013\001\
\014\001\015\001\016\001\017\001\255\255\255\255\020\001\021\001\
\022\001\255\255\024\001\255\255\026\001\000\001\028\001\002\001\
\003\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\012\001\013\001\255\255\015\001\016\001\255\255\255\255\
\255\255\020\001\021\001\022\001\255\255\255\255\255\255\026\001\
\255\255\028\001"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  SEMICOLON\000\
  ASSIGN\000\
  LEQ\000\
  OR\000\
  AND\000\
  NOT\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  EOF\000\
  SKIP\000\
  IF\000\
  THEN\000\
  ELSE\000\
  WHILE\000\
  DO\000\
  TRUE\000\
  FALSE\000\
  INT\000\
  BOOL\000\
  FUN\000\
  MAPSTO\000\
  COMMA\000\
  LBRACE\000\
  RBRACE\000\
  "

let yynames_block = "\
  NUM\000\
  VAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cmd) in
    Obj.repr(
# 54 "simplparser.mly"
               ( _1 )
# 333 "simplparser.ml"
               : Simpltypes.icmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cmd) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Simpltypes.icmd) in
    Obj.repr(
# 55 "simplparser.mly"
                                   ( Seq (_1,_3,linfo()) )
# 341 "simplparser.ml"
               : Simpltypes.icmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'cmd) in
    Obj.repr(
# 56 "simplparser.mly"
                         ( invalid 2 "superfluous semicolon" )
# 348 "simplparser.ml"
               : Simpltypes.icmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'cmd) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'simplecmd) in
    Obj.repr(
# 57 "simplparser.mly"
                         ( missing 1 )
# 356 "simplparser.ml"
               : Simpltypes.icmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simplecmd) in
    Obj.repr(
# 60 "simplparser.mly"
               ( _1 )
# 363 "simplparser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Simpltypes.icmd) in
    Obj.repr(
# 61 "simplparser.mly"
                             ( _2 )
# 370 "simplparser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Simpltypes.icmd) in
    Obj.repr(
# 62 "simplparser.mly"
                            ( unmatched "(" 1 ")" )
# 377 "simplparser.ml"
               : 'cmd))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "simplparser.mly"
                ( Skip (linfo()) )
# 383 "simplparser.ml"
               : 'simplecmd))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'parse_expr) in
    Obj.repr(
# 66 "simplparser.mly"
                                 ( Assign (_1,_3,linfo()) )
# 391 "simplparser.ml"
               : 'simplecmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'parse_expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'cmd) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'cmd) in
    Obj.repr(
# 67 "simplparser.mly"
                                           ( Cond (_2,_4,_6,linfo()) )
# 400 "simplparser.ml"
               : 'simplecmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'parse_expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'cmd) in
    Obj.repr(
# 68 "simplparser.mly"
                                        ( unmatched "if" 1 "else" )
# 408 "simplparser.ml"
               : 'simplecmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'parse_expr) in
    Obj.repr(
# 69 "simplparser.mly"
                               ( unmatched "if" 1 "then" )
# 415 "simplparser.ml"
               : 'simplecmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'parse_expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'cmd) in
    Obj.repr(
# 70 "simplparser.mly"
                                   ( While (_2,_4,linfo()) )
# 423 "simplparser.ml"
               : 'simplecmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'parse_expr) in
    Obj.repr(
# 71 "simplparser.mly"
                                  ( unmatched "while" 1 "do" )
# 430 "simplparser.ml"
               : 'simplecmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 72 "simplparser.mly"
                   ( Decl (TypInt,_2,linfo()) )
# 437 "simplparser.ml"
               : 'simplecmd))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 73 "simplparser.mly"
                    ( Decl (TypBool,_2,linfo()) )
# 444 "simplparser.ml"
               : 'simplecmd))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'arg_typs) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'arg_typ) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 75 "simplparser.mly"
             ( Decl (TypFunc (_3,_5), _7, linfo()) )
# 453 "simplparser.ml"
               : 'simplecmd))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'arg_typs) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'arg_typ) in
    Obj.repr(
# 76 "simplparser.mly"
                                                    ( unmatched "(" 2 ")" )
# 461 "simplparser.ml"
               : 'simplecmd))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'arg_typs) in
    Obj.repr(
# 77 "simplparser.mly"
                                     ( unmatched "fun" 1 "->" )
# 468 "simplparser.ml"
               : 'simplecmd))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "simplparser.mly"
             ( TypInt )
# 474 "simplparser.ml"
               : 'arg_typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 81 "simplparser.mly"
              ( TypBool )
# 480 "simplparser.ml"
               : 'arg_typ))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'arg_typs) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'arg_typ) in
    Obj.repr(
# 82 "simplparser.mly"
                                                   ( TypFunc (_3,_5) )
# 488 "simplparser.ml"
               : 'arg_typ))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'arg_typs) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'arg_typ) in
    Obj.repr(
# 83 "simplparser.mly"
                                                  ( unmatched "(" 2 ")" )
# 496 "simplparser.ml"
               : 'arg_typ))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'arg_typs) in
    Obj.repr(
# 84 "simplparser.mly"
                                   ( unmatched "fun" 1 "->" )
# 503 "simplparser.ml"
               : 'arg_typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'arg_typ) in
    Obj.repr(
# 85 "simplparser.mly"
                               ( _2 )
# 510 "simplparser.ml"
               : 'arg_typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'arg_typ) in
    Obj.repr(
# 86 "simplparser.mly"
                              ( unmatched "(" 1 ")" )
# 517 "simplparser.ml"
               : 'arg_typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "simplparser.mly"
          ( [] )
# 523 "simplparser.ml"
               : 'arg_typs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg_typ) in
    Obj.repr(
# 90 "simplparser.mly"
                  ( [_1] )
# 530 "simplparser.ml"
               : 'arg_typs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg_typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arg_typs) in
    Obj.repr(
# 91 "simplparser.mly"
                                 ( _1::_3 )
# 538 "simplparser.ml"
               : 'arg_typs))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "simplparser.mly"
                 ( True (linfo()) )
# 544 "simplparser.ml"
               : 'parse_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "simplparser.mly"
                  ( False (linfo()) )
# 550 "simplparser.ml"
               : 'parse_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'parse_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'parse_expr) in
    Obj.repr(
# 96 "simplparser.mly"
                                      ( Leq (_1,_3,linfo()) )
# 558 "simplparser.ml"
               : 'parse_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'parse_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'parse_expr) in
    Obj.repr(
# 97 "simplparser.mly"
                                      ( Conj (_1,_3,linfo()) )
# 566 "simplparser.ml"
               : 'parse_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'parse_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'parse_expr) in
    Obj.repr(
# 98 "simplparser.mly"
                                     ( Disj (_1,_3,linfo()) )
# 574 "simplparser.ml"
               : 'parse_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'parse_expr) in
    Obj.repr(
# 99 "simplparser.mly"
                           ( Neg (_2,linfo()) )
# 581 "simplparser.ml"
               : 'parse_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 100 "simplparser.mly"
                ( Const (_1,linfo()) )
# 588 "simplparser.ml"
               : 'parse_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 101 "simplparser.mly"
                      ( Const (-_2,linfo()) )
# 595 "simplparser.ml"
               : 'parse_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 102 "simplparser.mly"
                ( Var (_1,linfo()) )
# 602 "simplparser.ml"
               : 'parse_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'parse_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'parse_expr) in
    Obj.repr(
# 103 "simplparser.mly"
                                       ( Plus (_1,_3,linfo()) )
# 610 "simplparser.ml"
               : 'parse_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'parse_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'parse_expr) in
    Obj.repr(
# 104 "simplparser.mly"
                                        ( Minus (_1,_3,linfo()) )
# 618 "simplparser.ml"
               : 'parse_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'parse_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'parse_expr) in
    Obj.repr(
# 105 "simplparser.mly"
                                        ( Times (_1,_3,linfo()) )
# 626 "simplparser.ml"
               : 'parse_expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'formals) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : Simpltypes.icmd) in
    Obj.repr(
# 107 "simplparser.mly"
              ( Abstraction (_3,_6,linfo()) )
# 634 "simplparser.ml"
               : 'parse_expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'formals) in
    Obj.repr(
# 108 "simplparser.mly"
                                                   ( unmatched "{" 5 "}" )
# 641 "simplparser.ml"
               : 'parse_expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'formals) in
    Obj.repr(
# 109 "simplparser.mly"
                                     ( unmatched "(" 2 ")" )
# 648 "simplparser.ml"
               : 'parse_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'parse_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list) in
    Obj.repr(
# 110 "simplparser.mly"
                                               ( Apply (_1,_3,linfo()) )
# 656 "simplparser.ml"
               : 'parse_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'parse_expr) in
    Obj.repr(
# 111 "simplparser.mly"
                                     ( _2 )
# 663 "simplparser.ml"
               : 'parse_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'parse_expr) in
    Obj.repr(
# 112 "simplparser.mly"
                                    ( unmatched "(" 1 ")" )
# 670 "simplparser.ml"
               : 'parse_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 115 "simplparser.mly"
         ( [] )
# 676 "simplparser.ml"
               : 'formals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'arg_typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 116 "simplparser.mly"
                     ( [(_2,_1,linfo())] )
# 684 "simplparser.ml"
               : 'formals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'arg_typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'formals) in
    Obj.repr(
# 117 "simplparser.mly"
                                   ( (_2,_1,linfo_range 1 2)::_4 )
# 693 "simplparser.ml"
               : 'formals))
; (fun __caml_parser_env ->
    Obj.repr(
# 120 "simplparser.mly"
           ( [] )
# 699 "simplparser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'parse_expr) in
    Obj.repr(
# 121 "simplparser.mly"
                      ( [_1] )
# 706 "simplparser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'parse_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_list) in
    Obj.repr(
# 122 "simplparser.mly"
                                      ( _1::_3 )
# 714 "simplparser.ml"
               : 'expr_list))
(* Entry parse_cmd *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let parse_cmd (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Simpltypes.icmd)
