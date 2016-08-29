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

val parse_cmd :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Simpltypes.icmd
