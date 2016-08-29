{
  open Simplparser
  open Lexing
  let incr_lineno lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    }
}

rule token = parse
    [' ' '\t'] { token lexbuf }
  | ('\r' | '\n' | "\r\n" ) { incr_lineno lexbuf; token lexbuf }
  | "//" [^'\r' '\n']* { token lexbuf }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | "skip" { SKIP }
  | ';' { SEMICOLON }
  | ',' { COMMA }
  | ":=" { ASSIGN }
  | "->" { MAPSTO }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "while" { WHILE }
  | "do" { DO }
  | "true" { TRUE }
  | "false" { FALSE }
  | "int" { INT }
  | "bool" { BOOL }
  | "fun" { FUN }
  | "<=" { LEQ }
  | "||" { OR }
  | "&&" { AND }
  | "!" { NOT }
  | ['0'-'9']+ { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | ['A'-'Z' 'a'-'z' '_'] (['A'-'Z' 'a'-'z' '0'-'9' '_'])*
        { VAR (Lexing.lexeme lexbuf) }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | eof { EOF }
  | _ { let pos = lexbuf.lex_curr_p in
        raise (Failure ("Line "^(string_of_int pos.pos_lnum)^
          ",char "^(string_of_int (pos.pos_cnum - pos.pos_bol))^
          ": Illegal character: "^(Char.escaped (lexeme_char lexbuf 0))
          )) }

