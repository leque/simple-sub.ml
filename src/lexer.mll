{
  open Parser
}

rule token = parse
| [' ' '\t' '\n']
    { token lexbuf }     (* skip blanks *)
| '('
    { LPAREN }
| ')'
    { RPAREN }
| '{'
    { LBRACE }
| '}'
    { RBRACE }
| "."
    { DOT }
| ";"
    { SEMI }
| "let"
    { LET }
| "rec"
    { REC }
| "in"
    { IN }
| "="
    { EQL }
| "fun"
    { FUN }
| "->"
    { ARROW }
| "if"
    { IF }
| "then"
    { THEN }
| "else"
    { ELSE }
| ['0'-'9']+ as n
    { INT (int_of_string n) }
| ['a'-'z']+ as name
    { ID name }
| eof
    { EOF }
