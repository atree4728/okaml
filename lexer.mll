let digit = ['0'-'9']
let space = ' ' | '\t' | '\r' | '\n'
let alpha = ['a'-'z' 'A'-'Z' '_' ]
let ident = alpha (alpha | digit)*

rule main = parse
| space+       { main lexbuf }
| "+"          { Parser.PLUS }
| "*"          { Parser.TIMES }
| "-"          { Parser.MINUS }
| "/"          { Parser.DIV }
| "="          { Parser.EQ }
| "<"          { Parser.LT }
| "&&"         { Parser.LOGAND }
| "||"         { Parser.LOGOR }
| "let"        { Parser.LET }
| "rec"        { Parser.REC }
| "and"        { Parser.AND }
| "in"         { Parser.IN }
| "if"         { Parser.IF }
| "then"       { Parser.THEN }
| "else"       { Parser.ELSE }
| "match"      { Parser.MATCH }
| "with"       { Parser.WITH }
| "fun"        { Parser.FUN }
| "->"         { Parser.ARROW }
| "|"          { Parser.BAR }
| "true"       { Parser.BOOL (true) }
| "false"      { Parser.BOOL (false) }
| "("          { Parser.LPAR }
| ")"          { Parser.RPAR }
| "["          { Parser.LBRACKET }
| "]"          { Parser.RBRACKET }
| "::"         { Parser.CONS }
| ","          { Parser.COMMA }
| ";;"         { Parser.SEMISEMI }
| ";"          { Parser.SEMI }
| "shift"      { Parser.SHIFT }
| "reset"      { Parser.RESET }
| digit+ as n  { Parser.INT (int_of_string n) }
| ident  as id { Parser.ID id }
| eof          { Parser.EOF }
| _            { failwith ("Unknown Token: " ^ Lexing.lexeme lexbuf)}
