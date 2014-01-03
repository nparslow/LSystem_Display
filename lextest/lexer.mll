(* File lexer.mll *)
        {
        open Parser        (* The type token is defined in parser.mli *)
        exception Eof
        }
        rule token = parse
            [' ' '\t']     { token lexbuf }     (* skip blanks *)
          | ['\n' ]        { EOL }
          | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
	  | 'S'               { SS }
          | "Line"            { LINE }
          | "TurnPhi"         { TURNPHI }
          | "Turn"            { TURN }
          | "MoveNoScale"     { MOVENOSCALE }
	  | "Move"            { MOVE }
          | "Rectangle"       { RECTANGLE }
          | ['A'-'R''T'-'Z'] as lxm  { SYMBOL(lxm)} 
	  | '{'            { LBRACE }
	  | '}'            { RBRACE }
          | '['            { LSQBRACKET }
	  | ']'            { RSQBRACKET }
          | '('            { LPAREN }
          | ')'            { RPAREN }
          | ','            { COMMA }
	  | ';'            { SEMICOLON }
          | eof            { EOF }

