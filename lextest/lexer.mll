(* File lexer.mll *)
        {

	(* Définition de types *)
	open typage;;
        open Parser        (* The type token is defined in parser.mli *)
        exception Eof
        }
        rule token = parse
            [' ' '\t']     { token lexbuf }     (* skip blanks *)
          | ['\n' ]        { EOL }
          | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
	  | 'S'               { S }
          | "Line"            { LINE }
          | "TurnPhi"         { TURNPHI }
          | "Turn"            { TURN }
          | "MoveNoScale"     { MOVENOSCALE }
	  | "Move"            { MOVE }
          | "Rectangle"       { RECTANGLE }
          | ['A'-'Z'] as lxm  { SYMBOL of lxm} 
	  | '{'            { LBRACE }
	  | '}'            { RBRACE }
          | '['            { LSQBRACKET }
	  | ']'            { RSQBRACKET }
          | '('            { LPAREN }
          | ')'            { RPAREN }
          | ','            { COMMA }
	  | ';'            { SEMICOLON }
          | eof            { raise Eof }

