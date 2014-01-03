/* File parser.mly */
        %token <int> INT
	%token LINE
	%token TURNPHI
	%token TURN
	%token MOVENOSCALE
	%token MOVE
	%token RECTANGLE
	%token <char> SYMBOL
	%token LBRACE RBRACE
        %token LPAREN RPAREN
	%token LSQBRACKET RSQBRACKET
        %token BRANCH
        %token SEQUENCE
        %token EOL
        %token EOF
	%token COMMA
	%token SEMICOLON
        %start main             /* the entry point */
        %type <bracketed * rewriting_system * interpretation > main
        %%
        main:
            chaine EOL remplacements EOL instructions EOF  { ($1,$3,$5) }
        ;
        chaine:
          idsymbol                            { S $1 }
          | BRANCH LSQBRACKET list_chaine     { Branch $3 }
          | SEQUENCE LSQBRACKET list_chaine   { Seq $3 }
        ;
        idsymbol:
          SYMBOL                              { match $1 with
	                                         | 'A' -> A
	                                         | 'B' -> B
	                                         | 'C' -> C
	                                         | 'D' -> D
	                                         | 'E' -> E
	                                         | 'F' -> F
	                                         | 'G' -> G
	                                         | 'H' -> H
	                                         | 'I' -> I
	                                         | 'J' -> J
	                                         | 'K' -> K
	                                         | 'L' -> L
	                                         | 'M' -> M
	                                         | 'N' -> N
	                                         | 'O' -> O
	                                         | 'P' -> P
	                                         | 'Q' -> Q
	                                         | 'R' -> R
	                                         | 'T' -> T
	                                         | 'U' -> U
	                                         | 'V' -> V
	                                         | 'W' -> W
	                                         | 'X' -> X
	                                         | 'Y' -> Y
	                                         | 'Z' -> Z
	                                         | c -> raise (Failure ("symbol inconnue " ^ Char.escaped c))  }
        ;
        list_chaine:
          chaine SEMICOLON list_chaine    { $1 :: $3 }
          | chaine RSQBRACKET               { [$1] }
          | RSQBRACKET                      { [] }
	remplacements:
	  LSQBRACKET list_remplacements { $2 }
	;
	list_remplacements:
          RSQBRACKET                { [] }
          | remplacement RSQBRACKET { [$1] }
          | remplacement SEMICOLON list_remplacements { $1 :: $3 }
        ;
        remplacement:
	  LPAREN idsymbol COMMA chaine RPAREN { ($2, $4) }
        ;
	instructions:
	  LSQBRACKET list_instructions  { $2 }
	;
        list_instructions:
          instruction RSQBRACKET                 { [$1] }
          | instruction SEMICOLON list_instructions { $1 :: $3 }
        instruction:
	   LPAREN idsymbol COMMA list_orders RPAREN { ($2,$4) }
	;
	list_orders:
	   LSQBRACKET orders            { $2 }
        ;
        orders:
           order RSQBRACKET                          { [$1] }
           | order SEMICOLON orders                  { $1 :: $3 }
        ;
        order:
	  MOVE LPAREN INT RPAREN                 { Move($3) }
	  | LINE LPAREN INT RPAREN               { Line($3) }
	  | TURN LPAREN INT RPAREN               { Turn($3) }
          | TURNPHI LPAREN INT RPAREN            { TurnPhi($3) }
	  | RECTANGLE LPAREN INT COMMA INT RPAREN { Rectangle($3,$5) }
	  | MOVENOSCALE LPAREN INT RPAREN        { MoveNoScale($3) }
	;
