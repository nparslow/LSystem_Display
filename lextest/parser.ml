type symbol = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | T | U | V | W | X | Y | Z;;
(*type symbol = char;;*)

        type bracketed = S of symbol
                 | Seq of bracketed list
                 | Branch of bracketed list;;

        type turtle_command= Move of int
                 | Line of int
                 | Turn of int
                 | TurnPhi of int (* tourner par rapport à l'axe des z *)
                 | Rectangle of int * int
                 | MoveNoScale of int;; (* un déplacement qui n'est pas influencé par l'échelle du dessin - notamment pour Cantor*)

        type rewriting_system = (symbol * bracketed) list;;

type interpretation = (symbol * turtle_command list) list;;
type token =
  | INT of (int)
  | SS
  | LINE
  | TURNPHI
  | TURN
  | MOVENOSCALE
  | MOVE
  | RECTANGLE
  | SYMBOL of (char)
  | LBRACE
  | RBRACE
  | LPAREN
  | RPAREN
  | LSQBRACKET
  | RSQBRACKET
  | BRANCH
  | SEQUENCE
  | EOL
  | EOF
  | COMMA
  | SEMICOLON

open Parsing;;
let yytransl_const = [|
  258 (* SS *);
  259 (* LINE *);
  260 (* TURNPHI *);
  261 (* TURN *);
  262 (* MOVENOSCALE *);
  263 (* MOVE *);
  264 (* RECTANGLE *);
  266 (* LBRACE *);
  267 (* RBRACE *);
  268 (* LPAREN *);
  269 (* RPAREN *);
  270 (* LSQBRACKET *);
  271 (* RSQBRACKET *);
  272 (* BRANCH *);
  273 (* SEQUENCE *);
  274 (* EOL *);
    0 (* EOF *);
  275 (* COMMA *);
  276 (* SEMICOLON *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  265 (* SYMBOL *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\005\000\006\000\006\000\006\000\
\003\000\007\000\007\000\007\000\008\000\004\000\009\000\009\000\
\010\000\011\000\012\000\012\000\013\000\013\000\013\000\013\000\
\013\000\013\000\000\000"

let yylen = "\002\000\
\006\000\002\000\003\000\003\000\001\000\003\000\002\000\001\000\
\002\000\001\000\002\000\003\000\005\000\002\000\002\000\003\000\
\005\000\002\000\002\000\003\000\004\000\004\000\004\000\004\000\
\006\000\004\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\027\000\000\000\005\000\
\002\000\000\000\000\000\000\000\008\000\000\000\003\000\004\000\
\000\000\000\000\007\000\000\000\000\000\010\000\009\000\000\000\
\000\000\006\000\000\000\011\000\000\000\000\000\000\000\000\000\
\012\000\000\000\014\000\000\000\001\000\000\000\000\000\015\000\
\000\000\013\000\000\000\016\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\018\000\000\000\017\000\000\000\
\000\000\000\000\000\000\000\000\000\000\019\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\020\000\022\000\024\000\
\023\000\026\000\021\000\000\000\000\000\025\000"

let yydgoto = "\002\000\
\006\000\014\000\018\000\031\000\009\000\015\000\023\000\024\000\
\035\000\036\000\046\000\053\000\054\000"

let yysindex = "\026\000\
\001\255\000\000\019\255\016\255\018\255\000\000\015\255\000\000\
\000\000\255\254\255\254\020\255\000\000\245\254\000\000\000\000\
\014\255\021\255\000\000\255\254\019\255\000\000\000\000\246\254\
\022\255\000\000\023\255\000\000\014\255\025\255\035\000\001\255\
\000\000\019\255\000\000\248\254\000\000\027\255\024\255\000\000\
\025\255\000\000\030\255\000\000\017\255\028\255\026\255\033\255\
\034\255\035\255\036\255\037\255\000\000\249\254\000\000\049\255\
\050\255\051\255\052\255\053\255\054\255\000\000\017\255\043\255\
\044\255\045\255\046\255\047\255\042\255\000\000\000\000\000\000\
\000\000\000\000\000\000\061\255\055\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\255\255\000\000\000\000\241\255\247\255\034\000\000\000\
\023\000\000\000\000\000\002\000\000\000"

let yytablesize = 68
let yytable = "\007\000\
\003\000\016\000\003\000\019\000\028\000\027\000\040\000\062\000\
\020\000\029\000\026\000\041\000\063\000\013\000\004\000\005\000\
\004\000\005\000\039\000\047\000\048\000\049\000\050\000\051\000\
\052\000\021\000\001\000\008\000\022\000\010\000\038\000\011\000\
\012\000\017\000\037\000\030\000\034\000\056\000\025\000\042\000\
\055\000\032\000\043\000\045\000\057\000\058\000\059\000\060\000\
\061\000\064\000\065\000\066\000\067\000\068\000\069\000\071\000\
\072\000\073\000\074\000\075\000\076\000\077\000\033\000\044\000\
\070\000\000\000\000\000\078\000"

let yycheck = "\001\000\
\002\001\011\000\002\001\015\001\015\001\021\000\015\001\015\001\
\020\001\020\001\020\000\020\001\020\001\015\001\016\001\017\001\
\016\001\017\001\034\000\003\001\004\001\005\001\006\001\007\001\
\008\001\012\001\001\000\009\001\015\001\014\001\032\000\014\001\
\018\001\014\001\000\000\014\001\012\001\012\001\018\001\013\001\
\013\001\019\001\019\001\014\001\012\001\012\001\012\001\012\001\
\012\001\001\001\001\001\001\001\001\001\001\001\001\001\013\001\
\013\001\013\001\013\001\013\001\019\001\001\001\029\000\041\000\
\063\000\255\255\255\255\013\001"

let yynames_const = "\
  SS\000\
  LINE\000\
  TURNPHI\000\
  TURN\000\
  MOVENOSCALE\000\
  MOVE\000\
  RECTANGLE\000\
  LBRACE\000\
  RBRACE\000\
  LPAREN\000\
  RPAREN\000\
  LSQBRACKET\000\
  RSQBRACKET\000\
  BRANCH\000\
  SEQUENCE\000\
  EOL\000\
  EOF\000\
  COMMA\000\
  SEMICOLON\000\
  "

let yynames_block = "\
  INT\000\
  SYMBOL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'chaine) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'remplacements) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'instructions) in
    Obj.repr(
# 24 "parser.mly"
                                                           ( (_1,_3,_5) )
# 168 "parser.ml"
               : bracketed * rewriting_system * interpretation ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'idsymbol) in
    Obj.repr(
# 27 "parser.mly"
                                               ( S _2 )
# 175 "parser.ml"
               : 'chaine))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list_chaine) in
    Obj.repr(
# 28 "parser.mly"
                                              ( Branch _3 )
# 182 "parser.ml"
               : 'chaine))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list_chaine) in
    Obj.repr(
# 29 "parser.mly"
                                              ( Seq _3 )
# 189 "parser.ml"
               : 'chaine))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 32 "parser.mly"
                                              ( match _1 with
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
	                                         | c -> raise (Failure ("symbol inconnue " ^ Char.escaped c))  )
# 222 "parser.ml"
               : 'idsymbol))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'chaine) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list_chaine) in
    Obj.repr(
# 61 "parser.mly"
                                          ( _1 :: _3 )
# 230 "parser.ml"
               : 'list_chaine))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'chaine) in
    Obj.repr(
# 62 "parser.mly"
                                            ( [_1] )
# 237 "parser.ml"
               : 'list_chaine))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "parser.mly"
                                            ( [] )
# 243 "parser.ml"
               : 'list_chaine))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'list_remplacements) in
    Obj.repr(
# 65 "parser.mly"
                                 ( _2 )
# 250 "parser.ml"
               : 'remplacements))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "parser.mly"
                                    ( [] )
# 256 "parser.ml"
               : 'list_remplacements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'remplacement) in
    Obj.repr(
# 69 "parser.mly"
                                    ( [_1] )
# 263 "parser.ml"
               : 'list_remplacements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'remplacement) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list_remplacements) in
    Obj.repr(
# 70 "parser.mly"
                                                      ( _1 :: _3 )
# 271 "parser.ml"
               : 'list_remplacements))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'idsymbol) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'chaine) in
    Obj.repr(
# 73 "parser.mly"
                                       ( (_2, _4) )
# 279 "parser.ml"
               : 'remplacement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'list_instructions) in
    Obj.repr(
# 76 "parser.mly"
                                 ( _2 )
# 286 "parser.ml"
               : 'instructions))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'instruction) in
    Obj.repr(
# 79 "parser.mly"
                                                 ( [_1] )
# 293 "parser.ml"
               : 'list_instructions))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'instruction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list_instructions) in
    Obj.repr(
# 80 "parser.mly"
                                                    ( _1 :: _3 )
# 301 "parser.ml"
               : 'list_instructions))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'idsymbol) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'list_orders) in
    Obj.repr(
# 82 "parser.mly"
                                             ( (_2,_4) )
# 309 "parser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'orders) in
    Obj.repr(
# 85 "parser.mly"
                                 ( _2 )
# 316 "parser.ml"
               : 'list_orders))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'order) in
    Obj.repr(
# 88 "parser.mly"
                                                     ( [_1] )
# 323 "parser.ml"
               : 'orders))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'order) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'orders) in
    Obj.repr(
# 89 "parser.mly"
                                                     ( _1 :: _3 )
# 331 "parser.ml"
               : 'orders))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 92 "parser.mly"
                                          ( Move(_3) )
# 338 "parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 93 "parser.mly"
                                          ( Line(_3) )
# 345 "parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 94 "parser.mly"
                                          ( Turn(_3) )
# 352 "parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 95 "parser.mly"
                                                 ( TurnPhi(_3) )
# 359 "parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 96 "parser.mly"
                                           ( Rectangle(_3,_5) )
# 367 "parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 97 "parser.mly"
                                          ( MoveNoScale(_3) )
# 374 "parser.ml"
               : 'order))
(* Entry main *)
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
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : bracketed * rewriting_system * interpretation )
