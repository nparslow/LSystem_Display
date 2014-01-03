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
  258 (* LINE *);
  259 (* TURNPHI *);
  260 (* TURN *);
  261 (* MOVENOSCALE *);
  262 (* MOVE *);
  263 (* RECTANGLE *);
  265 (* LBRACE *);
  266 (* RBRACE *);
  267 (* LPAREN *);
  268 (* RPAREN *);
  269 (* LSQBRACKET *);
  270 (* RSQBRACKET *);
  271 (* BRANCH *);
  272 (* SEQUENCE *);
  273 (* EOL *);
    0 (* EOF *);
  274 (* COMMA *);
  275 (* SEMICOLON *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  264 (* SYMBOL *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\005\000\006\000\006\000\006\000\
\003\000\007\000\007\000\007\000\008\000\004\000\009\000\009\000\
\010\000\011\000\012\000\012\000\013\000\013\000\013\000\013\000\
\013\000\013\000\000\000"

let yylen = "\002\000\
\006\000\001\000\003\000\003\000\001\000\003\000\002\000\001\000\
\002\000\001\000\002\000\003\000\005\000\002\000\002\000\003\000\
\005\000\002\000\002\000\003\000\004\000\004\000\004\000\004\000\
\006\000\004\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\005\000\000\000\000\000\027\000\000\000\002\000\
\000\000\000\000\000\000\008\000\000\000\003\000\004\000\000\000\
\000\000\007\000\000\000\000\000\010\000\009\000\000\000\000\000\
\006\000\000\000\011\000\000\000\000\000\000\000\000\000\012\000\
\000\000\014\000\000\000\001\000\000\000\000\000\015\000\000\000\
\013\000\000\000\016\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\018\000\000\000\017\000\000\000\000\000\
\000\000\000\000\000\000\000\000\019\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\020\000\022\000\024\000\023\000\
\026\000\021\000\000\000\000\000\025\000"

let yydgoto = "\002\000\
\006\000\013\000\017\000\030\000\008\000\014\000\022\000\023\000\
\034\000\035\000\045\000\052\000\053\000"

let yysindex = "\005\000\
\253\254\000\000\000\000\015\255\020\255\000\000\008\255\000\000\
\001\255\001\255\021\255\000\000\245\254\000\000\000\000\249\254\
\018\255\000\000\001\255\002\255\000\000\000\000\010\255\023\255\
\000\000\019\255\000\000\249\254\027\255\039\000\253\254\000\000\
\002\255\000\000\012\255\000\000\028\255\024\255\000\000\027\255\
\000\000\030\255\000\000\016\255\029\255\033\255\034\255\035\255\
\036\255\037\255\038\255\000\000\013\255\000\000\049\255\050\255\
\051\255\052\255\053\255\054\255\000\000\016\255\044\255\045\255\
\046\255\047\255\048\255\043\255\000\000\000\000\000\000\000\000\
\000\000\000\000\061\255\055\255\000\000"

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
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\255\255\000\000\000\000\237\255\248\255\035\000\000\000\
\024\000\000\000\000\000\003\000\000\000"

let yytablesize = 67
let yytable = "\007\000\
\026\000\015\000\018\000\020\000\003\000\001\000\021\000\019\000\
\003\000\003\000\025\000\004\000\005\000\038\000\012\000\004\000\
\005\000\046\000\047\000\048\000\049\000\050\000\051\000\027\000\
\011\000\039\000\061\000\009\000\028\000\037\000\040\000\062\000\
\010\000\016\000\024\000\029\000\031\000\033\000\036\000\041\000\
\054\000\042\000\044\000\055\000\056\000\057\000\058\000\059\000\
\060\000\063\000\064\000\065\000\066\000\067\000\068\000\070\000\
\071\000\072\000\073\000\074\000\075\000\076\000\032\000\043\000\
\069\000\000\000\077\000"

let yycheck = "\001\000\
\020\000\010\000\014\001\011\001\008\001\001\000\014\001\019\001\
\008\001\008\001\019\000\015\001\016\001\033\000\014\001\015\001\
\016\001\002\001\003\001\004\001\005\001\006\001\007\001\014\001\
\017\001\014\001\014\001\013\001\019\001\031\000\019\001\019\001\
\013\001\013\001\017\001\013\001\018\001\011\001\000\000\012\001\
\012\001\018\001\013\001\011\001\011\001\011\001\011\001\011\001\
\011\001\001\001\001\001\001\001\001\001\001\001\001\001\012\001\
\012\001\012\001\012\001\012\001\018\001\001\001\028\000\040\000\
\062\000\255\255\012\001"

let yynames_const = "\
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
# 23 "parser.mly"
                                                           ( (_1,_3,_5) )
# 165 "parser.ml"
               : bracketed * rewriting_system * interpretation ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'idsymbol) in
    Obj.repr(
# 26 "parser.mly"
                                              ( S _1 )
# 172 "parser.ml"
               : 'chaine))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list_chaine) in
    Obj.repr(
# 27 "parser.mly"
                                              ( Branch _3 )
# 179 "parser.ml"
               : 'chaine))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list_chaine) in
    Obj.repr(
# 28 "parser.mly"
                                              ( Seq _3 )
# 186 "parser.ml"
               : 'chaine))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 31 "parser.mly"
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
# 219 "parser.ml"
               : 'idsymbol))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'chaine) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list_chaine) in
    Obj.repr(
# 60 "parser.mly"
                                          ( _1 :: _3 )
# 227 "parser.ml"
               : 'list_chaine))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'chaine) in
    Obj.repr(
# 61 "parser.mly"
                                            ( [_1] )
# 234 "parser.ml"
               : 'list_chaine))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "parser.mly"
                                            ( [] )
# 240 "parser.ml"
               : 'list_chaine))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'list_remplacements) in
    Obj.repr(
# 64 "parser.mly"
                                 ( _2 )
# 247 "parser.ml"
               : 'remplacements))
; (fun __caml_parser_env ->
    Obj.repr(
# 67 "parser.mly"
                                    ( [] )
# 253 "parser.ml"
               : 'list_remplacements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'remplacement) in
    Obj.repr(
# 68 "parser.mly"
                                    ( [_1] )
# 260 "parser.ml"
               : 'list_remplacements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'remplacement) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list_remplacements) in
    Obj.repr(
# 69 "parser.mly"
                                                      ( _1 :: _3 )
# 268 "parser.ml"
               : 'list_remplacements))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'idsymbol) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'chaine) in
    Obj.repr(
# 72 "parser.mly"
                                       ( (_2, _4) )
# 276 "parser.ml"
               : 'remplacement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'list_instructions) in
    Obj.repr(
# 75 "parser.mly"
                                 ( _2 )
# 283 "parser.ml"
               : 'instructions))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'instruction) in
    Obj.repr(
# 78 "parser.mly"
                                                 ( [_1] )
# 290 "parser.ml"
               : 'list_instructions))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'instruction) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list_instructions) in
    Obj.repr(
# 79 "parser.mly"
                                                    ( _1 :: _3 )
# 298 "parser.ml"
               : 'list_instructions))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'idsymbol) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'list_orders) in
    Obj.repr(
# 81 "parser.mly"
                                             ( (_2,_4) )
# 306 "parser.ml"
               : 'instruction))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'orders) in
    Obj.repr(
# 84 "parser.mly"
                                 ( _2 )
# 313 "parser.ml"
               : 'list_orders))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'order) in
    Obj.repr(
# 87 "parser.mly"
                                                     ( [_1] )
# 320 "parser.ml"
               : 'orders))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'order) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'orders) in
    Obj.repr(
# 88 "parser.mly"
                                                     ( _1 :: _3 )
# 328 "parser.ml"
               : 'orders))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 91 "parser.mly"
                                          ( Move(_3) )
# 335 "parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 92 "parser.mly"
                                          ( Line(_3) )
# 342 "parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 93 "parser.mly"
                                          ( Turn(_3) )
# 349 "parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 94 "parser.mly"
                                                 ( TurnPhi(_3) )
# 356 "parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 95 "parser.mly"
                                           ( Rectangle(_3,_5) )
# 364 "parser.ml"
               : 'order))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 96 "parser.mly"
                                          ( MoveNoScale(_3) )
# 371 "parser.ml"
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
