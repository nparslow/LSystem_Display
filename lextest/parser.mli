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

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> bracketed * rewriting_system * interpretation 
