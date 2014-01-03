
(*
#directory "+camlimages";;
#use "topfind";; 
#require "camlimages.all_formats";;
#require "camlimages.graphics";;
#load "graphics.cma"
#load "unix.cma";;
*)

(* ocamlfind ocamlc -package camlimages.all_formats -package labltk -package graphics -package camlimages.graphics unix.cma -linkpkg projet_v049c.ml *)


open Arg;;
open Images;;
open Graphics;;

let n = ref 1;;
let grow = ref false;;
let clear = ref true;;
let exemple = ref "snow";;
let arguments = ref[];;

let opt_spec = [
  ("-n", Set_int n, "Il faut préciser un nombre d'itérations n");
  ("-grow", Unit (function () -> grow := not !grow), "Faire agrandir le graphisme");
  ("-clear", Unit (function () -> clear := not !clear), "Ne pas effacer le dessin à chaque fois");
  ("-exemple", Set_string exemple, "choisir un exemple")
];;

let arg_action = (function s -> arguments:= !arguments @ [s]);;
let usage = "how to use";;

parse opt_spec arg_action usage;;

(* detecter des problemes *)
(*if !n = 0 then begin
prerr_endline "Erreur : pas de n donne";
exit 0

end;;*)

let thetaInit = ref 0;;
let phiInit = ref 90;;

type symbol = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z;;
(*type symbol = char;;*)

type bracketed = S of symbol
                 | Seq of bracketed list
                 | Branch of bracketed list;;

type turtle_command= Move of int
                 | Line of int
                 | Turn of int
		 | TurnPhi of int (* tourner par rapport a la z axis *)
                 | Rectangle of int * int
                 | MoveNoScale of int;;

type rewriting_system = (symbol * bracketed) list;;

type interpretation = (symbol * turtle_command list) list;;

(* Substitutions *)
let rec rewrite rws brack =
  let rec in_rewrite rws brack = match brack with
  | S s -> (List.assoc s rws)
  | Seq seq -> Seq (List.map (rewrite rws) seq)
  | Branch b -> Branch (List.map (rewrite rws) b)
in try in_rewrite rws brack
  with Not_found -> brack
  ;;

(* Calculer la chaine avec n iterations *)
let rec calc_n_chaine chaine rws n =
if n <= 1 then chaine
else rewrite rws (calc_n_chaine chaine rws (n-1));;

(* Calculer la taille = no. total d'operations d'une sequence *)
let rec taille t = match t with
  | S s -> 1
  | Branch b -> (match b with
                 | [] -> 0
                 | a::l -> (taille a) + (taille (Seq l)) )
  | Seq s -> (match s with
         | [] -> 0
         | a:: l -> (taille a) + (taille (Seq l)) )
;;


(* Intepretation *)

let pi = 2.*.(asin 1.);;
let rad_of_deg d = (2. *. pi *. (float_of_int d) ) /. 360.;;

type coords = { x : float; y : float; z : float };;
type maxmin = { maxx : float; minx : float; maxy : float; miny : float };; (* or 2 coords *)


(* calcule nouveaux x, y et z *)
(* NB si phi = 0, il n'y aura pas de ligne, donc il faut avoir phi = 90 par defaut *)
let deplace point dist theta phi scale_factor =
  let scaled_dist = (float_of_int dist) /. scale_factor in
  let new_x = point.x +.(scaled_dist *. (cos (rad_of_deg theta)) )
  and new_y = point.y +.(scaled_dist *. (sin (rad_of_deg theta)) *. (sin (rad_of_deg phi)))
  and new_z = point.z +.(scaled_dist *. (cos (rad_of_deg phi)))
  in { x = new_x; y = new_y; z = new_z };;



(* retourne les noueaux x et y plus un maxmin altere si neccessaire *)
let deplace_compare point dist theta phi scale_factor maxmin_xy =
  let new_point = deplace point dist theta phi scale_factor
  in ( new_point, { minx = (min maxmin_xy.minx new_point.x);
                 maxx = (max maxmin_xy.maxx new_point.x);
                 miny = (min maxmin_xy.miny new_point.y);
                 maxy = (max maxmin_xy.maxy new_point.y) } );;

let calc_new_minmax point maxmin_xy =
  { minx = (min maxmin_xy.minx point.x);
    maxx = (max maxmin_xy.maxx point.x);
    miny = (min maxmin_xy.miny point.y);
    maxy = (max maxmin_xy.maxy point.y) } ;;

(* maybe make a new entity = viewing conditions, perspective?
   theta, phi, some_kind_of_zoom_factor related to minmax_xy *)
(* dessine une operation *)
let rec execute point theta phi minmax_xy scale_factor instructions check =
  match instructions with
  | [] -> (point,theta,phi,minmax_xy)
  | instruction::instructions' ->
    match instruction with
    | Line(dist) ->
      begin
        let new_point = deplace point dist theta phi scale_factor in
        if check then (* todo implement min max deplace_compare *)
         moveto (int_of_float new_point.x) (int_of_float new_point.y)
        else
         lineto (int_of_float new_point.x) (int_of_float new_point.y);
        let new_minmax_xy = if check then calc_new_minmax new_point minmax_xy else minmax_xy in
        execute new_point theta phi new_minmax_xy scale_factor instructions' check
      end
    | Move(dist) ->
      begin
        let new_point = deplace point dist theta phi scale_factor in
        moveto (int_of_float new_point.x) (int_of_float new_point.y);
        let new_minmax_xy = if check then calc_new_minmax new_point minmax_xy else minmax_xy in
        execute new_point theta phi new_minmax_xy scale_factor instructions' check
      end
    | Turn(deg) ->
      begin
        let newtheta = (theta + deg) mod 360 in
        execute point newtheta phi minmax_xy scale_factor instructions' check
      end
    | TurnPhi (deg) ->
      begin
	let newphi = 180 - abs (((phi + deg) mod 360) - 180) in
                      (* on veut: 0 < phi < 180 et phi = 181 -> phi = 179 *)
	execute point theta newphi minmax_xy scale_factor instructions' check
      end
    | Rectangle(largeur, hauteur) ->
      begin
        let (* scaled_hauteur = int_of_float ( (float_of_int hauteur) /. scale_factor)
        and *) scaled_largeur = int_of_float ( (float_of_int largeur) /. scale_factor)
        in
        if check then () else fill_rect (int_of_float point.x) (int_of_float point.y) scaled_largeur hauteur;
        let new_point = deplace point largeur theta phi scale_factor in
        moveto (int_of_float new_point.x) (int_of_float new_point.y);
        let new_minmax_xy = if check then calc_new_minmax new_point minmax_xy else minmax_xy in
        execute new_point theta phi new_minmax_xy scale_factor instructions' check
      end
    | MoveNoScale(dist) ->
      begin
        let new_point = deplace point dist theta phi 1.0 in
        moveto (int_of_float new_point.x) (int_of_float new_point.y);
        let new_minmax_xy = if check then calc_new_minmax new_point minmax_xy else minmax_xy in
        execute new_point theta phi new_minmax_xy scale_factor instructions' check
      end
;;

(* lis chaque symbole de la sequence et associe une liste d'instructions, passee a la fonction execute *)
let rec aux_draw chaine interpretation point minmax_xy theta phi scale_factor check =
  begin
    moveto (int_of_float point.x) (int_of_float point.y);
    match chaine with
    | Branch branch ->
      let (_, _, _, minmax_xy') =
        aux_draw (Seq branch) interpretation point minmax_xy theta phi scale_factor check
      in (point, theta, phi, minmax_xy')
    | Seq seq ->
      begin
        match seq with
        | [] -> (point, theta, phi, minmax_xy)
        | a::l -> (* make return value of x y from draw use in next call *)
         let (point', theta', phi' ,minmax_xy') = aux_draw a interpretation point minmax_xy theta phi scale_factor check in
         aux_draw (Seq l) interpretation point' minmax_xy' theta' phi' scale_factor check
      end
    | S symbol -> let (point',theta', phi', minmax_xy') = execute point theta phi minmax_xy scale_factor (List.assoc symbol interpretation) check in
                 (point',theta', phi', minmax_xy')
  end
;;

(* renvoie l'echelle le plus extreme d'agrandissement ou de reduction d'une partie du dessin apres sa substitution *)
(* NB maybe this should check using the input theta and phi ... *)
let rec check_echelle rws interpretation =
  let minmax_xy = { minx = 0.0 ; miny = 0.0 ; maxx = 0.0 ; maxy = 0.0 }
  in match rws with
  | [] -> 1.
  | (symbol, remplacement)::rws' ->         
    let (point_symbol, _, _, _) = aux_draw (S symbol) interpretation { x=0. ; y=0.; z=0.} minmax_xy !thetaInit !phiInit 1. true
    and (point_remplacement, _, _, _) = aux_draw remplacement interpretation { x=0. ; y=0.; z=0. } minmax_xy !thetaInit !phiInit 1. true
    in
    let delta_sym = sqrt((point_symbol.x**2.)+.(point_symbol.y**2.))
    and delta_rem = sqrt((point_remplacement.x**2.)+.(point_remplacement.y**2.))
    in let tmp = max (delta_rem/.delta_sym) (check_echelle rws' interpretation)
       in if delta_sym = 0.0 then 1. else tmp
;;


(* Calcule le scale factor s et le point de depart (x,y) pour que le nieme chaine soit centre et contenu dans la fenetre *)

let check_adjustment largeur_fenetre hauteur_fenetre chaine interpretation echelle =
  let minmax_xy = { minx = 0.0 ; miny = 0.0 ; maxx = 0.0 ; maxy = 0.0 }
  in
  let (_,_,_, new_minmax_xy) = aux_draw chaine interpretation { x = 0.0 ; y = 0.0; z = 0.0 } minmax_xy !thetaInit !phiInit echelle true in
  (* begin print_float new_minmax_xy.maxx; *)
  let s = 1.5 *. ( max (( new_minmax_xy.maxx -. new_minmax_xy.minx)/.largeur_fenetre)
                 (( new_minmax_xy.maxy -. new_minmax_xy.miny)/.hauteur_fenetre ) )
  in (s, { x = ((largeur_fenetre /. 2.) -. ( (1.0 /. s) *. ( (new_minmax_xy.maxx +. new_minmax_xy.minx)/. 2. )));
           y = ((hauteur_fenetre /. 2.) -. ( (1.0 /. s) *. ( (new_minmax_xy.maxy +. new_minmax_xy.miny)/. 2. )));
	   z = 0. (* z pas utilise ici *) } )
  (* end *)
;;


exception Quit;;

(* sauvegarde une image graphics de type Graphics.image dans le fichier au format de son choix (.png, .jpg...) *)
let save_image image file_name =
  let image = Images.Rgb24 (Graphic_image.image_of image) in
  Images.save file_name None [] image;;

(* transforme une matrice de triplets (r,g,b) en une "image graphics"
de type Graphics.image *)

let to_graphics rgb_matrix =
  Graphics.make_image
    (Array.map
       (Array.map
          (fun (r, g, b) -> Graphics.rgb r g b))
       rgb_matrix);;

let rec reponse_utilisateur k chaine rws interpretation point_depart echelle grow scale_factor iter clear = 
  try
    while true do
      let event = wait_next_event [Key_pressed]
      in 
      if event.keypressed
      then
	(match event.key with
	| 'q' -> raise Quit
	| 's' -> 
	  let img =  get_image 0 0 (size_x ()) (size_y ())
	  in  save_image img "pic.bmp"
	| _ -> begin
	  let (new_point_depart, zoom ) =
	  (match event.key with
	  | 'o' -> (set_color (rgb 246 121 25); (point_depart, 1.0) )
	  | 'r' -> (set_color red; (point_depart, 1.0) )
	  | 'v' -> (set_color green; (point_depart, 1.0) )
	  | 'j' -> (set_color yellow; (point_depart, 1.0) )
	  | 'b' -> (set_color black; (point_depart, 1.0) )
	  | '1' -> (thetaInit := !thetaInit + 10; (point_depart, 1.0) )
	  | '2' -> (thetaInit := !thetaInit - 10; (point_depart, 1.0) )
	  | 'a' -> (phiInit := !phiInit + 10 ; (point_depart, 1.0) )
	  | 'z' -> (phiInit := !phiInit + 10; (point_depart, 1.0) )
	  | '3' -> ({x=float_of_int(int_of_float (point_depart.x -. 10.) mod (size_x()));
		     y=point_depart.y; z=point_depart.z},1.0)
	  | '4' -> ({x=float_of_int(int_of_float (point_depart.x +. 10.) mod (size_x()));
		     y=point_depart.y; z=point_depart.z},1.0)
	  | '5' -> ({x=point_depart.x; y=float_of_int(int_of_float(point_depart.y -. 10.) mod (size_y()));
		     z=point_depart.z},1.0)
	  | '6' -> ({x=point_depart.x; y=float_of_int(int_of_float(point_depart.y +. 10.) mod (size_y()));
		     z=point_depart.z},1.0)
	  | '7' -> (point_depart,1.1)
	  | '8' -> (point_depart,0.9)
	  | _ -> (point_depart, 1.0)) in
	  
	  rec_draw k chaine rws interpretation new_point_depart echelle grow (scale_factor*.zoom) iter clear;
	  ();
	end)
      else ()
    done
  with Quit -> close_graph()
    
and
    
    rec_draw k chaine rws interpretation point_depart echelle grow scale_factor iter clear =
  let tmp = ( if grow then 1 else k ) in
  begin
    if clear then clear_graph() else ();
    let minmax_xy = { minx = 0.0 ; maxx = 0.0 ; miny = 0.0 ; maxy = 0.0 } in
    let (_,_,_,_)= aux_draw chaine interpretation point_depart minmax_xy !thetaInit !phiInit ( scale_factor *. (echelle ** (float_of_int tmp))) false in ();
    (* si il s'agit de la dernière itération *)
    if iter=(k+1) then begin
      reponse_utilisateur k chaine rws interpretation point_depart echelle grow scale_factor iter clear (* appeler la réponse à l'utilisateur *)
    end
    (* si ce n'est pas la dernière itération *)
    else
      begin
        let t = Unix.gettimeofday () +. 1.0 in
        while (Unix.gettimeofday () < t) do () done;
        rec_draw (k+1) (rewrite rws chaine) rws interpretation point_depart
         echelle grow scale_factor iter clear;
      end
end
;;


let draw chaine rws interpretation iter grow clear =
  begin
   open_graph(" 600x600");
    clear_graph();
    let echelle = check_echelle rws interpretation in
    let n_chaine = calc_n_chaine chaine rws iter in
      if (taille n_chaine) > (size_x() * size_y() ) then failwith "trop gros!!" else
        let (scale_factor, point_depart) =
         check_adjustment (float_of_int (size_x ())) (float_of_int (size_y ()))
         n_chaine interpretation (echelle ** (float_of_int (iter-1)))
        in
        begin
         print_float scale_factor;
         print_float point_depart.x;
         let echelle = (if grow then (echelle ** (float_of_int (iter-1))) else echelle) in
         rec_draw 0 chaine rws interpretation point_depart echelle grow scale_factor iter clear
        end
  end
;;


(* Les exemples *)

(* snow flake *)
let w_snow = Seq [S A;S B;S A;S B;S A]
and s_snow = [A, Seq [S A;S C;S A;S B;S A;S C;S A]]
and i_snow = [(A,[Line(250)]); (B,[Turn(120)]); (C,[Turn(-60)]); (D,[Turn(60)])];;

(* Le Dragon *)
let w_dragon = Seq [S A]
and s_dragon = [
  (A, Seq [S A; S C; S B; S C]);
  (B, Seq [S D; S A; S D; S B])
]
and i_dragon = [(A,[Line(50)]);(B,[Line(50)]);(C,[Turn(90)]);(D,[Turn(-90)])];;

(* Cantor *)
let w_cantor = Seq [S D; S A;]
and s_cantor = [
  (A, Seq [ S A; S B ; S A ] );
  (B, Seq [ S B; S B ; S B ] );
  (D, Seq [ S C; S D ]);
]
and i_cantor = [(A,[Rectangle(50,5)]);(B,[Move(50)]); (C,[Turn(-90);MoveNoScale(10);Turn(+90)]); (D,[])];;

(* quadratic koch island *)
let w_koch = Seq [S A;S B;S A;S B;S A;S B;S A]
and s_koch =
  [A,
   Seq ([S A;S C;S A;S A;S B;S A;S A;S B;S A;S B;S A;S C;S A;S C;S A] @
	   [S A;S B;S A;S B;S A;S C;S A;S C;S A;S A;S C;S A;S A;S B;S A])]
and i_koch =
  [(A,[Line(10)]); (B,[Turn(90)]); (C,[Turn(-90)]); (D,[Move(10)])];;

(* koch squares and lakes *)
let w_koch1 = Seq [S A;S C;S A;S C;S A;S C;S A]
and s_koch1 =
  [A,
   Seq ([S A;S C;S D;S B;S A;S A;S C;S A;S C;S A;S A;S C;S A;S D;S C;S A;S A] @
	   [S B;S D;S C;S A;S A;S B;S A;S B;S A;S A;S B;S A;S D;S B;S A;S A] @
	   [S A]);
   D,
   Seq [S D;S D;S D;S D;S D;S D]];;

(* koch variants *)
let s_koch2 =
  [A, Seq [S A;S A;S B;S A;S B;S A;S B;S A;S B;S A;S B;S A;S C;S A]]
and s_koch3 =
  [A, Seq [S A;S A;S B;S A;S B;S A;S B;S A;S B;S A;S A]]
and s_koch4 =
  [A,Seq [S A;S A;S B;S A;S C;S A;S B;S A;S B;S A;S A]]
and s_koch5 =
  [A,Seq [S A;S A;S B;S A;S B;S B;S A;S B;S A]];;

(* Sierpinski Gasket. ABOP page 11 *)
let w_sierp = Seq [S B]
and s_sierp = [
  (A, Seq [S B; S C; S A; S C; S B]);
  (B, Seq [S A; S D; S B; S D; S A])
]
and i_sierp = [(A,[Line(6)]);(B,[Line(6)]);(C,[Turn(60)]);(D,[Turn(-60)])];;

(* convertir des caractères en constantes du type bracketed *)
let tree_of_char = function
  | 'L' -> S L
  | 'R' -> S R
  | '+' -> S P
  | '-' -> S M
  | c -> failwith ("Unknown character: "^(String.make 1 c))
;;

(* convertir une chaîne sans parenthèses en un bracketed *)
let tree_of_string s =
  let rec tree_of_string_aux i =
    if i = String.length s
    then []
    else (tree_of_char (String.get s i))::(tree_of_string_aux (i+1))
  in Seq (tree_of_string_aux 0)
;;


(* Hexagonal Gosper curve. ABOP page 12 *)
let w_6gosp = S L
and s_6gosp = [
  (L, tree_of_string "L+R++R-L--LL-R+");
  (R, tree_of_string "-L+RR++R+L--L-R")
]
and i_6gosp =[(L,[Line(4)]);(R,[Line(4)]);(P,[Turn(60)]);(M,[Turn(-60)])];;

(* Quadratic Gosper curve. ABOP page 12 *)
let w_4gosp = tree_of_string "-R"
and s_4gosp = [
  (L, tree_of_string "LL-R-R+L+L-R-RL+R+LLR-L+R+LL+R-LR-R-L+L+RR-");
  (R, tree_of_string "+LL-R-R+L+LR+L-RR-L-R+LRR-L-RL+L+R-R-L+L+RR")
]
and i_4gosp = [(L,[Line(2)]);(R,[Line(2)]);(P,[Turn(90)]);(M,[Turn(-90)])];;

(* Branching 1. ABOP page 25 *)
let w_br1 = S F
and s_br1 = [(F, Seq [S F; Branch [S P; S F]; S F; Branch [S M; S F]; S F])]
and i_br1 = [(F,[Line(5)]);(P,[Turn(25)]);(M,[Turn(-25)])];;

(* Branching 2. ABOP page 25 *)
let w_br2 = S F
and s_br2 = [(F, Seq [S F; Branch [S P; S F]; S F; Branch [S M; S F]; Branch [S F]])]
and i_br2 = [(F,[Line(5)]);(P,[Turn(20)]);(M,[Turn(-20)])];;

(* Plant *)
let w_plant = S X
and s_plant = [
  (X, Seq [S F; S M; Branch [Branch [S X]; S P; S X]; S P; S F; Branch[S P; S F; S X]; S M; S X]);
  (F, Seq[S F; S F])
]
and i_plant = [(F,[Line(5)]);(P,[Turn(25)]);(M,[Turn(-25)]); (X, [])];; 

(* Plant 3D *)
let w_plant3d = S X
and s_plant3d = [
  (X, Seq [S F; S M; S N; Branch [Branch [S X]; S P; S Q; S X];
	   S P; S Q; S F; Branch[S P; S Q; S F; S X]; S M; S N; S X]);
  (F, Seq[S F; S F])
]
and i_plant3d = [(F,[Line(5)]);(P,[Turn(25)]);(M,[Turn(-25)]);
		 (Q,[TurnPhi(+25)]); (N,[TurnPhi(-25)]); (X, [])];; 

let exemples = [
  "snow",  (w_snow, s_snow, i_snow) ;
  "koch",  (w_koch, s_koch, i_koch) ;
  "koch1", (w_koch1, s_koch1, i_koch) ;
  "koch2", (w_koch, s_koch2, i_koch) ;
  "koch3", (w_koch, s_koch3, i_koch) ;
  "koch4", (w_koch, s_koch4, i_koch) ;
  "koch5", (w_koch, s_koch5, i_koch);
  "dragon", (w_dragon, s_dragon, i_dragon);
  "sierp", (w_sierp, s_sierp, i_sierp);
  "6gosp", (w_6gosp, s_6gosp, i_6gosp);
  "4gosp", (w_4gosp, s_4gosp, i_4gosp);
  "plant", (w_plant, s_plant, i_plant);
  "plant3d", (w_plant3d, s_plant3d, i_plant3d);
  "br1",   (w_br1,s_br1,i_br1);
  "br2",   (w_br2,s_br2,i_br2)
]
;;



try
  let (chaine, rws, commands) =  List.assoc !exemple exemples
  in draw chaine rws commands !n !grow !clear
with Not_found -> print_string ("l'exemple "^(!exemple)^" n'existe pas");;


(* draw chaine w_koch s_kock i_kock 1 true true;; *)

(* A FAIRE :

- centrage de Cantor
- utilisateur tape Q pour quit
- utilisateur tape S pour sauvegarde
- utilisateur choisit l'exemple

*)
