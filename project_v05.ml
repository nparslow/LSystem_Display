(*#directory "+camlimages";;
#use "topfind";;
#require "camlimages.all_formats";;
#require "camlimages.graphics";;
#load "graphics.cma"
#load "unix.cma";;*)

(* ocamlfind ocamlc -I ./lextest/ parser.cmo lexer.cmo -package camlimages.all_formats -package graphics -package camlimages.graphics unix.cma -linkpkg project_v05.ml  *)

open Parser;;
open Lexer;;

open Arg;;
open Images;;
open Graphics;;

(* Les valeurs pour les arguments par défaut *)
let n = ref 1;;
let grow = ref false;;
let clear = ref true;;
let exemple = ref "";;
let arguments = ref[];;
let e = ref false;;

let opt_spec = [
  ("-e", Unit (function () -> e := not !e), "Afficher la liste d'exemples possible");
  ("-n", Set_int n, "Le nombre d'itérations à faire. 1 correspond à la séquence initiale");
  ("-grow", Unit (function () -> grow := not !grow), "Agrandit avec chaque itération. \nPar défaut -grow n'est pas applicable");
  ("-no_clear", Unit (function () -> clear := not !clear), " Ne pas effacer le graphisme après chaque itération. \nPar défaut no_clear n'est pas applicable");
  ("-exemple", Set_string exemple, "L'exemple, ou le fichier d'un exemple. Voir option -e pour voir la liste d'exemples")
];;

let arg_action = (function s -> arguments:= !arguments @ [s]);;
let usage = "dessiner_lsystems.ml exemple [options] ...";;

parse opt_spec arg_action usage ;;

(* Afficher un message d'erreur si un exemple n'est pas spécifié *)
if !e then begin
  print_string "Exemples possible :\nsnow, koch1, koch2, koch3, koch4, koch5, dragon, sierp, 6gosp, 4gosp, plant, plant3d, br1, br2, cantor\n"; 
  exit 0
end
else if !exemple  = "" then begin
  prerr_endline "Erreur : Il faut spécifier un exemple à dessiner.\n
Vous avez le choix parmi :\n
snow, koch1, koch2, koch3, koch4, koch5, dragon, sierp, 6gosp, 4gosp, plant, plant3d, br1, br2, cantor\n";
  exit 0  
end
;;

(* initialiser les valeurs theta et phi *)
let thetaInit = ref 0;;
let phiInit = ref 90;;

(*
(* Définition de types *)
(* sans 'S' pour eviter un conflit dans le parser *)
type symbol =  A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | T | U | V | W | X | Y | Z;;

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
*)

type flux = NA | Flux of out_channel;; (* Pour la sauvegarde, Flux quand on veut sauvegarder, NA sinon *)


(* Substitutions *)
let rec rewrite rws brack =
  let rec in_rewrite rws brack = match brack with
    | S s -> (List.assoc s rws)
    | Seq seq -> Seq (List.map (rewrite rws) seq)
    | Branch b -> Branch (List.map (rewrite rws) b)
  in try in_rewrite rws brack
    with Not_found -> brack (* renvoyer le bracketed original s'il n'y a pas de remplacement *)
;;

(* Calculer la chaine après n iterations *)
let rec calc_n_chaine chaine rws n =
  if n <= 1 then chaine
  else rewrite rws (calc_n_chaine chaine rws (n-1));;

(* Calculer la taille d'une chaîne, c-à-d le nombre total d'operations dans une sequence *)
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
type maxmin = { maxx : float; minx : float; maxy : float; miny : float };;


(* Calculer nouveaux x, y et z
   N.B. si phi = 0, il n'y aura pas de ligne, donc il faut avoir phi = 90 par défaut *)
let deplace point dist theta phi scale_factor =
  let scaled_dist = (float_of_int dist) /. scale_factor in
  let new_x = point.x +.(scaled_dist *. (cos (rad_of_deg theta)) )
  and new_y = point.y +.(scaled_dist *. (sin (rad_of_deg theta)) *. (sin (rad_of_deg phi)))
  and new_z = point.z +.(scaled_dist *. (cos (rad_of_deg phi)))
  in { x = new_x; y = new_y; z = new_z };;

(* Retourner les nouveaux x et y plus un maxmin modifié si nécessaire *)
let deplace_compare point dist theta phi scale_factor maxmin_xy =
  let new_point = deplace point dist theta phi scale_factor
  in ( new_point, { minx = (min maxmin_xy.minx new_point.x);
                    maxx = (max maxmin_xy.maxx new_point.x);
                    miny = (min maxmin_xy.miny new_point.y);
                    maxy = (max maxmin_xy.maxy new_point.y) } );;

(* Calculer les nouvelles valeurs maxmin en prenant la valeur la plus extrême entre le point actuel et la valeur minmax stockée*)
let calc_new_minmax point maxmin_xy =
  { minx = (min maxmin_xy.minx point.x);
    maxx = (max maxmin_xy.maxx point.x);
    miny = (min maxmin_xy.miny point.y);
    maxy = (max maxmin_xy.maxy point.y) } ;;

(* Ecrire dans le fichier .svg la balise associée à la commande *)
let sauvegarde save command point_xy new_xy current_color= 
  let (red, green, blue) = current_color in
  match save with
  | NA -> () (* ne rien faire s'il n'y a pas de canal ouvert *)
  | Flux(canal) -> match command with
    | Line(_) -> output_string canal (String.concat "" ["<line x1=\""; string_of_float (point_xy.x); "\" y1=\""; 
							(string_of_float (point_xy.y)); "\" x2=\"";
							(string_of_float new_xy.x);"\" y2=\"";
							(string_of_float new_xy.y);
							"\" style=\"stroke:rgb("; 
							string_of_int red; ","; string_of_int green;","; string_of_int blue; 
							");stroke-width:0.5\" />\n"])
      
    | Rectangle(largeur,hauteur) -> output_string canal (String.concat "" ["<rect width=\"";
									   (string_of_int largeur);
									   "\" height=\""; (string_of_int hauteur);
									   "\" style=\"fill:rgb(";
									   string_of_int red; ","; string_of_int green;","; string_of_int blue; 
									   ");stroke-width:0.5;stroke:rgb(255,0,0)\">\n"])
      
    | Move(_) -> output_string canal (String.concat "" [" <path d=\"M" ; string_of_float new_xy.x; string_of_float new_xy.y; "\"/>\n"])
    | _ -> ()
;;

(* maybe make a new entity = viewing conditions, perspective?
theta, phi, some_kind_of_zoom_factor related to minmax_xy *)
(* Exécuter une commande, c-à-d dessiner une opération *)
let rec execute point theta phi minmax_xy scale_factor instructions check save current_color=
  match instructions with
  | [] -> (point,theta,phi,minmax_xy)
  | instruction::instructions' -> match instruction with
    | Line(dist) ->
      begin
        let new_point = deplace point dist theta phi scale_factor in
	sauvegarde save (Line(dist))point new_point current_color; (* Ecrire dans le fichier .svg *)
	
        (* Si on veut vérifier la taille de l'image pour calculer l'échelle, ne rien dessiner *)
        if check then (* todo implement min max deplace_compare *)
          moveto (int_of_float new_point.x) (int_of_float new_point.y)
        else
          lineto (int_of_float new_point.x) (int_of_float new_point.y);
	
	(* Calculer les nouvelles valeurs pour les min et max x et y *)
        let new_minmax_xy = if check then calc_new_minmax new_point minmax_xy else minmax_xy in
        execute new_point theta phi new_minmax_xy scale_factor instructions' check save current_color
      end
	
    | Move(dist) ->
      begin
        let new_point = deplace point dist theta phi scale_factor in
	sauvegarde save (Move(dist)) point new_point current_color; (* Ecrire dans le fichier .svg *)
        moveto (int_of_float new_point.x) (int_of_float new_point.y);
	
	(* Calculer les nouvelles valeurs pour les min et max x et y *)
        let new_minmax_xy = if check then calc_new_minmax new_point minmax_xy else minmax_xy in
        execute new_point theta phi new_minmax_xy scale_factor instructions' check save current_color;
      end

    | Turn(deg) ->
      (* Calculer le nouvel angle theta et exécuter la commande *)
      let newtheta = (theta + deg) mod 360 in
      execute point newtheta phi minmax_xy scale_factor instructions' check save current_color

    | TurnPhi (deg) ->
      (* Calculer le nouvel angle phi et exécuter la commande *)
      let newphi = (phi + deg) mod 360 in (* 180 - abs (((phi + deg) mod 360) - 180) in
      (* on veut: 0 < phi < 180 et phi = 181 -> phi = 179 *) *)
      execute point theta newphi minmax_xy scale_factor instructions' check save current_color
	
    | Rectangle(largeur, hauteur) ->
      begin
        let (* scaled_hauteur = int_of_float ( (float_of_int hauteur) /. scale_factor)
	       and *) scaled_largeur = int_of_float ( (float_of_int largeur) /. scale_factor) in
	
	(* Si on veut vérifier la taille de l'image pour calculer l'échelle, ne rien dessiner *)
        if check then () else fill_rect (int_of_float point.x) (int_of_float point.y) scaled_largeur hauteur;
	
	(* Calculer le nouveau point et se déplacer jusqu'à ce point *)
        let new_point = deplace point largeur theta phi scale_factor in
        moveto (int_of_float new_point.x) (int_of_float new_point.y);
	
	(* Ecrire dans le fichier .svg *)
	sauvegarde save (Rectangle(largeur, hauteur)) point new_point current_color; 
	
	(* Calculer le nouveau minmax et exécuter la commande *)
        let new_minmax_xy = if check then calc_new_minmax new_point minmax_xy else minmax_xy in
        execute new_point theta phi new_minmax_xy scale_factor instructions' check save current_color;
      end
	
    | MoveNoScale(dist) ->
      begin
	(* Calculer le nouveau point et se déplacer jusqu'à ce point *)
        let new_point = deplace point dist theta phi 1.0 in
        moveto (int_of_float new_point.x) (int_of_float new_point.y);
	
	(* Ecrire dans le fichier .svg *)
	sauvegarde save (Move(dist)) point new_point current_color; 

	(* Calculer le nouveau minmax et exécuter la commande *)
        let new_minmax_xy = if check then calc_new_minmax new_point minmax_xy else minmax_xy in
        execute new_point theta phi new_minmax_xy scale_factor instructions' check save current_color;
      end
;;

(* Lire chaque symbole de la séquence et trouver la liste d'instructions associée au symbole, qui est ensuite passée à la fonction execute *)
let rec aux_draw chaine interpretation point minmax_xy theta phi scale_factor check save current_color =
  begin
    moveto (int_of_float point.x) (int_of_float point.y); (* se déplacer a priori *)
    
    match chaine with
    | Branch branch ->
      (* Conserver la valeur du point, theta, phi pour la suite *)
      let (_, _, _, minmax_xy') =
        aux_draw (Seq branch) interpretation point minmax_xy theta phi scale_factor check save current_color
      in (point, theta, phi, minmax_xy')
      
    | Seq seq ->
      begin
	match seq with
	| [] -> (point, theta, phi, minmax_xy)
	| a::l -> (* Utiliser de nouvelles valeurs pour la suite *)
          let (point', theta', phi' ,minmax_xy') = 
	    aux_draw a interpretation point minmax_xy theta phi scale_factor check save current_color in
          aux_draw (Seq l) interpretation point' minmax_xy' theta' phi' scale_factor check save current_color
      end
	
    | S symbol -> let (point',theta', phi', minmax_xy') = 
		    execute point theta phi minmax_xy scale_factor (List.assoc symbol interpretation) check save current_color in
                  (point',theta', phi', minmax_xy')
  end
;;

(* Renvoyer l'échelle d'agrandissement ou de réduction la plus extrême parmi chaque symbole et son replacement *)
(* NB maybe this should check using the input theta and phi ... *)
let rec check_echelle rws interpretation save current_color =
  let minmax_xy = { minx = 0.0 ; miny = 0.0 ; maxx = 0.0 ; maxy = 0.0 }
  in match rws with
  | [] -> 1.
  | (symbol, remplacement)::rws' ->
    let (point_symbol, _, _, _) = aux_draw (S symbol) interpretation { x=0. ; y=0.; z=0.} minmax_xy !thetaInit !phiInit 1. true save current_color
    and (point_remplacement, _, _, _) = aux_draw remplacement interpretation { x=0. ; y=0.; z=0. } minmax_xy !thetaInit !phiInit 1. true save current_color
    in
    let delta_sym = sqrt((point_symbol.x**2.)+.(point_symbol.y**2.))
    and delta_rem = sqrt((point_remplacement.x**2.)+.(point_remplacement.y**2.))
    in let tmp = max (delta_rem/.delta_sym) (check_echelle rws' interpretation save current_color)
       in if delta_sym = 0.0 then 1. else tmp
;;


(* Calculer le scale_factor s et le point de depart (x,y) pour que la nième chaine soit centrée et contenue dans la fenêtre *)
let check_adjustment largeur_fenetre hauteur_fenetre chaine interpretation echelle save current_color=
  let minmax_xy = { minx = 0.0 ; miny = 0.0 ; maxx = 0.0 ; maxy = 0.0 }
  in
  let (_,_,_, new_minmax_xy) = aux_draw chaine interpretation { x = 0.0 ; y = 0.0; z = 0.0 } minmax_xy !thetaInit !phiInit echelle true save current_color in
  (* begin print_float new_minmax_xy.maxx; *)
  let s = 1.5 *. ( max (( new_minmax_xy.maxx -. new_minmax_xy.minx)/.largeur_fenetre)
                     (( new_minmax_xy.maxy -. new_minmax_xy.miny)/.hauteur_fenetre ) )
  in (s, { x = ((largeur_fenetre /. 2.) -. ( (1.0 /. s) *. ( (new_minmax_xy.maxx +. new_minmax_xy.minx)/. 2. )));
           y = ((hauteur_fenetre /. 2.) -. ( (1.0 /. s) *. ( (new_minmax_xy.maxy +. new_minmax_xy.miny)/. 2. )));
           z = 0. (* z pas utilisé ici *) } )
;;


exception Quit;;

(* Sauvegarder une image graphics de type Graphics.image dans un fichier *)
let save_image image file_name =
  let image = Images.Rgb24 (Graphic_image.image_of image) in
  Images.save file_name None [] image;;

(* Commencer un fichier svg - entêtes du fichier *)
let start_svg_file c= 
  output_string c "<!DOCTYPE html>
<html>
<body>
<svg height=\"600\" width=\"600\">";; 

(* Finir un fichier svg - balises fermantes *)
let finish_svg_file c =
  begin
    output_string c "</svg>
</body>
</html>";
    close_out c
  end;;

(* Permettre à l'utilisateur de transformer le graphisme final, de sauvegarder et de quitter *)
let rec reponse_utilisateur k chaine rws interpretation point_depart echelle grow scale_factor zoom iter clear current_color =
  try
    while true do
      let event = wait_next_event [Key_pressed]
      in
      if event.keypressed
      then
        (match event.key with
        | 'q' -> raise Quit
        | 's' ->
	  (* Sauvegarder l'image actuelle *)
	  let c = open_out ("LSystem_sauvegarde"^(string_of_float( Unix.time()))^"svg") in 
          begin 
	    start_svg_file c;
            rec_draw k chaine rws interpretation point_depart echelle grow (scale_factor*.zoom) iter clear (Flux(c)) current_color;
	    finish_svg_file c;
          end
        | _ -> begin
          let (new_point_depart, zoom, current_color) =
            (match event.key with
	    (* Modifier la couleur du graphisme *)
            | 'o' -> let (r,g,b) = (246,121,25) in (set_color (rgb r g b); (point_depart, 1.0, (r,g,b)))
            | 'r' -> let (r,g,b) = (255,0,0) in (set_color (rgb r g b); (point_depart, 1.0, (r,g,b)))
            | 'v' -> let (r,g,b) = (0,153,0) in (set_color (rgb r g b); (point_depart, 1.0, (r,g,b)))
            | 'j' -> let (r,g,b) = (255, 255, 0) in (set_color (rgb r g b); (point_depart, 1.0, (r,g,b)))
            | 'b' -> let (r,g,b) = (255, 255, 255) in (set_color (rgb r g b); (point_depart, 1.0, (r,g,b)))
	    (* Modifier l'angle theta (2d) du graphisme - 1 = anticlockwise, 2 = clockwise *)
            | '1' -> (thetaInit := !thetaInit + 10; (point_depart, 1.0, current_color))
            | '2' -> (thetaInit := !thetaInit - 10; (point_depart, 1.0, current_color))
	    (* Modifier l'angle phi (3d) *)
            | 'a' -> (phiInit := !phiInit - 10 ; (point_depart, 1.0, current_color))
            | 'z' -> (phiInit := !phiInit + 10 ; (point_depart, 1.0, current_color))
	    (* Déplacer le graphisme vers la gauche (3) et vers la droite (4) *)
            | '3' -> ({x=float_of_int(int_of_float (point_depart.x -. 10.) mod (size_x()));
                       y=point_depart.y; z=point_depart.z},1.0, current_color)
            | '4' -> ({x=float_of_int(int_of_float (point_depart.x +. 10.) mod (size_x()));
                       y=point_depart.y; z=point_depart.z},1.0, current_color)
	    (* Déplacer le graphisme vers le bas (6) et vers le haut (5) *)
            | '5' -> ({x=point_depart.x; y=float_of_int(int_of_float(point_depart.y -. 10.) mod (size_y()));
                       z=point_depart.z}, 1.0, current_color)
            | '6' -> ({x=point_depart.x; y=float_of_int(int_of_float(point_depart.y +. 10.) mod (size_y()));
                       z=point_depart.z}, 1.0, current_color)
	    (* Modifier le zoom : zoom in (7) et zoom out (8) *)
            | '7' -> (point_depart, 1.1, current_color)
            | '8' -> (point_depart, 0.9, current_color)
            | _ -> (point_depart, 1.0, current_color)) in
          begin
            rec_draw k chaine rws interpretation new_point_depart echelle grow (scale_factor*.zoom) iter clear NA current_color;
            ();
	  end
        end)
      else ()
    done
  with Quit -> close_graph()
    
and
    (* Faire un appel à aux_draw pour chaque itération de 1 jusqu'à n *)
    rec_draw k chaine rws interpretation point_depart echelle grow scale_factor iter clear save current_color =
  let tmp = ( if grow then 1 else k ) in
  begin
    if clear then clear_graph() else ();
    let minmax_xy = { minx = 0.0 ; maxx = 0.0 ; miny = 0.0 ; maxy = 0.0 } in
    let (_,_,_,_)= aux_draw chaine interpretation point_depart minmax_xy !thetaInit !phiInit ( scale_factor *. (echelle ** (float_of_int tmp))) false save current_color in ();

    (* s'il s'agit de la dernière itération *)
    if iter=(k+1) then
      (* appeler la réponse à l'utilisateur *)
      reponse_utilisateur k chaine rws interpretation point_depart echelle grow scale_factor 1. iter clear current_color
    
    (* si ce n'est pas la dernière itération *)
    else
      begin
	(* petite pause *)
        let t = Unix.gettimeofday () +. 1.0 in
        while (Unix.gettimeofday () < t) do () done;
        (* prochaine itération *)
	rec_draw (k+1) (rewrite rws chaine) rws interpretation point_depart
          echelle grow scale_factor iter clear save current_color;
      end
  end
;;


(* La fonction principale qui démarre l'animation *)
let draw chaine rws interpretation iter grow clear =
  (* Couleur initiale est noir *)
  let current_color = (0,0,0) in
  begin
    open_graph(" 600x600");
    clear_graph();
    
    (* Calculer l'échelle du graphisme *)
    let echelle = check_echelle rws interpretation NA current_color in
    
    (* Calculer la nième chaîne pour juger s'il y a trop d'opérations *)
    let n_chaine = calc_n_chaine chaine rws iter in
    if (taille n_chaine) > (size_x() * size_y() ) then failwith "Vous demandez trop d'opérations. Diminuez la valeur de n\n" else
      
      (* Calculer le scale_factor entre chaque itération et le point de départ de l'image *)
      let (scale_factor, point_depart) =
        check_adjustment (float_of_int (size_x ())) (float_of_int (size_y ()))
          n_chaine interpretation (echelle ** (float_of_int (iter-1))) NA current_color
      in
        let echelle = (if grow then (echelle ** (float_of_int (iter-1))) else echelle) in
	(* Commencer l'itération *)
        rec_draw 0 chaine rws interpretation point_depart echelle grow scale_factor iter clear NA current_color
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

(* Sierpinski Gasket. *)
let w_sierp = Seq [S B]
and s_sierp = [
  (A, Seq [S B; S C; S A; S C; S B]);
  (B, Seq [S A; S D; S B; S D; S A])
]
and i_sierp = [(A,[Line(6)]);(B,[Line(6)]);(C,[Turn(60)]);(D,[Turn(-60)])];;

(* Convertir des caractères en constantes du type bracketed *)
let tree_of_char = function
  | 'L' -> S L
  | 'R' -> S R
  | '+' -> S P
  | '-' -> S M
  | c -> failwith ("Unknown character: "^(String.make 1 c))
;;

(* Convertir une chaîne sans parenthèses en un bracketed *)
let tree_of_string s =
  let rec tree_of_string_aux i =
    if i = String.length s
    then []
    else (tree_of_char (String.get s i))::(tree_of_string_aux (i+1))
  in Seq (tree_of_string_aux 0)
;;

(* Hexagonal Gosper curve *)
let w_6gosp = S L
and s_6gosp = [
  (L, tree_of_string "L+R++R-L--LL-R+");
  (R, tree_of_string "-L+RR++R+L--L-R")
]
and i_6gosp =[(L,[Line(4)]);(R,[Line(4)]);(P,[Turn(60)]);(M,[Turn(-60)])];;

(* Quadratic Gosper curve *)
let w_4gosp = tree_of_string "-R"
and s_4gosp = [
  (L, tree_of_string "LL-R-R+L+L-R-RL+R+LLR-L+R+LL+R-LR-R-L+L+RR-");
  (R, tree_of_string "+LL-R-R+L+LR+L-RR-L-R+LRR-L-RL+L+R-R-L+L+RR")
]
and i_4gosp = [(L,[Line(2)]);(R,[Line(2)]);(P,[Turn(90)]);(M,[Turn(-90)])];;

(* Branching 1 *)
let w_br1 = S F
and s_br1 = [(F, Seq [S F; Branch [S P; S F]; S F; Branch [S M; S F]; S F])]
and i_br1 = [(F,[Line(5)]);(P,[Turn(25)]);(M,[Turn(-25)])];;

(* Branching 2 *)
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
  "snow", (w_snow, s_snow, i_snow) ;
  "koch", (w_koch, s_koch, i_koch) ;
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
  "br1", (w_br1,s_br1,i_br1);
  "br2", (w_br2,s_br2,i_br2);
  "cantor", (w_cantor, s_cantor, i_cantor);
]
;;


(* Interpréter l'argument et options donnés en ligne de commande *)
try
  let (chaine, rws, commands) = List.assoc !exemple exemples
  in draw chaine rws commands !n !grow !clear
with Not_found -> 
  try
    (* lire des informations a partir d'un fichier *)
    begin
      print_string "Malheureusement le parser ne fonctionne pas encore,\n";
      print_string "S'il vous plait utiliser un des exemples \n";
      let lexbuf = Lexing.from_channel (open_in !exemple) 
      in let (chaine, rws, commands) = Parser.main Lexer.token lexbuf
	 in draw chaine rws commands !n !grow !clear
    end
  with e -> print_string ("l'exemple "^(!exemple)^" n'existe pas \n");;


(* A FAIRE :

- centrage de Cantor
- réglage de l'erreur à la fin on quit

*)




