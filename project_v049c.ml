#directory "+camlimages";;
#use "topfind";;
#require "camlimages.all_formats";;
#require "camlimages.graphics";;

open Arg;;

let n = ref 0;;
let grow = ref false;;
let arguments = ref[];;

let opt_spec = [
  ("-n", Set_int n, "Il faut préciser un nombre d'itérations n");
  ("-grow", Unit (function () -> grow := not !grow), "Faire agrandir le graphisme");
];;

let arg_action = (function s -> arguments:= !arguments @ [s]);;
let usage = "how to use";;

parse opt_spec arg_action usage;;

(* detecter des problemes *)
(*if !n = 0 then begin
prerr_endline "Erreur : pas de n donne";
exit 0

end;;*)

type symbol = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z;;
(*type symbol = char;;*)

type bracketed = S of symbol
                 | Seq of bracketed list
                 | Branch of bracketed list;;

type turtle_command= Move of int
                 | Line of int
                 | Turn of int
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
#load "graphics.cma"
open Graphics;;

#load "unix.cma";;

let pi = 2.*.(asin 1.);;
let rad_of_deg d = (2. *. pi *. (float_of_int d) ) /. 360.;;

type coords = { x : float; y : float };;
type maxmin = { maxx : float; minx : float; maxy : float; miny : float };; (* or 2 coords *)


(* calcule nouveaux x et y *)
let deplace point dist theta scale_factor =
  let scaled_dist = (float_of_int dist) /. scale_factor in
  let new_x = point.x +.(scaled_dist *. (cos (rad_of_deg theta)))
  and new_y = point.y +.(scaled_dist *. (sin (rad_of_deg theta)))
  in { x = new_x; y = new_y };;



(* retourne les noueaux x et y plus un maxmin altere si neccessaire *)
let deplace_compare point dist theta scale_factor maxmin_xy =
  let new_point = deplace point dist theta scale_factor
  in ( new_point, { minx = (min maxmin_xy.minx new_point.x);
                 maxx = (max maxmin_xy.maxx new_point.x);
                 miny = (min maxmin_xy.miny new_point.y);
                 maxy = (max maxmin_xy.maxy new_point.y) } );;

let calc_new_minmax point maxmin_xy =
  { minx = (min maxmin_xy.minx point.x);
    maxx = (max maxmin_xy.maxx point.x);
    miny = (min maxmin_xy.miny point.y);
    maxy = (max maxmin_xy.maxy point.y) } ;;

(* dessine une operation *)
let rec execute point theta minmax_xy scale_factor instructions check =
  match instructions with
  | [] -> (point,theta,minmax_xy)
  | instruction::instructions' ->
    match instruction with
    | Line(dist) ->
      begin
        let new_point = deplace point dist theta scale_factor in
        if check then (* todo implement min max deplace_compare *)
         moveto (int_of_float new_point.x) (int_of_float new_point.y)
        else
         lineto (int_of_float new_point.x) (int_of_float new_point.y);
        let new_minmax_xy = if check then calc_new_minmax new_point minmax_xy else minmax_xy in
        execute new_point theta new_minmax_xy scale_factor instructions' check
      end
    | Move(dist) ->
      begin
        let new_point = deplace point dist theta scale_factor in
        moveto (int_of_float new_point.x) (int_of_float new_point.y);
        let new_minmax_xy = if check then calc_new_minmax new_point minmax_xy else minmax_xy in
        execute new_point theta new_minmax_xy scale_factor instructions' check
      end
    | Turn(deg) ->
      begin
        let newtheta = (theta + deg) mod 360 in
        execute point newtheta minmax_xy scale_factor instructions' check
      end
    | Rectangle(largeur, hauteur) ->
      begin
        let (* scaled_hauteur = int_of_float ( (float_of_int hauteur) /. scale_factor)
        and *) scaled_largeur = int_of_float ( (float_of_int largeur) /. scale_factor)
        in
        if check then () else fill_rect (int_of_float point.x) (int_of_float point.y) scaled_largeur hauteur;
        let new_point = deplace point largeur theta scale_factor in
        moveto (int_of_float new_point.x) (int_of_float new_point.y);
        let new_minmax_xy = if check then calc_new_minmax new_point minmax_xy else minmax_xy in
        execute new_point theta new_minmax_xy scale_factor instructions' check
      end
    | MoveNoScale(dist) ->
      begin
        let new_point = deplace point dist theta 1.0 in
        moveto (int_of_float new_point.x) (int_of_float new_point.y);
        let new_minmax_xy = if check then calc_new_minmax new_point minmax_xy else minmax_xy in
        execute new_point theta new_minmax_xy scale_factor instructions' check
      end
;;

(* lis chaque symbole de la sequence et associe une liste d'instructions, passee a la fonction execute *)
let rec aux_draw chaine interpretation point minmax_xy theta scale_factor check =
  begin
    moveto (int_of_float point.x) (int_of_float point.y);
    match chaine with
    | Branch branch ->
      let (_, _, minmax_xy') =
        aux_draw (Seq branch) interpretation point minmax_xy theta scale_factor check
      in (point,theta, minmax_xy')
    | Seq seq ->
      begin
        match seq with
        | [] -> (point, theta, minmax_xy)
        | a::l -> (* make return value of x y from draw use in next call *)
         let (point', theta', minmax_xy') = aux_draw a interpretation point minmax_xy theta scale_factor check in
         aux_draw (Seq l) interpretation point' minmax_xy' theta' scale_factor check
      end
    | S symbol -> let (point',theta', minmax_xy') = execute point theta minmax_xy scale_factor (List.assoc symbol interpretation) check in
                 (point',theta', minmax_xy')
  end
;;

(* renvoie l'echelle le plus extreme d'agrandissement ou de reduction d'une partie du dessin apres sa substitution *)
let rec check_echelle rws interpretation =
  let minmax_xy = { minx = 0.0 ; miny = 0.0 ; maxx = 0.0 ; maxy = 0.0 }
  in match rws with
  | [] -> 1.
  | (symbol, remplacement)::rws' ->         
    let (point_symbol, _, _) = aux_draw (S symbol) interpretation { x=0. ; y=0.} minmax_xy 0 1. true
    and (point_remplacement, _, _) = aux_draw remplacement interpretation { x=0. ; y=0. } minmax_xy 0 1. true
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
  let (_,_, new_minmax_xy) = aux_draw chaine interpretation { x = 0.0 ; y = 0.0 } minmax_xy 0 echelle true in
  (* begin print_float new_minmax_xy.maxx; *)
  let s = 1.5 *. ( max (( new_minmax_xy.maxx -. new_minmax_xy.minx)/.largeur_fenetre)
                 (( new_minmax_xy.maxy -. new_minmax_xy.miny)/.hauteur_fenetre ) )
  in (s, { x = ((largeur_fenetre /. 2.) -. ( (1.0 /. s) *. ( (new_minmax_xy.maxx +. new_minmax_xy.minx)/. 2. )));
           y = ((hauteur_fenetre /. 2.) -. ( (1.0 /. s) *. ( (new_minmax_xy.maxy +. new_minmax_xy.miny)/. 2. ))) } )
  (* end *)
;;


exception Quit;;

(* sauvegarde une image graphics de type Graphics.image dans le fichier au format de son choix (.png, .jpg...) *)
let save_image image file_name =
  let img = Images.Rgb24 (Graphic_image.image_of image) in
  Images.save file_name None [] img;;

(* transforme une matrice de triplets (r,g,b) en une "image graphics"
de type Graphics.image *)

let to_graphics rgb_matrix =
  Graphics.make_image
    (Array.map
       (Array.map
          (fun (r, g, b) -> Graphics.rgb r g b))
       rgb_matrix);;

let rec reponse_utilisateur k chaine rws interpretation point_depart echelle grow scale_factor iter clear=
  let event = wait_next_event [Key_pressed]
  in 
  try
    while true do
      if event.keypressed
      then
	match event.key with
	| 'q' -> raise Quit
	| 's' -> 
	  let img =  get_image 0 0 (size_x ()) (size_y ())
	  in save_image img "picture.bmp"
	| _ -> begin
	  (match event.key with
	    | 'o' ->  set_color (rgb 246 121 25)
	    | 'r' ->  set_color red
	    | 'v' ->  set_color green
	    | 'j' -> set_color yellow);
	       
	  rec_draw k chaine rws interpretation point_depart echelle grow scale_factor iter clear;
	  ();
	end
    done
  with Quit -> close_graph();

and

rec_draw k chaine rws interpretation point_depart echelle grow scale_factor iter clear =
  let tmp = ( if grow then 1 else k ) in
  begin
    if clear then clear_graph() else ();
    let minmax_xy = { minx = 0.0 ; maxx = 0.0 ; miny = 0.0 ; maxy = 0.0 } in
    aux_draw chaine interpretation point_depart minmax_xy 0 ( scale_factor *. (echelle ** (float_of_int tmp))) false;
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


(* snow flake *)
let w_snow = Seq [S A;S B;S A;S B;S A]
and s_snow = [A, Seq [S A;S C;S A;S B;S A;S C;S A]]
and i_snow =
  [(A,[Line(250)]); (B,[Turn(120)]); (C,[Turn(-60)]); (D,[Turn(60)])];;

draw w_snow s_snow i_snow 5 false true;;

(* The infamous Dragon curve, the terror of IF1. ABOP page 11 *)
let w_dragon = Seq [S A]
and s_dragon = [
  (A, Seq [S A; S C; S B; S C]);
  (B, Seq [S D; S A; S D; S B])
]
and i_dragon = [(A,[Line(50)]);(B,[Line(50)]);(C,[Turn(90)]);(D,[Turn(-90)])];;

(*draw w_dragon s_dragon i_dragon 15 true true;;*)


let w_cantor = Seq [S D; S A;]
and s_cantor = [
  (A, Seq [ S A; S B ; S A ] );
  (B, Seq [ S B; S B ; S B ] );
  (D, Seq [ S C; S D ]);
]
and i_cantor = [(A,[Rectangle(50,5)]);(B,[Move(50)]); (C,[Turn(-90);MoveNoScale(10);Turn(+90)]); (D,[])];;

(*draw w_cantor s_cantor i_cantor 5 false false;;*)





(*Images.save "dessin.bla" None [] img;;*)



(* A FAIRE :

- centrage de Cantor
- utilisateur tape Q pour quit
- utilisateur tape S pour sauvegarde
- utilisateur choisit l'exemple

*)
