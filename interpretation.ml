open Defs;;
open Substitutions;;
open Graphics;;


let pi = 2.*.(asin 1.);;
let rad_of_deg d = (2. *. pi *. (float_of_int d) ) /. 360.;;


(* initialiser les valeurs theta et phi *)
let thetaInit = ref 0;;
let phiInit = ref 90;;

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
    | Line(_) -> output_string canal (String.concat "" ["<line x1=\""; (string_of_float point_xy.x); "\" y1=\"";
                                                        string_of_float ((float_of_int (size_y())) -. point_xy.y); "\" x2=\"";
                                                        string_of_float new_xy.x;"\" y2=\"";
                                                        string_of_float ((float_of_int (size_y())) -. new_xy.y);
                                                        "\" style=\"stroke:rgb(";
                                                        string_of_int red; ","; string_of_int green;","; string_of_int blue;
                                                        ");stroke-width:1\" />\n"])
      
    | Rectangle(largeur,hauteur) -> output_string canal (String.concat "" ["<rect width=\"";
                                                                         (string_of_int largeur);
                                                                         "\" height=\""; (string_of_int hauteur);
                                                                         "\" style=\"fill:rgb(";
                                                                         string_of_int red; ","; string_of_int green;","; string_of_int blue;
                                                                         ");stroke-width:1;stroke:rgb(255,0,0)\"/>\n"])
      
    | Move(_) -> output_string canal (String.concat "" [" <path d=\"M" ; string_of_float  new_xy.x;  string_of_float ((float_of_int (size_y())) -. new_xy.y); "\"/>\n"])
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
      let newphi = 180 - abs (((phi + deg) mod 360) - 180) in
      (* on veut: 0 < phi < 180 et phi = 181 -> phi = 179 *)
      execute point theta newphi minmax_xy scale_factor instructions' check save current_color
        
    | Rectangle(largeur, hauteur) ->
      begin
        let (* scaled_hauteur = int_of_float ( (float_of_int hauteur) /. scale_factor)
         and *) scaled_largeur = int_of_float ( (float_of_int largeur) /. scale_factor) in
        
        (* Si on veut vérifier la taille de l'image pour calculer l'échelle, ne rien dessiner *)
        if check then () else fill_rect (int_of_float point.x) (int_of_float point.y) scaled_largeur hauteur;
 
	(* Calculer le nouveau point et se déplacer jusqu'à ce point *)
        let new_point = deplace point largeur theta phi scale_factor in
        moveto (int_of_float new_point.x) (int_of_float new_point.y) ;

        
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
let rec reponse_utilisateur k chaine_initiale chaine rws interpretation point_depart echelle grow scale_factor zoom iter clear current_color =
 
  try
      let event = wait_next_event [Key_pressed]
      in
      if event.keypressed then
        match event.key with
        | 'q' -> raise Quit
        | 's' ->
          (* Sauvegarder l'image actuelle *)
	  let time = string_of_float (Unix.time()) in
          let c = open_out ("LSystem_sauvegarde"^time^"svg") in
          begin
	    start_svg_file c;(* Ouvrir les balises *)
	    clear_graph();

	    (* Redessiner juste la dernière chaine si le graphisme est effacé à chaque fois
	    Sinon redessiner depuis le début de l'itération (ex: cantor)*)
	    let chaine = if clear then chaine else chaine_initiale 
	    and k = if clear then k else 0 in
	    
	    (* Appeler rec_draw avec les valeurs ci-dessus, sans pauses cette fois-ci *)
	    rec_draw k chaine_initiale chaine rws interpretation point_depart echelle grow (scale_factor*.zoom) iter clear (Flux(c)) current_color false;
	    finish_svg_file c; (* Fermer les balises *)

	    (* Appel récursif*)
	    reponse_utilisateur k chaine_initiale chaine rws interpretation point_depart echelle grow scale_factor zoom iter clear current_color
          end
	    
        | _ -> begin
          let (new_point_depart, zoom, current_color) =
	    match event.key with
	    (* Modifier la couleur du graphisme *)
	    | 'o' -> let (r,g,b) = (246,121,25) in (set_color (rgb r g b); (point_depart, 1.0, (r,g,b)))
	    | 'r' -> let (r,g,b) = (255,0,0) in (set_color (rgb r g b); (point_depart, 1.0, (r,g,b)))
	    | 'v' -> let (r,g,b) = (0,153,0) in (set_color (rgb r g b); (point_depart, 1.0, (r,g,b)))
	    | 'j' -> let (r,g,b) = (255,255,0) in (set_color (rgb r g b); (point_depart, 1.0, (r,g,b)))
	    | 'n' -> let (r,g,b) = (0,0,0) in (set_color (rgb r g b); (point_depart, 1.0, (r,g,b)))
	    | 'b' -> let (r,g,b) = (0,0,153) in (set_color (rgb r g b); (point_depart, 1.0, (r,g,b)))
	    (* Modifier l'angle theta (2d) du graphisme - 1 = anticlockwise, 2 = clockwise *)
	    | '1' -> (thetaInit := !thetaInit + 10; (point_depart, 1.0, current_color))
	    | '2' -> (thetaInit := !thetaInit - 10; (point_depart, 1.0, current_color))
	    (* Modifier l'angle phi (3d) *)
	    | 'a' -> (phiInit := !phiInit + 10 ; (point_depart, 1.0, current_color))
	    | 'z' -> (phiInit := !phiInit + 10; (point_depart, 1.0, current_color))
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
	    | '7' -> (point_depart, zoom*.1.1, current_color)
	    | '8' -> (point_depart, zoom*.0.9, current_color)
	    | _ -> (point_depart, zoom*.1.0, current_color) in

	  begin
	    clear_graph();
	    
	    (* Redessiner juste la dernière chaine si le graphisme est effacé à chaque fois
	    Sinon redessiner depuis le début de l'itération (ex: cantor)*)
	    let chaine = if clear then chaine else chaine_initiale 
	    and k = if clear then k else 0 in
	    rec_draw k chaine_initiale chaine rws interpretation  new_point_depart echelle grow (scale_factor*.zoom) iter clear NA current_color false;
	    (* Appel récursif avec de nouvelles valeurs *)
	    reponse_utilisateur k chaine_initiale chaine rws interpretation  new_point_depart echelle grow scale_factor zoom iter clear current_color
	  end
        end
  with Quit -> close_graph()
    
and
    (* Faire un appel à aux_draw pour chaque itération de 1 jusqu'à n *)
    rec_draw k chaine_initiale chaine rws interpretation point_depart echelle grow scale_factor iter clear save current_color pause=
  let tmp = ( if grow then 1 else k ) in
  begin
    if clear then clear_graph() else ();
    let minmax_xy = { minx = 0.0 ; maxx = 0.0 ; miny = 0.0 ; maxy = 0.0 } in
    let (_,_,_,_)= aux_draw chaine interpretation point_depart minmax_xy !thetaInit !phiInit ( scale_factor *. (echelle ** (float_of_int tmp))) false save current_color in ();
    
    (* s'il s'agit de la dernière itération *)
    if iter=(k+1) then
      (* appeler la réponse à l'utilisateur - seulement s'il y a des pauses, indicateur qu'il n'y a pas déjà une attente de réponse *)
      (if pause then 
	  reponse_utilisateur k chaine_initiale chaine rws interpretation point_depart echelle grow scale_factor 1. iter clear current_color
       else ())
	
    (* si ce n'est pas la dernière itération *)
    else
      begin
        (* petite pause *)
	if pause then
          let t = Unix.gettimeofday () +. 1.0 in
          while (Unix.gettimeofday () < t) do () done
	else () ;
        (* prochaine itération *)
        rec_draw (k+1) chaine (rewrite rws chaine) rws interpretation point_depart
          echelle grow scale_factor iter clear save current_color pause;
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
    
    (* Calculer la nième chaîne pour juger s'il y a trop d'opérations *)
    let n_chaine = calc_n_chaine chaine rws iter in
    if (taille n_chaine) > (size_x() * size_y() ) then failwith "Vous demandez trop d'opérations. Diminuez la valeur de n\n";
    
  (* Calculer l'échelle du graphisme *)
    let echelle = check_echelle rws interpretation NA current_color in
    
  (* Calculer le scale_factor entre chaque itération et le point de départ de l'image *)
    let (scale_factor, point_depart) =
      check_adjustment (float_of_int (size_x ())) (float_of_int (size_y ()))
	n_chaine interpretation (echelle ** (float_of_int (iter-1))) NA current_color
    in
    let echelle = (if grow then (echelle ** (float_of_int (iter-1))) else echelle) in
  (* Commencer l'itération *)
    rec_draw 0 chaine chaine rws interpretation point_depart echelle grow scale_factor iter clear NA current_color true
  end
;;
