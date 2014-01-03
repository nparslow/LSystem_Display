Bienvenue à "LSystem_Display"

Ce logiciel sert à montrer les Systèmes de Lindenmayer graphiquement.
Voir http://www.pps.univ-paris-diderot.fr/~treinen/teaching/pf5/project/projet.pdf
pour plus de détails sur les Lindenmayer Systems

Compilation:
./Makefile

Fichiers:
Makefile                            : executable, pour compiler
project_v05.ml                      : code source
project_v05.cmi et project_v05.cmo  : code binaire
project_v05                         : executable, logiciel
lextest                             : directoire contentant lexer et parser
./lextest/lexer.mll                 : ocamllex lexer code source
./lextest/parser.mly                : ocamlyacc parser code source
./lextest/{lexer.mli, lexer.ml, parser.mli, parser.ml} : lexer et parser code source
./lextest/{lexer.cmi, lexer.cmo, parser.cmi, parser.cmo} : lexer et parser code binaire


options command line:

"-e"  "Afficher la liste d'exemples possible"
"-n"  "Le nombre d'itérations à faire. 1 correspond à la séquence initiale"
"-grow" "Agrandit avec chaque itération. \nPar défaut -grow n'est pas applicable"
"-no_clear" "Ne pas effacer le graphisme après chaque itération. \nPar défaut no_clear n'est pas applicable"
"-exemple" "L'exemple, ou le fichier d'un exemple. Voir option -e pour voir la liste d'exemples"

Exemples possible :
snow, koch1, koch2, koch3, koch4, koch5, dragon, sierp, 6gosp, 4gosp, plant, plant3d, br1, br2, cantor

options graphiques (à utiliser suite à la dernière itération du graphisme):

changer la couleur du graphisme :
‘o’ -> orange
‘n’ -> noir
‘b’ -> bleu
‘r’ -> rouge
‘v’ -> vert
‘j’ -> jaune
modifier l’angle (théta) du dessin (en 2D) :
‘1’ -> contre le sens des aiguilles d’une montre
‘2’ -> dans le sens des aiguilles d’une montre
modifier l’angle (phi) du dessin (en 3D) :
‘a’ -> rotation autour de l'axe x contre le sens des aiguilles d'une montre
‘b’ -> rotation autour de l'axe x dans le sens des aiguilles d'une montre
déplacer le graphisme :
‘3’ -> vers la gauche
‘4’ -> vers la droite
‘5’ -> vers le haut
‘6’ -> vers le bas
faire un zoom :
‘7’ -> zoom in
‘8’ -> zoom out



Pour créer votre propre L-Système:

actuellement il faut modifier le code, donce c'est fortement déconseillé.
Un moyen de créer un L-Système à partir d'un fichier .txt est en cours
de développement.




