
Bienvenue a "LSystem_Display"

ce logiciel sert a montrer les Lindenmayer Systemes graphiquement.
Voir http://www.pps.univ-paris-diderot.fr/~treinen/teaching/pf5/project/projet.pdf
pour plus de details sur les Lindenmayer Systems

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

options graphiques:



