cd ./lextest/
./compile.script
cd ..
ocamlfind ocamlc -I ./lextest/ parser.cmo lexer.cmo -package camlimages.all_formats -package graphics -package camlimages.graphics unix.cma -linkpkg project_v05.ml -o project_V05
