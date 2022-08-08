university Project

COMPILE :
    ocamlfind ocamlc CellularAutomaton.ml GUI.ml main.ml -o a.out -linkpkg -package graphics

alternative compile :
    <ocamlc directory>ocamlc -I <graphics directory>/graphics/ graphics.cma -o a.out CellularAutomaton.ml GUI.ml main.ml
    remplace <ocamlc directory> and <graphics directory> with the appropriate directories.

EXECUTION :
    In custom menu:
        You can chose to lunch a custom game by pressing 'c' after you have selected the rules you want with the checkboxs.
        Alternativly, you can start a game by pressing any other key.
    In game:
        You can click on any cell to invert it.
        You can press 'r' to create a new random matrice.
        You can press 'q' to quit.
        You can press any other key to apply the rules one time to all the cells.
