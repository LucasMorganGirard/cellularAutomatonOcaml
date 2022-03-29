(** Lucas GIRARD - Orianne GARAUD
 Licence 3 informatique - Univsité de Sciences et Techniques de Nantes
 Programation fonctionnelle
 Réalisation d'un modèle d'automate cellulaire.
 L'objectif est d'avoir un modèle simple
 *)

 (*
   State is an int to allows for a big number of possible state and to facilitate using different alphabet.
   In order to use it in other methods, a tech method to match int to alphabet and alphabet to int is needed.
 *)
type state = int list;;

type alphabet1 = Alive | Dead;;
type alphabet2 = A | B | C | D | E | F | G | H;;

let alphabet1ToState symbole =
  match symbole with
    | Alive -> 1
    | Dead -> 2;;

let stateToAlphabet1 state =
  if state == 1 then Alive
  else Dead;;

let printAlphabet1 symbole =
    match symbole with
      | Alive -> print_string "Alive"
      | Dead -> print_string "Dead";;

let alphabet2ToState symbole =
  match symbole with
    | A -> 1
    | B -> 2
    | C -> 3
    | D -> 4
    | E -> 5
    | F -> 6
    | G -> 7
    | H -> 8;;

let stateToAlphabet2 state =
  if state = 1 then A
  else if state = 2 then B
  else if state = 3 then C
  else if state = 4 then D
  else if state = 5 then E
  else if state = 6 then F
  else if state = 7 then G
  else H;;

let printAlphabet2 symbole =
  match symbole with
    | A -> print_string "A"
    | B -> print_string "B"
    | C -> print_string "C"
    | D -> print_string "D"
    | E -> print_string "E"
    | F -> print_string "F"
    | G -> print_string "G"
    | H -> print_string "H";;
(*
  Number of dimention and size of each side of the dimension;
  exemple for 2D : width and height; {numberOfDimention:2; cellPerSide: 10::10::[]}
  a space of 2 dimention with 10 cells width and 10 cells height.
*)
type dimension = {numberOfDimention: int; cellPerSide: int list};;

(*
  find the maximum coordinates for a list of @dimension
*)
let maxCoordinates dimension =
  let rec aux currentMult currentCellPerSide =
    match currentCellPerSide with
      | [] -> currentMult
      | h::t -> aux (currentMult * h) t
  in aux 1 dimension.cellPerSide;;

(*
  return state from list @listeOfState at integer @coordonate
*)
let stateOfCoordonate listOfState coordonate =
  (*
    cursor increase until it match with current coordonate
    list is traveled alongside cursor
  *)
  let rec aux currentList cursor =
    match currentList with
      | [] -> failwith "error given coordonate don't match list size"
      | h::t -> 
        if coordonate == cursor then
          h
        else
          aux t (cursor+1)
  in aux listOfState 0;;

(********************************)
(* 
    tech methods to find 2D neighborhood
*)
let flatCoordonateToMatriceCoordonate dimension coordonateFlat =
  match dimension.cellPerSide with
    | width::height::t ->
    begin
        let y = ((coordonateFlat-1) / width) + 1 in
        let x = (coordonateFlat - (y-1)*width) in x::y::[]
    end
    | _ -> failwith "Wrong dimension format";;

let matriceCoordonateToFlatCoordonate dimension coordonateMatrice =
  match dimension.cellPerSide with
    | width::height::t -> 
    begin
      match coordonateMatrice with
        | x::y::t -> (y-1)*width + x
        | _ -> failwith "Wrong coordonate format"
    end
    | _ -> failwith "Wrong dimension format";;


let coordonateInsideDimension dimension coordonateMatrice =
  match dimension.cellPerSide with
    | width::height::t ->
    begin
      match coordonateMatrice with
      | x::y::t -> x > 0 && x <= width && y > 0 && y <= height
      | _ -> failwith "Wrong coordonate format"
    end
    | _ -> failwith "Wrong dimension format";;
(********************************)

(*
    von Neumann neighborhood
    return list of integer coordinates of the neighborhood
*)
let noDiagonalNeighbords dimension coordonate = 
  

let rec print_list listOfInt =
  match listOfInt with
    | [] -> ()
    | h::t -> print_int h; print_endline "";print_list t;;

    (*
    state -> listOfNeighbordsStates -> newState
    *)
    let transitionFonction state listOfneighbors = ();;

    (*
    listeOfState 
    *)
    let nextStep listOfState transitionFonction calculateNeighbors dimension = ();;

(*     
    let dimensionTest = {numberOfDimention = 2; cellPerSide = 51::36::[]};;
    let dimensionTest2 = {numberOfDimention = 2; cellPerSide = 50::3::[]};;
    print_list (flatCoordonateToMatriceCoordonate dimensionTest 64);;
    print_endline "";
    print_list (flatCoordonateToMatriceCoordonate dimensionTest2 18);;
    print_endline "";
    print_int (matriceCoordonateToFlatCoordonate dimensionTest (flatCoordonateToMatriceCoordonate dimensionTest 64));; *)