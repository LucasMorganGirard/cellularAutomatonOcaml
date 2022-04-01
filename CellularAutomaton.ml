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

(*
  Alphabet1 is used for exemple game of life
*)
type alphabet1 = Aliv | Dead;;
type alphabet2 = A | B | C | D | E | F | G | H;;

let alphabet1ToState symbole =
  match symbole with
    | Aliv -> 1
    | Dead -> 2;;

let stateToAlphabet1 state =
  if state == 1 then Aliv
  else Dead;;

let printAlphabet1 symbole =
    match symbole with
      | Aliv -> print_string "Alive "
      | Dead -> print_string "Dead  ";;

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
      | [] -> print_int coordonate; failwith "error given coordonate don't match list size"
      | h::t -> 
        if coordonate == cursor then
          h
        else
          aux t (cursor+1)
  in aux listOfState 1;;

(********************************)
(* 
    tech methods to find 2D neighborhood
    convert coordonate from single dimension integer to two dimension integer coordonate
*)
let flatCoordonateToMatriceCoordonate dimension coordonateFlat =
  match dimension.cellPerSide with
    | width::height::t ->
    begin
        let y = ((coordonateFlat-1) / width) + 1 in
        let x = (coordonateFlat - (y-1)*width) in x::y::[]
    end
    | _ -> failwith "Wrong dimension format";;

(*
    Reverse from flatCoordonateToMatriceCoordonate
*)
let matriceCoordonateToFlatCoordonate dimension coordonateMatrice =
  match dimension.cellPerSide with
    | width::height::t -> 
    begin
      match coordonateMatrice with
        | x::y::t -> (y-1)*width + x
        | _ -> failwith "Wrong coordonate format"
    end
    | _ -> failwith "Wrong dimension format";;

(*
    check if matrice coordonate (list of coordinates) is valid for given dimension
*)
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
    Moore neighborhood
    return list of integer coordinates of the neighborhood of @coordonateFlat,
    @coordonateFlat a coordonate in a one dimensional list representing a matrice of dimension @dimension
*)
let diagonalNeighbords dimension coordonateFlat =
  (* calculate north and south of @coordonateFlat depending on given i *)
  let upDown i =
    match flatCoordonateToMatriceCoordonate dimension coordonateFlat with
      | (x::y::[]) ->
      begin
        if  (
              coordonateInsideDimension dimension (x::(y+i)::[])
            )
        then (matriceCoordonateToFlatCoordonate dimension (x::(y+i)::[]))::[]
        else []
      end
      | _ -> failwith "Wrong coordinates"
  in
    let goSide x i = match (flatCoordonateToMatriceCoordonate dimension x) with
                    | x::y::[] -> (x+i)::y::[]
                    | _ -> failwith "Wrong format"
    in
    let west x =
      if (coordonateInsideDimension dimension (goSide x (-1))) then (x - 1)::[] else []
  in
    let est x =
      if (coordonateInsideDimension dimension (goSide x 1)) then (x + 1)::[] else [] 
  in
  (* calculate est and west for each element of @listOfNorthSouthCenterNeighbors, north south and center*)
    let rec aux listOfNorthSouthCenterNeighbors result =
      match listOfNorthSouthCenterNeighbors with
        | [] -> result
        | h::t -> aux t (west h)@(est h)@result
  in aux ((upDown 1)@(upDown (-1))@(coordonateFlat::[])) (upDown 1)@(upDown (-1));;

(*
  transition for alphabet 1, rules of game of life
*)
let gameOfLife symbol listOfNeighborsSymbols listOfState =
  let rec auxNbAliveNeighbors neighbors res =
    match neighbors with
      | [] -> res
      | h::t -> auxNbAliveNeighbors t (res +
                                        match stateOfCoordonate listOfState h with
                                          | Aliv -> 1
                                          | Dead -> 0
                                      )
  in let nbAliveN = auxNbAliveNeighbors listOfNeighborsSymbols 0 in
    match symbol with
      | Aliv -> if nbAliveN = 2 || nbAliveN = 3 then Aliv else Dead
      | Dead -> if nbAliveN = 3 then Aliv else Dead;;

(*

*)
let nextStep listOfState transitionFonction calculateNeighbors dimension = 
  let rec aux currentList currentCoord result =
    match currentList with
      | [] -> result
      | h::t -> aux t (currentCoord+1) (result@(transitionFonction h (calculateNeighbors dimension currentCoord) listOfState)::[])
  in aux listOfState 1 [];;

(********************************)
(* Utilities *)

(* print a list of int *)
let rec print_list listOfInt =
  match listOfInt with
    | [] -> print_endline ""
    | h::t -> print_int h; print_string " ";print_list t;;

(*
  print a matrice (represented by a one dimension list of 'a) of dimension @dimension,
  fonction to print 'a must be provided in @statePrintFct.
*)
let print_matrice listOfState statePrintFct dimension =
  match dimension.cellPerSide with
  | x::y::t -> 
  begin
    let rec aux currList currX =
      match currList with
        | [] -> ()
        | h::t ->
          if currX = x then
          begin
            statePrintFct h; print_endline ""; aux t 1
          end
          else
          begin
            statePrintFct h; aux t (currX+1)
          end
    in aux listOfState 1
  end
  | _ -> failwith "Wrong dimension format";;


(********************************)
(* TESTS *)

(*
  the ruban and the dimension goes together and the ruban MUST match de given dimension size for height and width.
  The ruban is a one dimension list so it just need to be of size width*height size for 2D.
*)

(* let dimensionTest = {numberOfDimention = 2; cellPerSide = 5::3::[]};;
let rubanTest = 
  Dead::Dead::Dead::Dead::Aliv::
  Aliv::Aliv::Aliv::Dead::Dead::
  Dead::Dead::Dead::Dead::Aliv::[];;

let print_gameOfLife ruban iterations =
  let rec aux nbIteration currentRuban =
    let nextIt = nextStep currentRuban gameOfLife diagonalNeighbords dimensionTest in
    print_matrice nextIt printAlphabet1 dimensionTest; print_endline "";
    if nbIteration > 0 then
    begin
      aux (nbIteration-1) nextIt
    end
  in aux iterations ruban;;

print_gameOfLife rubanTest 100 *)


(* check if the number of alive cells respect one of the rule in the list *)
let number_of_cells nbAlive listSelectedCell = 
  let rec aux l = 
    match l with 
    | [] -> false
    | h::t -> if(nbAlive = h) then true 
              else aux t 
  in aux listSelectedCell;;
  
(* game of life with custom rules *)
let gameOfLifeCustom symbol listOfNeighborsSymbols listOfState listSelectedCell =
  let rec auxNbAliveNeighbors neighbors res =
    match neighbors with
      | [] -> res
      | h::t -> auxNbAliveNeighbors t (res +
                                        match stateOfCoordonate listOfState h with
                                          | Aliv -> 1
                                          | Dead -> 0
                                      )
  in let nbAliveN = auxNbAliveNeighbors listOfNeighborsSymbols 0 in
    match symbol with
      | Aliv -> if number_of_cells nbAliveN listSelectedCell then Aliv else Dead
      | Dead -> if number_of_cells nbAliveN listSelectedCell then Aliv else Dead;;


  let nextStepCustom listOfState transitionFonction calculateNeighbors dimension listSelectedCell = 
  let rec aux currentList currentCoord result =
    match currentList with
      | [] -> result
      | h::t -> aux t (currentCoord+1) (result@(transitionFonction h (calculateNeighbors dimension currentCoord) listOfState listSelectedCell)::[])
  in aux listOfState 1 [];;
