(*2D Neighborhood*)
(*return the list of coordinates of *)

type state = Alive | Dead;;

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

(*
    von Neumann neighborhood for any dimension (i think)
    return list of integer coordinates of the neighborhood
*)
let noDiagonalNeighbords dimension coordonate = 
  let maxCoo = maxCoordinates dimension in
  let left = if coordonate - 1 > 0 then (coordonate - 1)::[] else [] in
  let right = if coordonate + 1 <= maxCoo then (coordonate + 1)::[] else [] in
  let rec aux dimensionBound neighborhood multiplicator =
    match dimensionBound with
      | [] -> neighborhood;
      | h::t -> 
            let north = if coordonate + h * multiplicator <= maxCoo then (coordonate + h * multiplicator)::[] else [] in
            let south = if coordonate - h * multiplicator > 0 then (coordonate - h * multiplicator)::[] else [] in
            aux t (north@south@neighborhood) (h*multiplicator)
  in left@right@(aux dimension.cellPerSide [] 1);;

let rec print_list listOfInt =
  match listOfInt with
    | [] -> ()
    | h::t -> print_int h; print_endline "";print_list t;;

(* let dimensionTest = {numberOfDimention = 3; cellPerSide = 4::8::2::[]};;
let dimensionTest2 = {numberOfDimention = 2; cellPerSide = 50::3::[]};;
print_list (noDiagonalNeighbords dimensionTest 64);;
print_endline "";
print_list (noDiagonalNeighbords dimensionTest2 18);; *)





(*
  state -> listOfNeighbordsStates -> newState
*)
let transitionFonction state listOfneighbors = ();;

(*
  listeOfState 
*)
let nextStep listOfState transitionFonction calculateNeighbors dimension = ();;