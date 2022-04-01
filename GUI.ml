(** 
  GIRARD Lucas E176580k
  GARAUD Orianne E216701U
  Licence 3 informatique - Univsité de Sciences et Techniques de Nantes
  Programation fonctionnelle
  Réalisation d'un modèle d'automate cellulaire.
*)

open Graphics;;
open CellularAutomaton;;

(*******************************)
(*
  DIMENSIONS: You can change cellPerSide values to change width and height of display
*)
let dimensionGUI = {numberOfDimention=2; cellPerSide=30::30::[]};;

let casesSize = 20;;

let x_size_side = 
  match dimensionGUI.cellPerSide with
    | x::y::[] -> (x*casesSize+20)-100
    | _ -> failwith "Wrong format";;

let y_size_side = 
  match dimensionGUI.cellPerSide with
    | x::y::[] -> (y*casesSize+20)-100
    | _ -> failwith "Wrong format";;

(*
  Open windows with appropriate size to accomodate cellPerSide of dimensionGUI
  +100+100 represent windows offset from border of screen
*)
let lunchWindow () =
  match dimensionGUI.cellPerSide with
    | x::y::[] -> open_graph (" "^(string_of_int (x*casesSize+2*casesSize))^"x"^(string_of_int (y*casesSize+2*casesSize))^"+100+100")
    | _ -> failwith "Wrong format";;


(*******************************)
(*
  Every one of the following method needs a valid instance of lunchWindow ()
*)

(*
  int -> int -> unit
  Two fonctions drawing rectangles at given coordonates of size 10*10 for the game of life.
*)
let draw_alive x y = fill_rect x y casesSize casesSize;;
let draw_dead x y = draw_rect x y casesSize casesSize;;

(*
  unit -> alphabet list
  Create a random 1D ruban of alphabet sypmbol (states) for the game of life.

  EXTERNAL METHODS:
  Uses dimensionGUI as a global variable and as the reference to the dimensions of the ruban.
*)
let create_rubanStartGUI () =
  let width = match dimensionGUI.cellPerSide with
                | width::height::[] -> width
                | _ -> failwith "Wrong dimension" in
  let height = match dimensionGUI.cellPerSide with
                | width::height::[] -> height
                | _ -> failwith "Wrong dimension" in
  let rec aux startRuban coordonate =
    if coordonate <= width*height then
    begin
      Random.self_init ();
      let randomState = Random.int 2 in
      if randomState == 0 then
        aux (Dead::startRuban) (coordonate+1)
      else
        aux (Aliv::startRuban) (coordonate+1)
    end
    else
      startRuban
  in aux [] 1;;

(*
  unit -> unit
  Remove all key stacked up in the list of keys pressed.
  Stop keys from staking up if one key stay pushed down.
*)
let rec flushKeyPressed () =
  if key_pressed () then
    let _ = read_key () in flushKeyPressed ();;

(*
  alphabet list -> unit
  Draw given 1D ruban of alphabet sypmbol (states).
  Ruban is 1D list but represente a 2D matrice.

  EXTERNAL METHODS:
  Uses draw_alive and draw_dead to draw the different states of the ruban of alphabet symbols at given coordonate.
  Uses dimensionGUI as a global variable and as the reference to the dimensions of the ruban.
*)
let draw_matrice ruban =
  clear_graph ();
  let width = match dimensionGUI.cellPerSide with
                    | width::height::[] -> width
                    | _ -> failwith "Wrong ruban dimension" in
  let rec aux x y currentRub =
    match currentRub with
      | [] -> ()
      | h::t ->
        match h with
          | Aliv -> draw_alive (x*casesSize) (y*casesSize); if x+1>width then aux 1 (y+1) t else aux (x+1) y t
          | Dead -> draw_dead (x*casesSize) (y*casesSize); if x+1>width then aux 1 (y+1) t else aux (x+1) y t
  in aux 1 1 ruban;;

(*
  unit -> unit
  Play a game of life, depending on user input.

  EXTERNAL METHODS:
  Uses draw_matrice to draw the ruban of alphabet symbols (states).
  Uses next_step to apply the transformation rules to the ruban.
  Uses flushKeyPressed to decrease input lag when a key have been pressed multiple times in a raw.
  Uses create_rubanStartGUI to create the original random ruban of alphabet symbols (states).
  Uses coordonateInsideDimension to check if coordonate clicked on correspond to a symbol in the ruban.
  Uses inverseState to inverse the state of the symbol that was clicked on.
  Uses dimensionGUI as a global variable and as the reference to the dimensions of the ruban.

  n -> next step of algorithm
  q -> quit
  r -> reset board to initial state
  click on cell -> inverse cell state
  *)
let draw_gameOfLife () =
  let rec aux currentRuban =
    draw_matrice currentRuban;
    let nextIt = nextStep currentRuban gameOfLife diagonalNeighbords dimensionGUI in
        let rec key () =
        let e = wait_next_event [Button_down; Key_pressed] in
            if e.keypressed then
                match e.key with
                | 'q' -> flushKeyPressed (); close_graph ();
                | 'r' -> flushKeyPressed (); aux (create_rubanStartGUI ())
                | _ -> flushKeyPressed (); aux nextIt
            else
            if e.button then
                begin
                let clickCoordonate = ((e.mouse_x-casesSize)/casesSize+1)::((e.mouse_y-casesSize)/casesSize+1)::[] in
                    if coordonateInsideDimension dimensionGUI clickCoordonate then
                    aux (inverseState (matriceCoordonateToFlatCoordonate dimensionGUI clickCoordonate) currentRuban)
                    else key ()
                end
        in key ()
    in aux (create_rubanStartGUI ());;


(* 
  unit -> unit
  Play a game of life with custom rules depending on user choice 
  listCheckedAlive -> is the rule for an alive cell
  listCheckedDead -> is the rule for an dead cell
*)
  let draw_gameOfLifeCustom listCheckedAlive listCheckedDead =
  let rec aux currentRuban =
    draw_matrice currentRuban;
    let nextIt = nextStepCustom currentRuban gameOfLifeCustom diagonalNeighbords dimensionGUI listCheckedAlive listCheckedDead in
    let e = wait_next_event [Button_down; Key_pressed] in
      if e.keypressed then
        match e.key with
        | 'q' -> close_graph ();
        | 'r' -> flushKeyPressed (); aux (create_rubanStartGUI ())
        | _ -> aux nextIt
      else if e.button then
        begin
        let clickCoordonate = ((e.mouse_x-casesSize)/casesSize+1)::((e.mouse_y-casesSize)/casesSize+1)::[] in
            if coordonateInsideDimension dimensionGUI clickCoordonate then
            aux (inverseState (matriceCoordonateToFlatCoordonate dimensionGUI clickCoordonate) currentRuban)
        end
  in aux (create_rubanStartGUI ());;


(* 
  int -> int -> unit 
  Place the checkbox in the frame
*)
let create_checkbox x y = 
  let rec place_box x i =
    if(i < 9) then 
    begin
      moveto x (y+25);
      draw_string (string_of_int i);
      moveto (x) y;
      draw_rect x y casesSize casesSize;
      place_box (x+50) (i+1);
    end
    else ();
  in place_box x 0;;


(* 
  int -> int list 
  Get the coordinate of the box x position in a list
 *)
let checkbox_coord x = 
    let rec place_box x i list =
    if(i < 9) then 
        let listBox = x::list in
        place_box (x+50) (i+1) listBox
    else List.rev list;
    in place_box x 0 [];;


(* 
  int -> int -> int list -> int -> bool * int * int
  Check if the coordinates x y of the user's click are in a box 
*)
let check_mouse_clicked x y coor_box_x box_y =
    if(y >= box_y && y <= (box_y+casesSize)) then 
        let rec aux list i =
            match list with 
            | [] -> (false,0,0) 
            | h::t -> if (x >= h && x <= (h+casesSize)) then (true,h,i)
                    else aux t (i+1)
        in aux coor_box_x 0
    else (false,0,0);;


(*
  Use to detect mouse event to fill the checkbox 
  Init the game when a key is pressed 
*)
let rec event_loop x y listBoxCoord listCheckedAlive listCheckedDead =
    if button_down () then  
        let x_mouse, y_mouse = mouse_pos () in
        let inBox1, x_box1, ind1 = check_mouse_clicked x_mouse y_mouse listBoxCoord y in 
        let inBox2, x_box2, ind2 = check_mouse_clicked x_mouse y_mouse listBoxCoord (y-100) in 
        
            if(inBox1) then begin 
                let list = ind1::listCheckedAlive in
                fill_rect x_box1 y casesSize casesSize; 
                event_loop x y listBoxCoord list listCheckedDead
            end
            else if(inBox2) then begin 
                let list = ind2::listCheckedDead in 
                fill_rect x_box2 (y-100) casesSize casesSize; 
                event_loop x y listBoxCoord listCheckedAlive list 
            end
            else begin event_loop x y listBoxCoord listCheckedAlive listCheckedDead
            end
            else 
            if key_pressed () then
                if(read_key () = 'c') then  (* press c for custom version *)
                    draw_gameOfLifeCustom listCheckedAlive listCheckedDead 
                else 
                    draw_gameOfLife ()
            else 
                event_loop x y listBoxCoord listCheckedAlive listCheckedDead;;

(* 
  unit -> unit
  Init the frame and add a description of how to use the game
  Also init the checkbox for the custom version
 *)
let game () = 
    moveto (x_size_side/2) ((y_size_side/2)+250);
    draw_string "GAME OF LIFE";

    moveto (x_size_side/6) ((y_size_side/2)+200);
    draw_string "Command : q -> quit ; r -> reset ; c -> start custom game ";
    
    moveto (x_size_side/6) ((y_size_side/2)+150);
    draw_string "Press any key to start a classic game";
    moveto (x_size_side/6) ((y_size_side/2)+125);
    draw_string "In the classical version, a living cell needs 2 or 3 neighbors";
    moveto (x_size_side/6) ((y_size_side/2)+100);
    draw_string "and a dead cell needs 3 living cells to be alive.";
    
    let x = x_size_side/4 in 
    let y = y_size_side/2 in 
    moveto x (y+50);
    draw_string "Number of neighbors for Alive cells to stay alive";
    create_checkbox x y;
    moveto x (y-50);
    draw_string "Number of neighbors for Dead cells to become alive";
    create_checkbox x (y-100); 

    let listBoxCoord = checkbox_coord x in 
    event_loop x y listBoxCoord [] []; 
;;


