open Graphics;;
open CellularAutomaton;;

(*******************************)
(*
  DIMENSIONS: You can change cellPerSide values to change width and height of display
*)
let dimensionGUI = {numberOfDimention=2; cellPerSide=30::30::[]};;
Random.init;;

let s = 20;;
let x_size_side = 
  match dimensionGUI.cellPerSide with
    | x::y::[] -> (x*s+20)-100
    | _ -> failwith "Wrong format";;
let y_size_side = 
  match dimensionGUI.cellPerSide with
    | x::y::[] -> (y*s+20)-100
    | _ -> failwith "Wrong format";;

(*
  Open windows with appropriate size to accomodate cellPerSide of dimensionGUI
  +100+100 represent windows offset from border of screen
*)
match dimensionGUI.cellPerSide with
  | x::y::[] -> open_graph (" "^(string_of_int (x*s+20))^"x"^(string_of_int (y*s+20))^"+100+100")
  | _ -> failwith "Wrong format";;

(*
  Two fonctions drawing rectangles at given coordonates of size 10*10 for the game of life.
*)

let draw_alive x y = fill_rect x y s s;;
let draw_dead x y = draw_rect x y s s;;

(*
  Create a blank ruban for the game of life.
*)
let create_rubanStartGUI =
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
  Draw given ruban for game of life
*)
let draw_matrice ruban =
  clear_graph ();
  let width = match dimensionGUI.cellPerSide with
                    | width::height::[] -> width
                    | _ -> failwith "Wrong dimension" in
  let rec aux x y currentRub =
    match currentRub with
      | [] -> ()
      | h::t ->
        match h with
          | Aliv -> draw_alive (x*s) (y*s); if x+1>width then aux 1 (y+1) t else aux (x+1) y t
          | Dead -> draw_dead (x*s) (y*s); if x+1>width then aux 1 (y+1) t else aux (x+1) y t
  in aux 1 1 ruban;;

let draw_gameOfLife ruban =
  let rec aux currentRuban =
    draw_matrice currentRuban;
    let nextIt = nextStep currentRuban gameOfLife diagonalNeighbords dimensionGUI in
    let e = wait_next_event [Key_pressed] in
    if e.keypressed then
      match e.key with
      | 'q' -> close_graph ();
      | _ -> aux nextIt
  in aux ruban;;


(* game of life with a list of a number of cell to be equals to *)
  let draw_gameOfLifeCustom ruban nbCell =
  let rec aux currentRuban =
    draw_matrice currentRuban;
    let nextIt = nextStepCustom currentRuban gameOfLifeCustom diagonalNeighbords dimensionGUI nbCell in
    let e = wait_next_event [Key_pressed] in
    if e.keypressed then
      match e.key with
      | 'q' -> close_graph ();
      | _ -> aux nextIt
  in aux ruban;;

(* draw_gameOfLife create_rubanStartGUI;;

close_graph () *)

let match_input_user e = 
  match e with 
  | '1' -> 1
  | '2' -> 2
  | '3' -> 3
  | '4' -> 4
  | '5' -> 5
  | '6' -> 6
  | '7' -> 7
  | '8' -> 8
  | _ -> 0;;



(* place the checkbox in the graph *)
let create_checkbox x y = 
  let rec place_box x i =
    if(i < 9) then 
    begin
      moveto x (y+25);
      draw_string (string_of_int i);
      moveto (x) y;
      draw_rect x y s s;
      place_box (x+50) (i+1);
    end
    else ();
  in place_box x 0
;;

(* get the coor of the box in a list *)
let checkbox_coord x = 
    let rec place_box x i list =
    if(i < 9) then 
        let listBox = x::list in
        place_box (x+50) (i+1) listBox
    else list;
    in place_box x 0 []
;;

(* check if the coord x y are in a box *)
let check_mouse_clicked x y coor_box_x box_y =
    if(y >= box_y && y <= (box_y+s)) then 
        let rec aux list i =
            match list with 
            | [] -> (false,0,0) 
            | h::t -> if (x >= h && x <= (h+s)) then (true,h,i)
                    else aux t (i+1)
        in aux coor_box_x 0
    else (false,0,0);;


(* allow the user to click on box *)
let rec event_loop x y listBoxCoord listChecked =
    if button_down () then  
        let x_mouse, y_mouse = mouse_pos () in
        let inBox, x_box, ind = check_mouse_clicked x_mouse y_mouse listBoxCoord y in 
        
            if(inBox) then begin 
                let list = (List.nth listBoxCoord ind)::listChecked in (*  recup pas le bon indice logique *)
                fill_rect x_box y s s; 
                print_int ind;
                event_loop x y listBoxCoord list
            end
            else begin event_loop x y listBoxCoord listChecked
            end
    else 
            if key_pressed () then
                if(read_key () = 'c') then (* press c for custom version *)
                    draw_gameOfLifeCustom create_rubanStartGUI listChecked
                else 
                    draw_gameOfLife create_rubanStartGUI
            else 
                event_loop x y listBoxCoord listChecked;;

let menu_game = 
    moveto (x_size_side/2) ((y_size_side/2)+250);
    draw_string "GAME OF LIFE";
    
    let x = x_size_side/4 in 
    let y = y_size_side/3 in 
    create_checkbox x y;

    let listBoxCoord = checkbox_coord x in 
    event_loop x y listBoxCoord []; 
;;

menu_game;;

