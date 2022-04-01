open Graphics;;
open CellularAutomaton;;

(*******************************)
(*
  DIMENSIONS: You can change cellPerSide values to change width and height of display
*)
let dimensionGUI = {numberOfDimention=2; cellPerSide=30::45::[]};;
Random.init;;

(*
  Open windows with appropriate size to accomodate cellPerSide of dimensionGUI
  +100+100 represent windows offset from border of screen
*)
match dimensionGUI.cellPerSide with
  | x::y::[] -> open_graph (" "^(string_of_int (x*10+20))^"x"^(string_of_int (y*10+20))^"+100+100")
  | _ -> failwith "Wrong format";;

(*
  Two fonctions drawing rectangles at given coordonates of size 10*10 for the game of life.
*)
let draw_alive x y = fill_rect x y 10 10;;
let draw_dead x y = draw_rect x y 10 10;;

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
  Remove all key stacked up in the list of keys pressed.
  Stop keys from staking up if one key stay pushed down.
*)
let rec flushKeyPressed () =
  if key_pressed () then
    let _ = read_key () in flushKeyPressed ();;

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
          | Aliv -> draw_alive (x*10) (y*10); if x+1>width then aux 1 (y+1) t else aux (x+1) y t
          | Dead -> draw_dead (x*10) (y*10); if x+1>width then aux 1 (y+1) t else aux (x+1) y t
  in aux 1 1 ruban;;

let draw_gameOfLife () =
  let rec aux currentRuban =
    draw_matrice currentRuban;
    let nextIt = nextStep currentRuban gameOfLife diagonalNeighbords dimensionGUI in
    let rec key () =
    let e = wait_next_event [Button_down; Key_pressed] in
      if e.keypressed then
      begin
        if e.Graphics.key = 'n' then
        begin
          flushKeyPressed ();
          aux nextIt
        end
        else
        
        if e.Graphics.key = 'q' then
        begin
          flushKeyPressed ();
          close_graph ()
        end
        else if e.Graphics.key = 'r' then
        begin
          flushKeyPressed ();
          aux create_rubanStartGUI
        end
        else
          key ()
      end
      else
      if e.Graphics.button then
        begin
          let clickCoordonate = ((e.Graphics.mouse_x-10)/10+1)::((e.Graphics.mouse_y-10)/10+1)::[] in
            if coordonateInsideDimension dimensionGUI clickCoordonate then
              aux (inverseState (matriceCoordonateToFlatCoordonate dimensionGUI clickCoordonate) currentRuban)
            else
              key ()
        end
    in key ()
  in aux create_rubanStartGUI;;

draw_gameOfLife ();;
