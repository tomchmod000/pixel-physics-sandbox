open Pixel

(** [current_matrix] is the mutable board used for the main game loop. *)
let current_matrix = ref (Pixel.init_board ())

(** [current_material] is a global variable that is used to represent what the
    users currently drawing with *)
let current_material = ref Dirt

(** [current_action] represents what action the user is currently able to do
    with their mouse: 0 = delete, 1 = clear, 2 = draw *)
let current_action = ref 2

(** [mouse_pos_to_index board material] maps mouse position on game window to
    coordinates on pixel board. Window size is 500 x 500, with menu being the
    bottom 100 coordinates. This leaves 500 * 400 -> 100 * 80 pixels of display
    space.*)
let mouse_pos_to_index (matrix : board) (material : material) : board =
  if Graphics.button_down () then
    match Graphics.mouse_pos () with
    | x, y ->
        let x = if x < 0 then 0 else if x >= 500 then 499 else x in
        let y = if y < 0 then 0 else if y >= 500 then 499 else y in

        (* let _ = print_endline ("pos=" ^ string_of_int x ^ ", " ^
           string_of_int y) in *)

        (* If x, y ouside of window: Don't do anything *)
        (* If in menu: perform menu actions *)
        if y > 70 && y < 80 then
          if x > 200 && x < 230 then
            let _ = print_endline "changing to steel" in
            let _ = current_material := Steel in
            matrix
          else if x > 300 && x < 325 then
            let _ = print_endline "changing to dirt" in
            let _ = current_material := Dirt in
            matrix
          else if x > 400 && x < 430 then
            let _ = print_endline "changing to water" in
            let _ = current_material := Water in
            matrix
          else matrix
        else if y > 18 && y < 28 then
          if x > 200 && x < 230 then
            let _ = print_endline "changing to delete" in
            let _ = current_action := 0 in
            matrix
          else if x > 300 && x < 330 then
            let _ = print_endline "changing to clear" in
            let _ = current_action := 1 in
            matrix
          else if x > 400 && x < 424 then
            let _ = print_endline "changing to draw" in
            let _ = current_action := 2 in
            matrix
          else matrix
        else if y > 100 && y < 500 then
          let _ =
            print_endline
              ("index="
              ^ string_of_int (x / 5)
              ^ ", "
              ^ string_of_int ((y - 100) / 5))
          in
          if !current_action = 0 then
            let _ = print_endline "deleting" in
            let _ = delete_pixel (x / 5, (y - 100) / 5) matrix in
            (* let _ = print_board !current_matrix in *)
            matrix
          else if !current_action = 1 then
            let _ = print_endline "clearing" in
            let _ = current_matrix := init_board () in
            (* let _ = print_board !current_matrix in *)
            matrix
          else if !current_action = 2 then
            let _ = print_endline "drawing" in
            let new_pixel = create_pixel !current_material in
            let _ = draw_pixel (new_pixel (x / 5, (y - 100) / 5)) matrix in
            (* let _ = print_board !current_matrix in *)
            matrix
          else matrix
        else matrix
  else matrix

(** [return_unit_helper board] removes the board return from a function and
    makes it return unit instead. *)
let return_unit_helper (matrix : board) : unit = ()

(** [mouse_control ()] is the main control loop for user input. *)
let mouse_control () =
  return_unit_helper (mouse_pos_to_index !current_matrix !current_material)

(** [pixel_movement_reference ()] is the initial prototype for pixel drawing and
    deleting. *)
let pixel_movement_reference () : unit =
  if Graphics.key_pressed () then
    let _ = Graphics.set_color Graphics.black in
    let _ = Graphics.fill_rect 200 200 5 5 in
    let x = Graphics.read_key () in
    let _ = print_endline (String.make 1 x) in
    let _ = Graphics.set_color Graphics.white in
    let _ = Graphics.fill_rect 200 200 5 5 in
    let _ = Graphics.set_color Graphics.black in
    let _ = Graphics.fill_rect 250 250 5 5 in
    let y = Graphics.read_key () in
    let _ = print_endline (String.make 1 y) in
    let _ = Graphics.set_color Graphics.white in
    Graphics.fill_rect 250 250 5 5

(** [draw_pix pixel x y] draws a pixel onto the screen. *)
let draw_pix p x y : unit =
  let r, g, b = p.color in
  Graphics.set_color (Graphics.rgb r g b);
  Graphics.fill_rect ((x * 5) - 2) (100 + (y * 5) + 2) 5 5

(** [draw_board ()] draws all the pixels of the current board onto the screen. *)
let draw_board () : unit =
  let b = !current_matrix in
  for x = 0 to 99 do
    for y = 0 to 79 do
      draw_pix b.(x).(y) x y
    done
  done

(** [update_board ()] updates the current board with the new positions of all
    the pixels. *)
let update_board () : unit = current_matrix := rev_flush !current_matrix

(** [event_loop ()] calls all the functions needed to run the program in
    succession. *)
let rec event_loop () : unit =
  mouse_control ();
  (* pixel_movement_reference (); *)
  update_board ();
  draw_board ();
  event_loop ()
