(** [delay seconds] pauses the program for a short duration. *)
let delay (time : int) : unit =
  for variable = time * 100000000 downto 0 do
    ()
  done

(** [draw_box x y] draws a box on the screen at the specified coordinates of
    size 10 x 10 pixels. Requires x and y to be valid screen coordinates. *)
let draw_box x y = Graphics.fill_rect x y 10 10

(** [draw_start_title ()] draws the large title text on the start screen. *)
let draw_start_title () =
  Graphics.set_color Graphics.black;
  delay 4;

  (* S *)
  draw_box 40 350;
  draw_box 50 350;
  draw_box 60 350;
  draw_box 70 350;
  draw_box 80 350;

  draw_box 80 350;
  draw_box 80 360;
  draw_box 80 370;
  draw_box 80 380;
  draw_box 80 390;

  draw_box 70 390;
  draw_box 60 390;
  draw_box 50 390;
  draw_box 40 390;

  draw_box 40 390;
  draw_box 40 400;
  draw_box 40 410;
  draw_box 40 420;
  draw_box 40 430;

  draw_box 40 430;
  draw_box 50 430;
  draw_box 60 430;
  draw_box 70 430;
  draw_box 80 430;

  delay 2;

  (* A *)
  draw_box 100 350;
  draw_box 100 360;
  draw_box 100 370;
  draw_box 100 380;
  draw_box 100 390;
  draw_box 100 400;
  draw_box 100 410;
  draw_box 100 420;

  draw_box 110 430;
  draw_box 120 430;
  draw_box 130 430;

  draw_box 140 350;
  draw_box 140 360;
  draw_box 140 370;
  draw_box 140 380;
  draw_box 140 390;
  draw_box 140 400;
  draw_box 140 410;

  draw_box 140 420;

  draw_box 110 390;
  draw_box 120 390;

  draw_box 130 390;

  delay 2;

  (* n *)
  draw_box 160 350;
  draw_box 160 360;
  draw_box 160 370;
  draw_box 160 380;
  draw_box 160 390;
  draw_box 160 400;
  draw_box 160 410;
  draw_box 160 420;
  draw_box 160 430;

  draw_box 170 430;
  draw_box 180 430;

  draw_box 190 430;

  draw_box 200 350;
  draw_box 200 360;
  draw_box 200 370;
  draw_box 200 380;
  draw_box 200 390;
  draw_box 200 400;
  draw_box 200 410;
  draw_box 200 420;

  delay 2;

  (* d *)
  draw_box 220 350;
  draw_box 220 360;
  draw_box 220 370;
  draw_box 220 380;
  draw_box 220 390;
  draw_box 220 400;
  draw_box 220 410;
  draw_box 220 420;
  draw_box 220 430;

  draw_box 230 430;
  draw_box 240 430;
  draw_box 250 430;

  draw_box 260 360;
  draw_box 260 370;
  draw_box 260 380;
  draw_box 260 390;
  draw_box 260 400;
  draw_box 260 410;
  draw_box 260 420;

  draw_box 230 350;
  draw_box 240 350;
  draw_box 250 350;

  delay 2;

  (* b *)
  draw_box 280 350;
  draw_box 280 360;
  draw_box 280 370;
  draw_box 280 380;
  draw_box 280 390;
  draw_box 280 400;
  draw_box 280 410;
  draw_box 280 420;
  draw_box 280 430;

  draw_box 290 430;
  draw_box 300 430;
  draw_box 310 430;

  draw_box 320 360;
  draw_box 320 370;
  draw_box 320 380;
  draw_box 320 400;
  draw_box 320 410;
  draw_box 320 420;

  draw_box 290 350;
  draw_box 300 350;
  draw_box 310 350;

  draw_box 290 390;
  draw_box 300 390;
  draw_box 310 390;

  delay 2;

  (* o *)
  draw_box 340 360;
  draw_box 340 370;
  draw_box 340 380;
  draw_box 340 390;
  draw_box 340 400;
  draw_box 340 410;
  draw_box 340 420;

  draw_box 350 430;
  draw_box 360 430;
  draw_box 370 430;

  draw_box 380 360;
  draw_box 380 370;
  draw_box 380 380;
  draw_box 380 390;
  draw_box 380 400;
  draw_box 380 410;
  draw_box 380 420;

  draw_box 350 350;
  draw_box 360 350;
  draw_box 370 350;

  delay 2;

  (* x *)
  draw_box 400 350;
  draw_box 400 360;
  draw_box 400 370;
  draw_box 410 380;
  draw_box 420 390;
  draw_box 410 400;
  draw_box 400 410;
  draw_box 400 420;
  draw_box 400 430;

  draw_box 440 350;
  draw_box 440 360;
  draw_box 440 370;
  draw_box 430 380;
  draw_box 420 390;
  draw_box 430 400;
  draw_box 440 410;
  draw_box 440 420;
  draw_box 440 430;

  delay 2

(** [draw_start_small_txt () ] draws the small text on the start screen. *)
let draw_start_small_txt () =
  delay 2;
  Graphics.set_color Graphics.black;
  Graphics.moveto 100 300;
  Graphics.draw_string "Powder Game is a particle simulator game with the";
  Graphics.moveto 100 275;
  Graphics.draw_string "ability to create things with the available";
  Graphics.moveto 100 250;
  Graphics.draw_string "elements. Powder Game has a number of unique";
  Graphics.moveto 100 225;
  Graphics.draw_string "elements, each with their own properties.";
  Graphics.moveto 175 150;
  Graphics.draw_string "Click anywhere to start"

(** [draw_menu ()] draws the bottom menu bar of the game. *)
let draw_menu () =
  let _ = Graphics.set_color Graphics.black in
  let _ = Graphics.fill_rect 0 0 500 100 in
  let _ = Graphics.set_color Graphics.white in
  let _ = Graphics.moveto 50 70 in
  let _ = Graphics.draw_string "material" in
  let _ = Graphics.moveto 200 70 in
  let _ = Graphics.draw_string "steel" in
  let _ = Graphics.moveto 300 70 in
  let _ = Graphics.draw_string "dirt" in
  let _ = Graphics.moveto 400 70 in
  let _ = Graphics.draw_string "water" in
  let _ = Graphics.moveto 50 20 in
  let _ = Graphics.draw_string "tools" in
  let _ = Graphics.moveto 200 20 in
  let _ = Graphics.draw_string "erase" in
  let _ = Graphics.moveto 300 20 in
  let _ = Graphics.draw_string "clear" in
  let _ = Graphics.moveto 400 20 in
  Graphics.draw_string "draw"

(** [color_int] is a variable used to help go through the color updating loop
    set by [color_loop]. *)
let color_int = ref 0

(** [color_flag] determines if the color is increasing or decreasing in
    saturation. 0 is becoming darker while 1 is becoming lighter. *)
let color_flag = ref 0

(** [current_color] is the current color used in [color_loop]. *)
let current_color = ref (204, 204, 204)

(** [flag_check ()] is a helper function that checks the direction of the loop
    and updates [color_int] accordingly. *)
let flag_check () : unit =
  if !color_flag = 0 then incr color_int else decr color_int

(** [color_loop ()] is responsible for the main color changing animation of the
    start text. *)
let color_loop () : int * int * int =
  flag_check ();
  match !color_int with
  | 0 ->
      let _ = color_flag := 0 in
      (204, 204, 204)
  | 1 -> (191, 191, 191)
  | 2 -> (179, 179, 179)
  | 3 -> (166, 166, 166)
  | 4 -> (153, 153, 153)
  | 5 -> (140, 140, 140)
  | 6 -> (128, 128, 128)
  | 7 -> (115, 115, 115)
  | 8 -> (102, 102, 102)
  | 9 -> (89, 89, 89)
  | 10 -> (77, 77, 77)
  | 11 -> (64, 64, 64)
  | 12 -> (51, 51, 51)
  | 13 -> (38, 38, 38)
  | 14 ->
      let _ = color_flag := 1 in
      (26, 26, 26)
  | _ -> (0, 0, 0)

(** [fst color] is a helper function to get the first element of a triple. *)
let fst (color : int * int * int) : int =
  match color with
  | a, b, c -> a

(** [snd color] is a helper function to get the second element of a triple. *)
let snd (color : int * int * int) : int =
  match color with
  | a, b, c -> b

(** [thd color] is a helper function to get the third element of a triple. *)
let thd (color : int * int * int) : int =
  match color with
  | a, b, c -> c

(** [button_wait ()] waits for a button to be pressed in order to progress to
    the next menu. This also flashes the start screen text. *)
let rec button_wait () =
  let color = color_loop () in
  let new_rgb = Graphics.rgb (fst color) (snd color) (thd color) in
  let _ = Graphics.set_color new_rgb in
  let _ = Graphics.moveto 175 150 in
  let _ = Graphics.draw_string "Click anywhere to start" in
  delay 2;
  if Graphics.button_down () then (
    Graphics.set_color Graphics.white;
    Graphics.fill_rect 0 0 500 500;
    draw_menu ())
  else button_wait ()

(** [draw_start_menu ()] draws the start screen items in order. *)
let draw_start_menu () =
  draw_start_title ();
  draw_start_small_txt ();
  button_wait ()
