(** type material denotes the kind of substance that a pixel represents. *)
type material =
  | Steel (* doesn't move*)
  | Dirt (* moves down unless affected by other things *)
  | Water
    (* moves down to bottom, then move left or right randomly depending on
       space *)
  | Empty

type rgb = int * int * int
(** type rgb is the type used to represent the colors of our materials in the
    game graphics. *)

type pixel = {
  material : material;
  mutable color : rgb;
  mutable coordinates : int * int;
  mutable step : int;
  step_change : int;
}
(** type pixel represents a pixel in the canvas of our UI. It has a field for
    its material, color, coordinates, step, and step change. Step refers to the
    update speed of a material. *)

type board = pixel array array
(** type board is the representation type of our canvas. It represents a 2D
    coordinate system. *)

(** [mat_mat_rgb coordinates mat] returns a rgb value that corresponds to the
    material passed in mat. The color also depends on where on the screen the
    material was generated. *)
let map_mat_rgb (coords : int * int) (mat : material) : rgb =
  let y = snd coords in
  if y < 10 then
    match mat with
    | Steel -> (140, 140, 140)
    | Dirt -> (77, 38, 0)
    | Water -> (26, 163, 255)
    | Empty -> (255, 255, 255)
  else if y < 20 then
    match mat with
    | Steel -> (153, 153, 153)
    | Dirt -> (102, 51, 0)
    | Water -> (51, 173, 255)
    | Empty -> (255, 255, 255)
  else if y < 30 then
    match mat with
    | Steel -> (166, 166, 166)
    | Dirt -> (128, 64, 0)
    | Water -> (77, 184, 255)
    | Empty -> (255, 255, 255)
  else if y < 40 then
    match mat with
    | Steel -> (179, 179, 179)
    | Dirt -> (153, 77, 0)
    | Water -> (102, 194, 255)
    | Empty -> (255, 255, 255)
  else if y < 50 then
    match mat with
    | Steel -> (191, 191, 191)
    | Dirt -> (179, 89, 0)
    | Water -> (128, 204, 255)
    | Empty -> (255, 255, 255)
  else if y < 60 then
    match mat with
    | Steel -> (204, 204, 204)
    | Dirt -> (204, 102, 0)
    | Water -> (153, 214, 255)
    | Empty -> (255, 255, 255)
  else if y < 70 then
    match mat with
    | Steel -> (217, 217, 217)
    | Dirt -> (230, 115, 0)
    | Water -> (179, 224, 255)
    | Empty -> (255, 255, 255)
  else
    match mat with
    | Steel -> (230, 230, 230)
    | Dirt -> (255, 128, 0)
    | Water -> (204, 235, 255)
    | Empty -> (255, 255, 255)

(** [map_mat_step_change mat] returns the int value corresponding to the
    material passed by mat. The int value represents the step value of each
    material that will make a pixel move when its step value reaches it. *)
let map_mat_step_change (mat : material) : int =
  match mat with
  | Steel -> -1 (* Doesn't move *)
  | Dirt -> 4 (* Moves a pixel every 4 frames *)
  | Water -> 0 (* Moves a pixel every frame *)
  | Empty -> -1 (* Doesn't move *)

(** [create_pixel mat coords] creates a pixel of a material mat and gives it
    coordinates passed by coords. *)
let create_pixel (mat : material) (coords : int * int) : pixel =
  let step_m = map_mat_step_change mat in
  {
    material = mat;
    color = map_mat_rgb coords mat;
    coordinates = (fst coords, snd coords);
    step = step_m;
    step_change = step_m;
  }

(** [draw_pixel p matrix] inserts a pixel into its place in the canvas matrix
    placing it at the coordinates denoted in the type of p. *)
let draw_pixel (p : pixel) (matrix : board) =
  let x, y = p.coordinates in
  let _ = p.coordinates <- (x, y) in
  match matrix.(x).(y).material with
  | _ ->
      let _ = matrix.(x).(y) <- p in
      matrix

(** [delete_pixel coords matrix] makes the pixel at coordinates coords Empty. *)
let delete_pixel (coordinates : int * int) (matrix : board) =
  let first = fst coordinates in
  let second = snd coordinates in
  let empty_x = create_pixel Empty (first, second) in
  match matrix.(first).(second).material with
  | _ ->
      let _ = matrix.(first).(second) <- empty_x in
      matrix

(** [init_board ()] creates a board of 100x80, thses dimensions are 5 times
    smaller than our UI screen allowing for each pixel to be 5x5 pixels on the
    monitor. *)
let init_board () : board = Array.make_matrix 100 80 (create_pixel Empty (0, 0))

(** [current_board] is a global variable holding the board being shown by the
    UI. *)
let current_board = init_board ()

(** [inc_step p x] increments the step of pixel p by the amount given by x. *)
let inc_step p x = p.step <- p.step + x

(** [in_water p c] returns true if the pixel passed has water to its left and
    right in the canvas passed, and false otherwise. *)
let in_water p canvas : bool =
  let x, y = p.coordinates in

  if
    (x > 0 && canvas.(x - 1).(y).material = Water)
    && x < 100
    && canvas.(x + 1).(y).material = Water
  then true
  else false

(** [edge_dirt p canvas x y] handles the boundary pixels of dirt piling. It
    takes in a pixel at location (x,y) and handles its piling logic near the
    side of the board. returns the pixel with its new coordinates. *)
let edge_dirt p canvas x y =
  match x with
  | 0 ->
      if
        canvas.(x + 1).(y).material = Dirt
        || canvas.(x + 1).(y).material = Steel
        || canvas.(x + 1).(y - 1).material = Dirt
        || canvas.(x + 1).(y - 1).material = Steel
      then p
      else (
        p.coordinates <- (x + 1, y);
        p)
  | 99 ->
      if
        canvas.(x - 1).(y).material = Dirt
        || canvas.(x - 1).(y).material = Steel
        || canvas.(x - 1).(y - 1).material = Dirt
        || canvas.(x - 1).(y - 1).material = Steel
      then p
      else (
        p.coordinates <- (x - 1, y);
        p)
  | _ -> failwith "Not an edge case or out of bounds"

(** [obs_dirt p canvas] handles the move logic of dirt pixel p in the board
    canvas when the dirt is obstructed by a material below it that effects its
    ability to fall. Returns the pixel with new coordinates based on pixel
    interaction logic. *)
let obs_dirt p (canvas : pixel array array) : pixel =
  let x, y = p.coordinates in
  if x = 0 || x = 99 then edge_dirt p canvas x y
  else
    let sl = canvas.(x - 1).(y).material in
    let sr = canvas.(x + 1).(y).material in
    let dl = canvas.(x - 1).(y - 1).material in
    let dr = canvas.(x + 1).(y - 1).material in
    if
      (dr = Empty || dr = Water)
      && (dl = Empty || dl = Water)
      && (sr = Empty || sr = Water)
      && (sl = Empty || sl = Water)
    then
      let rand = Random.int 4 in
      if rand = 0 then (
        p.coordinates <- (x - 1, y);
        p)
      else if rand = 1 then (
        p.coordinates <- (x + 1, y);
        p)
      else p
    else if
      (dr = Empty || dr = Water)
      && (sl = Empty || sl = Water)
      && (sr = Empty || sr = Water)
    then
      let rand = Random.int 4 in
      if rand = 0 then (
        p.coordinates <- (x + 1, y);
        p)
      else p
    else if
      (dl = Empty || dl = Water)
      && (sl = Empty || sl = Water)
      && (sr = Empty || sr = Water)
    then
      let rand = Random.int 4 in
      if rand = 0 then (
        p.coordinates <- (x - 1, y);
        p)
      else p
    else if (dl = Empty || dl = Water) && (sl = Empty || sl = Water) then
      let rand = Random.int 4 in
      if rand = 0 then (
        p.coordinates <- (x - 1, y);
        p)
      else p
    else if (sr = Empty || sr = Water) && (dr = Empty || dr = Water) then
      let rand = Random.int 4 in
      if rand = 0 then (
        p.coordinates <- (x + 1, y);
        p)
      else p
    else p

(** [move_dirt p canvas] moves a dirt pixel returning the pixel with mutated
    coordinates based on the move logic of our game. Does not move if already on
    bottom of the canvas. *)
let move_dirt p (canvas : board) : pixel =
  let x, y = p.coordinates in
  if
    y != 0
    && (canvas.(x).(y - 1).material = Empty
       || canvas.(x).(y - 1).material = Water)
  then (
    (* let _ = canvas.(x).(y) = create_pixel Empty (x, y) in *)
    p.coordinates <- (x, y - 1);
    p)
  else if y = 0 then p
  else obs_dirt p canvas

(** [obs_water p canvas] handles the move logic of a water particle when it has
    encountered an obstruction to its natural downward movement. Returns the
    pixel with mutated coordinates based on pixel interaction rules. *)
let obs_water p canvas : pixel =
  let x, y = p.coordinates in
  if
    (x = 0
    || canvas.(x - 1).(y).material = Steel
    || canvas.(x - 1).(y).material = Dirt
    || canvas.(x - 1).(y).material = Water)
    && (x = 99
       || canvas.(x + 1).(y).material = Steel
       || canvas.(x + 1).(y).material = Dirt
       || canvas.(x + 1).(y).material = Water)
  then p
  else if
    x = 0
    || canvas.(x - 1).(y).material = Steel
    || canvas.(x - 1).(y).material = Dirt
    || canvas.(x - 1).(y).material = Water
  then (
    p.coordinates <- (x + 1, y);
    p)
  else if
    x = 99
    || canvas.(x + 1).(y).material = Steel
    || canvas.(x + 1).(y).material = Dirt
    || canvas.(x + 1).(y).material = Water
  then
    let rand = Random.int 2 in
    if rand = 0 then (
      p.coordinates <- (x - 1, y);
      p)
    else p
  else
    let rand = Random.int 2 in
    if rand = 0 then (
      p.coordinates <- (x + 1, y);
      p)
    else (
      p.coordinates <- (x - 1, y);
      p)

(** [move_water p canvas] moves a water pixel. Returns the pixel with new
    coordinates based on pixel interaction rules. *)
let move_water p canvas : pixel =
  let x, y = p.coordinates in
  if y != 0 && canvas.(x).(y - 1).material = Empty then (
    p.coordinates <- (x, y - 1);
    p)
  else obs_water p canvas

(** [move p canvas] matches a pixels material to move the pixel. Returns the
    pixel with mutated coordinates if it needs to move based on move logic ancd
    pixel interactions. *)
let move (p : pixel) (canvas : board) =
  match p.material with
  | Steel -> p (* doesn't move*)
  | Dirt ->
      (* moves down unless affected by other things (water slows it, steel stops
         it) *)
      if p.step >= p.step_change then
        let _ = p.step <- 1 in
        move_dirt p canvas
      else if in_water p canvas then
        let _ = inc_step p 1 in
        p
      else
        let _ = inc_step p 2 in
        p
  | Water -> move_water p canvas
  (* moves down to bottom, then move left or right randomly depending on
     space *)
  | Empty -> p (* doesn't move*)

(** [update_color pixel] updates the pixel color based on its current location. *)
let update_color (p : pixel) =
  let coords = p.coordinates in
  let mat = p.material in
  let new_color = map_mat_rgb coords mat in
  p.color <- new_color

(** [rev_array ary] is a helper function that reverses the array ary that is
    passed to it *)
let rev_array (ary : pixel array) : pixel array =
  let length = Array.length ary in
  let rev = Array.make length (create_pixel Empty (0, 0)) in
  Array.iteri (fun i row -> rev.(length - i - 1) <- ary.(i)) ary;
  rev

(** [rev_flush canvas] iterates through the canvas calling move on each pixel
    and updating the canvas as it goes. The iteration goes from bottom to top in
    order to avoid collisions in the move logic. Returns the updated board *)
let rev_flush (canvas : board) =
  Array.iter
    (fun col ->
      Array.iter
        (fun pixel ->
          let ox, oy = pixel.coordinates in
          let new_pixel = move pixel canvas in
          (* let _ = update_color new_pixel in *)
          let nx, ny = new_pixel.coordinates in
          if ox = nx && oy = ny then ()
          else
            let _ = delete_pixel (ox, oy) canvas in
            canvas.(nx).(ny) <- new_pixel)
        (rev_array col))
    canvas;
  canvas

(** [coord_convert pixel] takes a pixel and formats its coordinates into a
    printable string. *)
let coord_convert (p : pixel) : string =
  match p.coordinates with
  | a, b -> "x = " ^ string_of_int a ^ " y = " ^ string_of_int b

(** [string_convert pixel] converts the material of a pixel to a printable
    string. *)
let string_convert (p : pixel) : string =
  match p.material with
  | Steel -> "steel " ^ coord_convert p
  | Dirt -> "dirt " ^ coord_convert p
  | Water -> "water " ^ coord_convert p
  | Empty -> ""

(** [print_pixel pixel] converts a pixel to a string. *)
let print_pixel (p : pixel) = print_string (string_convert p)

(** [print_row (pixel array)] prints a row of pixels of the board. *)
let print_row (r : pixel array) : unit =
  let _ = Array.iter print_pixel r in
  print_string ""

(** [print_board board] prints the entire board of pixels, ignoring Empty ones.*)
let print_board (b : board) = Array.iter print_row b
