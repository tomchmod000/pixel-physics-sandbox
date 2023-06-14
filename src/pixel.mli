(** The Pixel module is responsible for all the logic regarding the creation,
    deletion, and movement of pixels. *)

(** Types of pixels present in game *)
type material =
  | Steel
  | Dirt
  | Water
  | Empty

type rgb = int * int * int
(** Represents (r, g, b) values of a color *)

type pixel = {
  material : material;
  mutable color : rgb;
  mutable coordinates : int * int;
  mutable step : int;
  step_change : int; (* step max changes based on surrounding material *)
}
(** Represents a pixel on the board *)

type board = pixel array array
(** Represents the 2D board of pixels in the game. Initialized to a matrix of
    Empty pixels. *)

val current_board : board
(** Current board used for all drawing / deleting *)

val move : pixel -> board -> pixel
(** [move p (x, y)] is a pixel with new intended coordinates and velocity. Uses
    the velocity of [p] to calculate new coordinates. *)

val rev_flush : board -> board
(** [flush canvas] is the new board after updating the position of each pixel
    within the canvas. *)

val map_mat_rgb : int * int -> material -> rgb
(** [map_mat_rgb mat] is the rgb value of [mat]. *)

val map_mat_step_change : material -> int
(** [map_mat_step_max mat] is the step_max value of that [mat] when surrounded
    by Empty pixels. *)

val create_pixel : material -> int * int -> pixel
(** [create_pixel mat] is a pixel created with material type [mat]. *)

val draw_pixel : pixel -> board -> board
(** [draw_pixel x coordinates matrix] is the board of pixels after [x] has been
    set at [coordinates] if the pixel at [coordinates] is an Empty pixel.
    Otherwise, do nothing. *)

val delete_pixel : int * int -> board -> board
(** [delete_pixel x coordinates matrix] is the board of pixels after the pixel
    at [coordinates] has been set to Empty. *)

val init_board : unit -> board
(** [init_board ()] is the board containing pixels with all pixels initialized
    to Empty. *)

val print_board : board -> unit
(** [print_board board] prints the current board out for debugging purposes.*)

val coord_convert : pixel -> string
(** [coord_convert pixel] takes a pixel and formats its coordinates into a
    printable string. *)

val string_convert : pixel -> string
(** [string_convert pixel] converts the material of a pixel to a printable
    string. *)
