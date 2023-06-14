(** The Menu module draws and animates the start and menu screens of the game. *)

val draw_start_menu : unit -> unit
(** [draw_start_menu ()] calls the functions to draw the start menu text. *)

val button_wait : unit -> unit
(** [button_wait ()] animates the start text and exits when the game window is
    clicked. *)

val draw_start_small_txt : unit -> unit
(** [draw_start_small_txt ()] draws the instructions on the start screen. *)

val draw_start_title : unit -> unit
(** [draw_start_title ()] draws the title text in the starting screen. *)

val draw_menu : unit -> unit
(** [draw_menu ()] draws the menu bar in the game window. *)

val delay : int -> unit
(** [delay (int)] pauses the program for a short period of time to smooth out
    animation transitions. *)

val draw_box : int -> int -> unit
(** [draw_box x y] draws a 10 by 10 box at the specified location, where x y are
    the coordinates of the bottom left corner. *)
