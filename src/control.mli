(** The Control module handles the main gameplay event loops and user
    interaction. *)

val mouse_control : unit -> unit
(** [mouse_control ()] is the main control loop for user inputs. *)

val mouse_pos_to_index : Pixel.board -> Pixel.material -> Pixel.board
(** [mouse_pos_to_index board material] maps mouse position on game window to
    coordinates on pixel board. *)

val event_loop : unit -> unit
(** [event_loop ()] is the main backend loop controlling all logic in game. *)

val current_material : Pixel.material ref
(** [current_material] is a global variable that is used to represent what the
    users currently drawing with. This is initalized to Dirt. *)

val current_matrix : Pixel.board ref
(** [current_matrix] is the main board used in update loops. *)

val update_board : unit -> unit
(** [update_board ()] updates the current board with the new positions of all
    the pixels. *)

val current_action : int ref
(** [current_action] represents what action the user is currently able to do
    with their mouse: 0 = delete, 1 = clear, 2 = draw. It is initialized to
    draw. *)
