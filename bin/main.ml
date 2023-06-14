open Game.Control
open Game.Menu

let () =
  let _ = Graphics.open_graph "" in
  let _ = Graphics.resize_window 500 500 in
  let _ = draw_start_menu () in
  (* graph only needs to open once before loop *)
  event_loop ()
