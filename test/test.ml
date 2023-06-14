open OUnit2
open Game
open Game.Control
open Game.Pixel

(* Start of Test Plan *)

(* Due to the nature of our game (lots of refs, random numbers, visual elements,
   etc) we had to approach testing from many directions.

   Most of the Pixel module was tested using OUnit unit tests as that was where
   all the critical movement logic was. We used glass-box testing for those
   functions because it would be hard to figure out edge cases from a black-box
   testing standpoint. Helper functions were tested via the functions specified
   in the mli. In order to test mutable Pixel functions, we needed to make new
   variables for each test case to prevent test cases from interefering with
   each other. For functions that had random aspects, we tested using our game
   interface, by placing one pixel and observing its movement, as well as
   looking at the output of the printed board on the terminal. We also tested
   some of these by creating possible outcomes and using those as our test
   variables, though it does result in some of our tests occasionally failing.

   The Menu module was tested visually by seeing how the display looked when the
   game was run. For anything visual, information was printed to the terminal in
   order to have data on what was happening on the screen. This allowed us to
   make sure our Menu function loops were running smoothly.

   Because the Control module only has a little bit of logic on top of function
   calls to the Pixel module, we did not see the need to unit test Control, as
   all relevant functions would have already been tested in Pixel. However, we
   did use print statements and manual playtesting to make sure control worked
   as intended.

   This combination of unit testing and human playtesting assures system
   correctness as we have tested the underlying logic of the system while also
   making sure that the player experience is smooth. *)

(* End of Test Plan *)

(* Test Variables *)
let board = init_board ()

let steel_pixel_0_0 : pixel =
  {
    material = Steel;
    color = map_mat_rgb (0, 0) Steel;
    coordinates = (0, 0);
    step = map_mat_step_change Steel;
    step_change = map_mat_step_change Steel;
  }

let dirt_pixel_0_0 : pixel =
  {
    material = Dirt;
    color = map_mat_rgb (0, 0) Dirt;
    coordinates = (0, 0);
    step = map_mat_step_change Dirt;
    step_change = map_mat_step_change Dirt;
  }

let water_pixel_0_0 : pixel =
  {
    material = Water;
    color = map_mat_rgb (0, 0) Water;
    coordinates = (0, 0);
    step = map_mat_step_change Water;
    step_change = map_mat_step_change Water;
  }

let empty_pixel_0_0 : pixel =
  {
    material = Empty;
    color = map_mat_rgb (0, 0) Empty;
    coordinates = (0, 0);
    step = map_mat_step_change Empty;
    step_change = map_mat_step_change Empty;
  }

let empty_pixel_1_1 : pixel =
  {
    material = Empty;
    color = map_mat_rgb (1, 1) Empty;
    coordinates = (1, 1);
    step = map_mat_step_change Empty;
    step_change = map_mat_step_change Empty;
  }

let steel_pixel_43_23 : pixel =
  {
    material = Steel;
    color = map_mat_rgb (43, 23) Steel;
    coordinates = (43, 23);
    step = map_mat_step_change Steel;
    step_change = map_mat_step_change Steel;
  }

let steel_pixel_99_79 : pixel =
  {
    material = Steel;
    color = map_mat_rgb (99, 79) Steel;
    coordinates = (99, 79);
    step = map_mat_step_change Steel;
    step_change = map_mat_step_change Steel;
  }

let dirt_pixel_1_1 : pixel =
  {
    material = Dirt;
    color = map_mat_rgb (1, 1) Dirt;
    coordinates = (1, 1);
    step = map_mat_step_change Dirt;
    step_change = map_mat_step_change Dirt;
  }

let water_pixel_2_1 : pixel =
  {
    material = Water;
    color = map_mat_rgb (2, 1) Water;
    coordinates = (2, 1);
    step = map_mat_step_change Water;
    step_change = map_mat_step_change Water;
  }

let empty_pixel_32_22 =
  {
    material = Empty;
    color = map_mat_rgb (32, 22) Empty;
    coordinates = (32, 22);
    step = map_mat_step_change Empty;
    step_change = map_mat_step_change Empty;
  }

let empty_board = init_board ()
let steel_board = draw_pixel steel_pixel_0_0 (init_board ())

let single_board =
  let init = init_board () in
  draw_pixel steel_pixel_0_0 init

let single_board2 = init_board () |> draw_pixel steel_pixel_0_0
let double_board = init_board ()
let _ = double_board.(2).(1) <- water_pixel_2_1
let _ = double_board.(0).(0) <- steel_pixel_0_0

let double_board_2 =
  init_board () |> draw_pixel steel_pixel_0_0 |> draw_pixel water_pixel_2_1

let _ = delete_pixel (2, 1) double_board_2

let double_board_3 =
  init_board () |> draw_pixel steel_pixel_0_0 |> draw_pixel water_pixel_2_1

let init_board_empty = Array.make_matrix 100 80 (create_pixel Empty (0, 0))
let delete_board_1 = init_board ()
let delete_board_2 = init_board ()
let _ = delete_board_2.(0).(0) <- empty_pixel_0_0
let delete_board_3 = init_board ()
let _ = delete_board_3.(2).(1) <- water_pixel_2_1
let _ = delete_board_3.(2).(1) <- create_pixel Empty (2, 1)
let delete_board_4 = init_board ()
let _ = delete_board_4.(2).(1) <- water_pixel_2_1
let delete_board_5 = init_board ()
let _ = delete_board_5.(0).(0) <- empty_pixel_0_0
let _ = delete_board_5.(2).(1) <- water_pixel_2_1
let _ = delete_board_5.(2).(1) <- create_pixel Empty (2, 1)
let delete_board_6 = init_board ()
let _ = delete_board_6.(0).(0) <- empty_pixel_0_0
let _ = delete_board_6.(2).(1) <- water_pixel_2_1
let delete_board_7 = init_board ()
let _ = delete_board_7.(1).(1) <- empty_pixel_1_1
let delete_board_8 = init_board ()
let _ = delete_board_8.(1).(1) <- dirt_pixel_1_1

(* Helper Test Functions *)

let board_equal_test (name : string) (board1 : board) (board2 : board) : test =
  name >:: fun _ -> assert_equal board1 board2

let create_pixel_test (name : string) (mat : material) (coordinates : int * int)
    (expected_output : pixel) : test =
  name >:: fun _ -> assert_equal expected_output (create_pixel mat coordinates)

let flush_test (name : string) (board : board) (expected_output : board) : test
    =
  name >:: fun _ -> assert_equal expected_output (rev_flush board)

let delete_pixel_test (name : string) (coordinates : int * int) (matrix : board)
    (expected_output : board) =
  name >:: fun _ ->
  assert_equal expected_output (delete_pixel coordinates board)

let move_test (name : string) (x : pixel) (board : board)
    (expected_output : pixel) : test =
  name >:: fun _ -> assert_equal expected_output (move x board)

let draw_pixel_test (name : string) (x : pixel) (coordinates : int * int)
    (matrix : board) (expected_output : board) : test =
  name >:: fun _ -> assert_equal expected_output (draw_pixel x matrix)

let init_board_test (name : string) (expected_output : board) : test =
  name >:: fun _ -> assert_equal expected_output (init_board ())

let map_mat_rgb_test (name : string) (coords : int * int) (mat : material)
    (expected_output : rgb) : test =
  name >:: fun _ -> assert_equal expected_output (map_mat_rgb coords mat)

let coord_convert_test (name : string) (x : pixel) (expected_output : string) :
    test =
  name >:: fun _ -> assert_equal expected_output (coord_convert x)

let string_convert_test (name : string) (x : pixel) (expected_output : string) :
    test =
  name >:: fun _ -> assert_equal expected_output (string_convert x)

(* Tests *)

let create_pixel_tests =
  [
    create_pixel_test "Create Steel Pixel" Steel (0, 0) steel_pixel_0_0;
    create_pixel_test "Create Sand Pixel" Dirt (1, 1) dirt_pixel_1_1;
    create_pixel_test "Create Water Pixel" Water (2, 1) water_pixel_2_1;
    create_pixel_test "Create Empty Pixel" Empty (32, 22) empty_pixel_32_22;
    create_pixel_test "Create Steel Pixel (43,23)" Steel (43, 23)
      steel_pixel_43_23;
    create_pixel_test "Create Steel Pixel bounds" Steel (99, 79)
      steel_pixel_99_79;
  ]

let empty_board_1 = init_board ()
let empty_board_2 = init_board ()
let _ = empty_board_2.(0).(0) <- empty_pixel_0_0
let empty_board_3 = init_board ()
let _ = empty_board_3.(0).(0) <- empty_pixel_0_0
let _ = empty_board_3.(2).(1) <- water_pixel_2_1
let empty_board_4 = init_board ()
let _ = empty_board_4.(0).(0) <- dirt_pixel_0_0
let empty_board_5 = init_board ()
let empty_board_6 = init_board ()
let _ = empty_board_6.(0).(0) <- steel_pixel_0_0
let empty_board_7 = init_board ()
let empty_board_8 = init_board ()
let _ = empty_board_8.(1).(1) <- dirt_pixel_1_1

let draw_pixel_tests =
  [
    draw_pixel_test "draw single pixel" empty_pixel_0_0 (0, 0) empty_board_1
      empty_board_2;
    draw_pixel_test "draw two pixels" water_pixel_2_1 (2, 1) empty_board_2
      empty_board_3;
    draw_pixel_test "draw over empty pixel" water_pixel_0_0 (0, 0) empty_board_2
      empty_board_2;
    draw_pixel_test "draw over occupied pixel" water_pixel_0_0 (0, 0)
      empty_board_4 empty_board_4;
    draw_pixel_test "draw steel pixel" steel_pixel_0_0 (0, 0) empty_board_5
      empty_board_6;
    draw_pixel_test "draw dirt pixel" dirt_pixel_1_1 (1, 1) empty_board_7
      empty_board_8;
  ]

let delete_pixel_tests =
  [
    delete_pixel_test "Single pixel board" (0, 0) single_board empty_board;
    delete_pixel_test "Empty board" (0, 0) empty_board empty_board;
    delete_pixel_test "Delete again" (0, 0) delete_board_2 delete_board_1;
    delete_pixel_test "Double pixel board 1" (2, 1) delete_board_4
      delete_board_3;
    delete_pixel_test "Double pixel board 2" (2, 1) delete_board_6
      delete_board_5;
  ]

let init_board_tests = [ init_board_test "Init board" init_board_empty ]
let move_board1 = init_board ()

let water_pixel_0_1 : pixel =
  {
    material = Water;
    color = map_mat_rgb (0, 1) Water;
    coordinates = (0, 1);
    step = map_mat_step_change Water;
    step_change = map_mat_step_change Water;
  }

let water_pixel_0_1_moved : pixel =
  {
    material = Water;
    color = map_mat_rgb (0, 0) Water;
    coordinates = (0, 0);
    step = map_mat_step_change Water;
    step_change = map_mat_step_change Water;
  }

let _ = move_board1.(0).(1) <- water_pixel_0_1
let move_board2 = init_board ()

let dirt_pixel_0_1 : pixel =
  {
    material = Dirt;
    color = map_mat_rgb (0, 1) Dirt;
    coordinates = (0, 1);
    step = map_mat_step_change Dirt;
    step_change = map_mat_step_change Dirt;
  }

let dirt_pixel_0_1_moved : pixel =
  {
    material = Dirt;
    color = map_mat_rgb (0, 0) Dirt;
    coordinates = (0, 0);
    step = 1;
    step_change = map_mat_step_change Dirt;
  }

let _ = move_board2.(0).(1) <- dirt_pixel_0_1
let move_board3 = init_board ()

let steel_pixel_0_1 : pixel =
  {
    material = Steel;
    color = map_mat_rgb (0, 1) Steel;
    coordinates = (0, 1);
    step = map_mat_step_change Steel;
    step_change = map_mat_step_change Steel;
  }

let steel_pixel_0_1_moved : pixel =
  {
    material = Steel;
    color = map_mat_rgb (0, 1) Steel;
    coordinates = (0, 1);
    step = map_mat_step_change Steel;
    step_change = map_mat_step_change Steel;
  }

let _ = move_board3.(0).(1) <- steel_pixel_0_1
let move_board4 = init_board ()

let dirt_pixel_0_0_move : pixel =
  {
    material = Dirt;
    color = map_mat_rgb (0, 0) Dirt;
    coordinates = (0, 0);
    step = map_mat_step_change Dirt;
    step_change = map_mat_step_change Dirt;
  }

let dirt_pixel_0_0_moved : pixel =
  {
    material = Dirt;
    color = map_mat_rgb (0, 0) Dirt;
    coordinates = (0, 0);
    step = 1;
    step_change = map_mat_step_change Dirt;
  }

let _ = move_board4.(0).(0) <- dirt_pixel_0_0_move
let move_board5 = init_board ()

let steel_pixel_0_0_move : pixel =
  {
    material = Steel;
    color = map_mat_rgb (0, 0) Steel;
    coordinates = (0, 0);
    step = map_mat_step_change Steel;
    step_change = map_mat_step_change Steel;
  }

let dirt_pixel_0_1_unmoved : pixel =
  {
    material = Dirt;
    color = map_mat_rgb (0, 1) Dirt;
    coordinates = (0, 1);
    step = 1;
    step_change = map_mat_step_change Dirt;
  }

let dirt_pixel_0_1_possible : pixel =
  {
    material = Dirt;
    color = map_mat_rgb (1, 1) Dirt;
    coordinates = (1, 1);
    step = 1;
    step_change = map_mat_step_change Dirt;
  }

let dirt_pixel_0_1_possible_moved : pixel =
  {
    material = Dirt;
    color = map_mat_rgb (1, 1) Dirt;
    coordinates = (1, 1);
    step = 3;
    step_change = map_mat_step_change Dirt;
  }

let dirt_pixel_0_1_possible_moved_again : pixel =
  {
    material = Dirt;
    color = map_mat_rgb (1, 1) Dirt;
    coordinates = (1, 1);
    step = 5;
    step_change = map_mat_step_change Dirt;
  }

let dirt_pixel_0_1_fall : pixel =
  {
    material = Dirt;
    color = map_mat_rgb (1, 0) Dirt;
    coordinates = (1, 0);
    step = 1;
    step_change = map_mat_step_change Dirt;
  }

let dirt_pixel_0_1_fall_next : pixel =
  {
    material = Dirt;
    color = map_mat_rgb (1, 0) Dirt;
    coordinates = (1, 0);
    step = 3;
    step_change = map_mat_step_change Dirt;
  }

let dirt_pixel_0_1_fall_wait : pixel =
  {
    material = Dirt;
    color = map_mat_rgb (1, 0) Dirt;
    coordinates = (1, 0);
    step = 5;
    step_change = map_mat_step_change Dirt;
  }

let _ = move_board5.(0).(1) <- dirt_pixel_0_1
let _ = move_board5.(0).(0) <- steel_pixel_0_0_move
let move_board6 = init_board ()

let water_pixel_0_0_unmoved : pixel =
  {
    material = Water;
    color = map_mat_rgb (0, 0) Water;
    coordinates = (0, 0);
    step = map_mat_step_change Water;
    step_change = map_mat_step_change Water;
  }

let water_pixel_0_0_possible_1 : pixel =
  {
    material = Water;
    color = map_mat_rgb (1, 0) Water;
    coordinates = (1, 0);
    step = map_mat_step_change Water;
    step_change = map_mat_step_change Water;
  }

let water_pixel_0_0_possible_2 : pixel =
  {
    material = Water;
    color = map_mat_rgb (0, 0) Water;
    coordinates = (0, 0);
    step = map_mat_step_change Water;
    step_change = map_mat_step_change Water;
  }

let water_pixel_0_0_possible_3 : pixel =
  {
    material = Water;
    color = map_mat_rgb (2, 0) Water;
    coordinates = (2, 0);
    step = map_mat_step_change Water;
    step_change = map_mat_step_change Water;
  }

let _ = move_board6.(0).(0) <- water_pixel_0_0_unmoved

let move_tests =
  [
    move_test "move water down" water_pixel_0_1 move_board1
      water_pixel_0_1_moved;
    move_test "move dirt down" dirt_pixel_0_1 move_board2 dirt_pixel_0_1_moved;
    move_test "dirt at bottom" dirt_pixel_0_0_move move_board4
      dirt_pixel_0_0_moved;
    move_test "move steel " steel_pixel_0_1 move_board3 steel_pixel_0_1_moved;
    (* move_test "dirt stays on top of steel" dirt_pixel_0_1 move_board5
       dirt_pixel_0_1_unmoved; *)
    move_test "CAN FAIL DUE TO RANDOMNESS dirt moves off of steel"
      dirt_pixel_0_1 move_board5 dirt_pixel_0_1_possible;
    move_test "dirt moves down after" dirt_pixel_0_1_possible move_board5
      dirt_pixel_0_1_possible_moved;
    move_test "dirt delay" dirt_pixel_0_1_possible_moved move_board5
      dirt_pixel_0_1_possible_moved_again;
    move_test "dirt finally falls" dirt_pixel_0_1_possible_moved_again
      move_board5 dirt_pixel_0_1_fall;
    move_test "dirt stops moving" dirt_pixel_0_1_fall move_board5
      dirt_pixel_0_1_fall_next;
    move_test "dirt delay again" dirt_pixel_0_1_fall_next move_board5
      dirt_pixel_0_1_fall_wait;
    move_test "water in corner" water_pixel_0_0_unmoved move_board6
      water_pixel_0_0_possible_1;
    move_test "CAN FAIL DUE TO RANDOMNESS water in corner again"
      water_pixel_0_0_possible_1 move_board6 water_pixel_0_0_possible_3;
  ]

let dirt_pixel_0_1_flush : pixel =
  {
    material = Dirt;
    color = map_mat_rgb (0, 1) Dirt;
    coordinates = (0, 1);
    step = map_mat_step_change Dirt;
    step_change = map_mat_step_change Dirt;
  }

let dirt_pixel_0_1_flush_moved : pixel =
  {
    material = Dirt;
    color = map_mat_rgb (0, 1) Dirt;
    coordinates = (0, 1);
    step = 1;
    step_change = map_mat_step_change Dirt;
  }

let flush_board1 = init_board ()
let _ = flush_board1.(0).(0) <- dirt_pixel_0_0
let flush_board2 = init_board ()
let _ = flush_board2.(0).(1) <- dirt_pixel_0_1_flush

let rev_flush_tests =
  [
    flush_test "dirt on bottom" flush_board1 flush_board1;
    flush_test "flush again" flush_board1 flush_board1;
    flush_test "Empty Board" empty_board empty_board;
    flush_test "Steel board" steel_board steel_board;
  ]

let map_mat_rgb_tests =
  [
    map_mat_rgb_test "bottom steel" (10, 10) Steel (153, 153, 153);
    map_mat_rgb_test "middle steel" (50, 50) Steel (204, 204, 204);
    map_mat_rgb_test "middle edge steel" (50, 49) Steel (191, 191, 191);
    map_mat_rgb_test "top steel" (50, 75) Steel (230, 230, 230);
    map_mat_rgb_test "top water" (50, 75) Water (204, 235, 255);
    map_mat_rgb_test "middle water" (50, 50) Water (153, 214, 255);
    map_mat_rgb_test "bottom water" (0, 0) Water (26, 163, 255);
    map_mat_rgb_test "bottom dirt" (20, 20) Dirt (128, 64, 0);
    map_mat_rgb_test "top dirt" (50, 79) Dirt (255, 128, 0);
    map_mat_rgb_test "empty1" (15, 60) Empty (255, 255, 255);
    map_mat_rgb_test "empty2" (40, 59) Empty (255, 255, 255);
  ]

let cc_empty_pixel_0_0 : pixel =
  {
    material = Empty;
    color = map_mat_rgb (0, 0) Empty;
    coordinates = (0, 0);
    step = map_mat_step_change Empty;
    step_change = map_mat_step_change Empty;
  }

let cc_water_pixel_0_0 : pixel =
  {
    material = Water;
    color = map_mat_rgb (0, 0) Water;
    coordinates = (0, 0);
    step = map_mat_step_change Water;
    step_change = map_mat_step_change Water;
  }

let cc_dirt_pixel_0_0 : pixel =
  {
    material = Dirt;
    color = map_mat_rgb (0, 0) Dirt;
    coordinates = (0, 0);
    step = map_mat_step_change Dirt;
    step_change = map_mat_step_change Dirt;
  }

let cc_steel_pixel_0_0 : pixel =
  {
    material = Steel;
    color = map_mat_rgb (0, 0) Steel;
    coordinates = (0, 0);
    step = map_mat_step_change Steel;
    step_change = map_mat_step_change Steel;
  }

let cc_empty_pixel_32_22 =
  {
    material = Empty;
    color = map_mat_rgb (32, 22) Empty;
    coordinates = (32, 22);
    step = map_mat_step_change Empty;
    step_change = map_mat_step_change Empty;
  }

let cc_water_pixel_2_1 : pixel =
  {
    material = Water;
    color = map_mat_rgb (2, 1) Water;
    coordinates = (2, 1);
    step = map_mat_step_change Water;
    step_change = map_mat_step_change Water;
  }

let cc_dirt_pixel_0_1_fall_wait : pixel =
  {
    material = Dirt;
    color = map_mat_rgb (1, 0) Dirt;
    coordinates = (1, 0);
    step = 5;
    step_change = map_mat_step_change Dirt;
  }

let cc_empty_pixel_99_79 : pixel =
  {
    material = Empty;
    color = map_mat_rgb (99, 79) Empty;
    coordinates = (99, 79);
    step = map_mat_step_change Empty;
    step_change = map_mat_step_change Empty;
  }

let coord_convert_tests =
  [
    coord_convert_test "empty (0,0)" cc_empty_pixel_0_0 "x = 0 y = 0";
    coord_convert_test "water (0,0)" cc_water_pixel_0_0 "x = 0 y = 0";
    coord_convert_test "dirt (0,0)" cc_dirt_pixel_0_0 "x = 0 y = 0";
    coord_convert_test "steel (0,0)" cc_steel_pixel_0_0 "x = 0 y = 0";
    coord_convert_test "empty (32,22)" cc_empty_pixel_32_22 "x = 32 y = 22";
    coord_convert_test "water (2,1)" cc_water_pixel_2_1 "x = 2 y = 1";
    coord_convert_test "dirt fall wait (1,0)" cc_dirt_pixel_0_1_fall_wait
      "x = 1 y = 0";
    coord_convert_test "Empty (99,79)" cc_empty_pixel_99_79 "x = 99 y = 79";
  ]

let string_convert_tests =
  [
    string_convert_test "empty (0,0)" cc_empty_pixel_0_0 "";
    string_convert_test "water (0,0)" cc_water_pixel_0_0 "water x = 0 y = 0";
    string_convert_test "dirt (0,0)" cc_dirt_pixel_0_0 "dirt x = 0 y = 0";
    string_convert_test "steel (0,0)" cc_steel_pixel_0_0 "steel x = 0 y = 0";
    string_convert_test "empty (32,22)" cc_empty_pixel_32_22 "";
    string_convert_test "water (2,1)" cc_water_pixel_2_1 "water x = 2 y = 1";
    string_convert_test "dirt fall wait (1,0)" cc_dirt_pixel_0_1_fall_wait
      "dirt x = 1 y = 0";
    string_convert_test "empty (99,99)" cc_empty_pixel_32_22 "";
  ]

(* Run tests *)
let suite =
  "test suite for final project"
  >::: List.flatten
         [
           create_pixel_tests;
           draw_pixel_tests;
           delete_pixel_tests;
           rev_flush_tests;
           init_board_tests;
           move_tests;
           map_mat_rgb_tests;
           coord_convert_tests;
           string_convert_tests;
         ]

let _ = run_test_tt_main suite
