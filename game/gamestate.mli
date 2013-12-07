(*Modual Interface File for Gamestate*)

  open Constants
  open Definitions

  type my_game = {
    duration : float;
    data : game_data;
    red_moves : (direction * direction) list;
    blue_moves : (direction * direction) list;
    red_inv : int;
    blue_inv : int;
    red_bomb : bool;
    blue_bomb : bool;
  }

  val handle_move : my_game -> color -> (direction * direction) list -> my_game

  val handle_shoot : my_game -> color -> bullet_type -> position -> 
    acceleration -> my_game

  val handle_focus : my_game -> color -> bool -> my_game

  val handle_bomb : my_game -> color -> my_game

  val check_result : game_data -> float -> result

  val update_game : my_game -> my_game
