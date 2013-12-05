(*Modual Interface File for Gamestate*)

module type Gamestate = sig

  open Constants
  open Definitions

  type game = {
    duration : float;
    data : game_data;
    red_moves : direction list;
    blue_moves : direction list;
    red_inv : int;
    blue_inv : int;
    red_bomb : bool;
    blue_bomb : bool;
  }

  val victim : team_data -> team_data

  val shooter : team_data -> team_data

  val handle_move : game -> color -> direction list -> game

  val handle_shoot : game -> color -> bullet_type -> position -> 
    acceleration -> game

  val handle_focus : game -> color -> bool -> game

  val handle_bomb : game -> color -> game

  val check_result : game_data -> float -> result

end