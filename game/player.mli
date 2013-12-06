(*Module Interface for Team*)
(* module type Team = sig *)
  open Constants
  open Definitions

  val update_pos : (direction * direction) list -> team_data -> team_data

  val victim : team_data -> team_data

  val shooter : team_data -> team_data

  val grazed : team_data -> team_data

  val add_charge : team_data -> team_data

  val dec_charge : team_data -> int -> team_data 
  
  val can_shoot : team_data -> bullet_type -> bool 

  val get_p_pos : game_data -> color -> position

(* end *)