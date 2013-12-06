(*Module Interface for Team*)
(* module type Team = sig *)
  open Constants
  open Definitions

  val update_pos : (direction * direction) list -> team_data -> team_data

  val add_charge : team_data -> team_data
  
(* end *)