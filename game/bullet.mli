(*Module Interface for Bullet*)
(* module type Bullet = sig *)
  open Definitions
  open Constants
  open Util

  val check_contacts : game_data -> (player_char * bullet) list * (player_char * bullet) list

  val update : bullet list -> bullet list

  val remove_bullet : bullet -> bullet list -> bullet list

  val build_targets_spread : vector list -> vector -> int -> vector list

  val build_targets_trail : vector -> vector list

  val calc_vel : vector -> float -> velocity

  val calc_acc : acceleration -> acceleration

(* end *)