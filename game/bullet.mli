(*Module Interface for Bullet*)
(* module type Bullet = sig *)
  open Definitions
  open Constants

  val check_contacts : game_data -> (player_char * bullet) list * (player_char * bullet) list

  val update : bullet list -> bullet list

  val remove_bullet : bullet -> bullet list -> bullet list

(* end *)