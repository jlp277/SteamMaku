module Team = struct

open Definitions
open Constants
open Util

let update_pos (dir_lst : direction list) (team : team_data) : team_data =
  failwith("Ocaml is the best")

let add_charge (team : team_data) : team_data = 
  let (lives, bombs, score, power, charge, player) = team in
  let charge' = charge + cCHARGE_RATE in
  if charge' > cCHARGE_MAX then
    (lives, bombs, score, power, cCHARGE_MAX, player)
  else
    (lives, bombs, score, power, charge', player)
end