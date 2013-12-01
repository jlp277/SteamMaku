module GameState = struct

open Definitions
open Constants
open Util

let decrement_lives (team : team_data) : team_data =
  match team with
  | (lives,bombs,score,power,charge,player) ->
    (lives-1,cINITIAL_BOMBS,score,power,charge,player)

let check_result (data: game_data) (duration: float) : result =
  failwith "Picasso was the man"
end