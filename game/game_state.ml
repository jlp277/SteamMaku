module GameState = struct

open Definitions
open Constants
open Util

let decrement_lives (team : team_data) : team_data =
  match team with
  | (lives,bombs,score,power,charge,player) ->
    (lives-1,cINITIAL_BOMBS,score,power,charge,player)

let victim (team : team_data) : team_data=

let shooter (team : team_data) : team_data = 

let handle_move game col dir_lst =

let handle_shoot game col b_type b_pos b_acc =

let handle_focus game col f_bool =

let handle_bomb game col =

let check_result (data: game_data) (duration: float) : result =
  failwith "Picasso was the man"
end