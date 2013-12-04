module GameState = struct

open Definitions
open Constants
open Util

let decrement_lives (team : team_data) : team_data =

  match team with
  | (lives,bombs,score,power,charge,player) ->
    (lives-1,cINITIAL_BOMBS,score,power,charge,player)

let victim (team : team_data) : team_data =
  match team with
  | (lives,bomb,score,power,charge,player) ->
    (lives - 1,cINITIAL_BOMBS,score,power/2,charge,player)
  | _ -> failwith "bad team_data in handle_focus"

let shooter (team : team_data) : team_data = 
  match team with
  | (lives,bomb,score,power,charge,player) ->
    (lives,bomb,score+cKILL_POINTS,power,charge,player)
  | _ -> failwith "bad team_data in handle_focus"

let handle_move game col dir_lst =

let handle_shoot game col b_type b_pos b_acc =

let handle_focus game col f_bool =
  let data' = match game.data with
    | (red,blue,npcs,bullets,power) ->
      let team = if col = Red then red else blue in
      let team' = (
        match team with
        | (lives,bomb,score,power,charge,player) ->
          (lives,bomb,score,power,charge,(player with p_focused = f_bool))
        | _ -> failwith "bad team_data in handle_focus" )
    | failwith "bad game_data in handle_focus" in
  game with data = data'

let handle_bomb game col =
  let data' = match game.data with
    | (red,blue,npcs,bullets,power) ->
      (red,blue,npcs,[],power)
    | failwith "bad game_data in handle_bomb" in
  if col = Red then
    { game with data = data'; red_inv = cBOMB_DURATION; red_bomb = true }
  else
    { game with data = data'; blue_inv = cBOMB_DURATION; blue_bomb = true }

let check_result (data: game_data) (duration: float) : result =
  failwith "Picasso was the man"
end