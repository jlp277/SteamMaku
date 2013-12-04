module GameState = struct

open Definitions
open Constants
open Util

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

let handle_move (game : game) (col : color) (dir_lst : direction list) : game =
  if col = Red then
    { game with red_moves = dir_lst }
  else
    { game with blue_moves = dir_lst }

let get_p_pos game col =
  match game with
    | (red,blue,_,_,_) ->
      if col = Red then (
        match red with
        | (_,_,_,_,_,player) -> player.p_pos
        | _ -> failwith "bad team_data in handle_shoot" )
      else (
        match blue with
        | (_,_,_,_,_,player) -> player.p_pos
        | _ -> failwith "bad team_data in handle_shoot" )
    | _ -> failwith "bad game in handle_shoot"

let get_vel origin target speed : velocity =
  scale speed (unit_v (subt_v target origin))

let get_acc acc : acceleration =
  if (magnitude acc) >= cACCEL_LIMIT then (0.,0.) else acc

let dec_charge team amt : team_data =
  match team with
  | (lives,bomb,score,power,charge,player) ->
    let charge' = if charge-amt < 0 then 0 else charge-amt in
    (lives,bomb,score,power,charge',player)
  | _ -> failwith "bad team_data in dec_charge"

let can_shoot team b_type : bool =
  match team with
  | (lives,bomb,score,power,charge,player) ->
    (charge-(cost_of_bullet b_type)) >= 0
  | _ -> failwith "bad team_data in can_shoot"

let handle_shoot game col b_type target b_acc =
  let p_pos = get_p_pos game col in
  let data' = match b_type with
    | Bubble ->
      let bubble : bullet = {
        b_type = Bubble;  
        b_id = next_available_id();
        b_pos = p_pos;
        b_vel = get_vel p_pos target (speed_of_bullet Bubble);
        b_accel = get_acc b_acc;
        b_radius = radius_of_bullet Bubble;
        b_color = col } in
      let data' = match game.data with
        | (red,blue,npcs,bullets,powerups) ->
          if col = Red & (can_shoot red Bubble) then
            let red' = dec_charge red (cost_of_bullet Bubble) in
            (red',blue,npcs,(bubble::bullets),powerups)
          else if col = Blue & (can_shoot blue Bubble) then
            let blue' = dec_charge blue (cost_of_bullet Bubble) in
            (red,blue',npcs,(bubble::bullets),powerups)
          else
            (red,blue,npcs,bullets,powerups)
        | _ -> failwith "bad game_data in bubble" in
      { game with data = data' }
    | Spread -> (* todo *)
    | Trail -> (* todo *)
    | _ -> failwith "bad bullet type in handle_shoot" in
  { game with data = data' }

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
  { game with data = data' }

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