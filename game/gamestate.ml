(*module Gamestate : Gamestate = struct*)

open Definitions
open Constants
open Util
open Netgraphics

type my_game = {
    duration : float;
    data : game_data;
    red_moves : (direction * direction) list;
    blue_moves : (direction * direction) list;
    red_inv : int;
    blue_inv : int;
    red_bomb : bool;
    blue_bomb : bool;
  }

(* updates player hit by enemy bullet *)
let victim (team : team_data) : team_data =
  match team with
  | (lives,bomb,score,power,charge,player) ->
    (*decrease this players lives in the gui*)
    let _ = add_update (SetLives(player.p_color,(lives-1))) in
    (lives - 1,cINITIAL_BOMBS,score,power/2,charge,player)

(* updates player who hit enemy with bullet *)
let shooter (team : team_data) : team_data = 
  match team with
  | (lives,bomb,score,power,charge,player) ->
    (*increase the points for this player in the gui*)
    let _ = add_update (SetScore(player.p_color,(score+cKILL_POINTS))) in
    (lives,bomb,score+cKILL_POINTS,power,charge,player)

(*updates player who was grazed by a bullet*)
let grazed (team : team_data) : team_data =
  match team with
  | (lives,bomb,score,power,charge,player) ->
    (*increase the score of this player*)
    let _ = add_update (SetScore(player.p_color,(score+cGRAZE_POINTS))) in
    (lives,bomb,score+cGRAZE_POINTS,power,charge,player)


(* updates player's list of movements *)
let handle_move (game : my_game) (col : color) (dir_lst : (direction * direction) list) : my_game =
  if col = Red then
    { game with red_moves = dir_lst }
  else
    { game with blue_moves = dir_lst }

(* returns position of player *)
let get_p_pos data col =
  match data with
    | (red,blue,_,_,_) ->
      if col = Red then (
        match red with
        | (_,_,_,_,_,player) -> player.p_pos )
      else (
        match blue with
        | (_,_,_,_,_,player) -> player.p_pos )

(* returns velocity *)
let get_vel vector speed : velocity =
  scale speed (unit_v vector)

(* returns capped acceleration *)
let get_acc acc : acceleration =
  if (magnitude acc) >= cACCEL_LIMIT then (0.,0.) else acc

(* decreases charge of player *)
let dec_charge team amt : team_data =
  match team with
  | (lives,bomb,score,power,charge,player) ->
    let charge' = if charge-amt < 0 then 0 else charge-amt in
    (lives,bomb,score,power,charge',player)

(* checks if player has enough charge to shoot *)
let can_shoot team b_type : bool =
  match team with
  | (lives,bomb,score,power,charge,player) ->
    (charge-(cost_of_bullet b_type)) >= 0

let rec build_targets_spread acc orig_v i =
  if i = cSPREAD_NUM then acc
  else
    let new_v = rotate_deg orig_v (float_of_int (360/(cSPREAD_NUM*i)) in
    build_targets_spread (new_v::acc) orig_v (i+1)

let rec build_targets_trail orig_v =
  (rotate_deg orig_v (float_of_int cTRAIL_ANGLE))::
  (rotate_deg orig_v (float_of_int(360-cTRAIL_ANGLE)))::
  (orig_v)::[]

(* helper function to add a list of bullets to the gui *)
let rec gui_add_bullets (bullets : bullet list) : unit =
  match bullets with
  | [] -> ()
  | h::t ->
    let _ = add_update (AddBullet(h.b_id, h.b_color, h.b_type, h.b_pos)) in
    gui_add_bullets t

(* updates list of active bullets *)
let handle_shoot (game : my_game) col b_type target b_acc =
  let p_pos = get_p_pos game.data col in
  let data' = match b_type with
    | Bubble ->
      let bubble : bullet = {
        b_type = Bubble;  
        b_id = next_available_id();
        b_pos = p_pos;
        b_vel = get_vel (subt_v target p_pos) (float_of_int(speed_of_bullet Bubble));
        b_accel = get_acc b_acc;
        b_radius = radius_of_bullet Bubble;
        b_color = col } in
      (match game.data with
        | (red,blue,npcs,bullets,powerups) ->
          if col = Red & (can_shoot red Bubble) then
            let red' = dec_charge red (cost_of_bullet Bubble) in
            (* add this bullet to the gui *)
            let _ = add_update (AddBullet(bubble.b_id,Red,Bubble,bubble.b_pos)) in
            (red',blue,npcs,(bubble::bullets),powerups)
          else if col = Blue & (can_shoot blue Bubble) then
            let blue' = dec_charge blue (cost_of_bullet Bubble) in
            (* add this bullet to the gui *)
            let _ = add_update (AddBullet(bubble.b_id,Blue,Bubble,bubble.b_pos)) in
            (red,blue',npcs,(bubble::bullets),powerups)
          else
            (red,blue,npcs,bullets,powerups) )
    | Spread ->
      let spread target' : bullet = {
        b_type = Spread;  
        b_id = next_available_id();
        b_pos = p_pos;
        b_vel = get_vel target' (float_of_int (speed_of_bullet Bubble));
        b_accel = get_acc b_acc;
        b_radius = radius_of_bullet Spread;
        b_color = col } in
      let targets = build_targets_spread [] (subt_v target p_pos) 0 in
      let spread_list =
        List.fold_left (fun a target' -> (spread target')::a) [] targets in
      (match game.data with
        | (red,blue,npcs,bullets,powerups) ->
          if col = Red & (can_shoot red Spread) then
            let red' = dec_charge red (cost_of_bullet Spread) in
            (* add this list of bullets to the gui *)
            let _ = gui_add_bullets spread_list in
            (red',blue,npcs,(spread_list@bullets),powerups)
          else if col = Blue & (can_shoot blue Spread) then
            let blue' = dec_charge blue (cost_of_bullet Spread) in
            (* add this list of bullets to the gui *)
            let _ = gui_add_bullets spread_list in
            (red,blue',npcs,(spread_list@bullets),powerups)
          else
            (red,blue,npcs,bullets,powerups) )
    | Trail ->
      let trail target' step : bullet = {
        b_type = Trail;  
        b_id = next_available_id();
        b_pos = p_pos;
        b_vel = get_vel target' (float_of_int ((speed_of_bullet Trail)*step));
        b_accel = get_acc b_acc;
        b_radius = radius_of_bullet Trail;
        b_color = col } in
      let targets = build_targets_trail (subt_v target p_pos) in
      let create_trail acc target' =
        let rec create_trail_bullets acc i =
          if i = cTRAIL_NUM+1 then acc
          else
            let new_trail_bullet = trail target' (cTRAIL_SPEED_STEP*i) in
            create_trail_bullets (new_trail_bullet::acc) (i+1) in
        create_trail_bullets acc 1 in
      let trail_list = List.fold_left create_trail [] targets in
      (match game.data with
        | (red,blue,npcs,bullets,powerups) ->
          if col = Red & (can_shoot red Trail) then
            let red' = dec_charge red (cost_of_bullet Trail) in
            (* add this list of bullets to the gui *)
            let _ = gui_add_bullets trail_list in
            (red',blue,npcs,(trail_list@bullets),powerups)
          else if col = Blue & (can_shoot blue Trail) then
            let blue' = dec_charge blue (cost_of_bullet Trail) in
            (* add this list of bullets to the gui *)
            let _ = gui_add_bullets trail_list in
            (red,blue',npcs,(trail_list@bullets),powerups)
          else
            (red,blue,npcs,bullets,powerups) ) 
    | Power -> failwith "No powerups!" in
  { game with data = data' }

(* updates player's focus state *)
let handle_focus game col f_bool =
  let data' = match game.data with
    | (red,blue,npcs,bullets,power) ->
      if col = Red then
        let red' = (
          match red with
          | (lives,bomb,score,power,charge,player) ->
            let player' = { player with p_focused = f_bool } in
            (lives,bomb,score,power,charge,player') ) in
        (red',blue,npcs,bullets,power)
      else
        let blue' = (
          match blue with
          | (lives,bomb,score,power,charge,player) ->
            let player' = { player with p_focused = f_bool } in
            (lives,bomb,score,power,charge,player') ) in
        (red,blue',npcs,bullets,power) in
  { game with data = data' }

(* updates game when a player has used a bomb *)
let handle_bomb game col =
  let data' = match game.data with
    | (red,blue,npcs,bullets,power) ->
      if col = Red then
        let red' = (
          match red with
          | (lives,bomb,score,power,charge,player) ->
            (lives,bomb - 1,score,power,charge,player) ) in
        (red',blue,npcs,[],power)
      else
        let blue' = (
          match blue with
          | (lives,bomb,score,power,charge,player) ->
            (lives,bomb - 1,score,power,charge,player) ) in
        (red,blue',npcs,[],power) in
  if col = Red then
    (* signal the gui for a red bomb *)
    let _ = add_update (UseBomb(Red)) in
    { game with data = data'; red_inv = cBOMB_DURATION; red_bomb = true }
  else
    (* signal the gui for a red bomb *)
    let _ = add_update (UseBomb(Blue)) in
    { game with data = data'; blue_inv = cBOMB_DURATION; blue_bomb = true }

let check_score r_score b_score : result =
  if r_score > b_score then Winner(Red)
  else if b_score > r_score then Winner(Blue)
  else Tie

(* returns state of game *)
let check_result (data: game_data) (duration: float) : result =
  let ((r_score,r_lives),(b_score,b_lives)) =
    match data with
    | (red,blue,npcs,bullets,power) ->
      let r_stats = (
        match red with
        | (lives,_,score,_,_,_) -> (score,lives) ) in
      let b_stats = (
        match blue with
        | (lives,_,score,_,_,_) -> (score,lives) ) in
      (r_stats,b_stats) in
  match (r_lives,b_lives,duration,r_score,b_score) with
  | (0,0,0.,r_score,b_score) -> check_score r_score b_score
  | (0,0,duration,r_score,b_score) -> check_score r_score b_score
  | (0,b_lives,0.,r_score,b_score) -> Winner(Blue)
  | (r_lives,0,0.,r_score,b_score) -> Winner(Red)
  | (r_lives,b_lives,0.,r_score,b_score) -> check_score r_score b_score
  | (r_lives,0,duration,r_score,b_score) -> Winner(Red)
  | (0,b_lives,duration,r_score,b_score) -> Winner(Blue)
  | (r_lives,b_lives,duration,r_score,b_score) -> Unfinished
(*end*)