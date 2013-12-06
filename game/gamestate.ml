(* module Gamestate : Gamestate = struct*)

open Definitions
open Constants
open Util
open Netgraphics
include Player
include Bullet
include Gui

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

(* updates player's list of movements *)
let handle_move (game : my_game) (col : color) (dir_lst : (direction * direction) list) : my_game =
  if col = Red then
    { game with red_moves = dir_lst }
  else
    { game with blue_moves = dir_lst }

let add_bullets (data : game_data) (col : color) (b_type : bullet_type) (new_b : bullet list) : game_data =
  match data with
  | (red,blue,npcs,bullets,powerups) ->
    if col = Red & (Player.can_shoot red b_type) then
      let red' = Player.dec_charge red (cost_of_bullet b_type) in
      (* add bullets to the gui *)
      let _ = Gui.gui_add_bullets new_b in
      (red',blue,npcs,(new_b@bullets),powerups)
    else if col = Blue & (Player.can_shoot blue Bubble) then
      let blue' = Player.dec_charge blue (cost_of_bullet Bubble) in
      (* add bullets to the gui *)
      let _ = Gui.gui_add_bullets new_b in
      (red,blue',npcs,(new_b@bullets),powerups)
    else
      (red,blue,npcs,bullets,powerups) 

(* updates list of active bullets *)
let handle_shoot (game : my_game) col b_type target b_acc =
  let p_pos = Player.get_p_pos game.data col in
  let data' = match b_type with
    | Bubble ->
      let bubble target' : bullet = {
        b_type = Bubble;  
        b_id = next_available_id();
        b_pos = p_pos;
        b_vel = Bullet.calc_vel target' (float_of_int(speed_of_bullet Bubble));
        b_accel = Bullet.calc_acc b_acc;
        b_radius = radius_of_bullet Bubble;
        b_color = col } in
      let targets = Bullet.build_targets_bubble [] (subt_v target p_pos) in
      let create_bubble acc target' =
        (bubble target')::acc in
      let new_b = List.fold_left create_bubble [] targets in
      add_bullets game.data col b_type new_b
    | Spread ->
      let spread target' : bullet = {
        b_type = Spread;  
        b_id = next_available_id();
        b_pos = p_pos;
        b_vel = Bullet.calc_vel target' (float_of_int (speed_of_bullet Bubble));
        b_accel = Bullet.calc_acc b_acc;
        b_radius = radius_of_bullet Spread;
        b_color = col } in
      let targets = Bullet.build_targets_spread [] (subt_v target p_pos) in
      let create_spread acc target' =
        (spread target')::acc in
      let new_b = List.fold_left create_spread [] targets in
      add_bullets game.data col b_type new_b
    | Trail ->
      let trail target' step : bullet = {
        b_type = Trail;  
        b_id = next_available_id();
        b_pos = p_pos;
        b_vel = Bullet.calc_vel target' (float_of_int ((speed_of_bullet Trail)*step));
        b_accel = Bullet.calc_acc b_acc;
        b_radius = radius_of_bullet Trail;
        b_color = col } in
      let targets = Bullet.build_targets_trail [] (subt_v target p_pos) in
      let create_trail acc target' =
        let rec create_trail_bullets acc i =
          if i = cTRAIL_NUM+1 then acc
          else
            let new_trail_bullet = trail target' (cTRAIL_SPEED_STEP*i) in
            create_trail_bullets (new_trail_bullet::acc) (i+1) in
        create_trail_bullets acc 1 in
      let new_b = List.fold_left create_trail [] targets in
      add_bullets game.data col b_type new_b
    | Power ->
      (* power ups not implemented *)
      let new_b = [] in
      add_bullets game.data col b_type new_b in
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
      if col = Red & (Player.can_bomb red) then
        let red' = (
          match red with
          | (lives,bomb,score,power,charge,player) ->
            let _ = add_update (SetBombs(Red,(bomb-1))) in
            (lives,bomb - 1,score,power,charge,player) ) in
        let _ = Gui.gui_clear_bullets bullets in
        (red',blue,npcs,[],power)
      else if col = Blue & (Player.can_bomb blue) then
        let blue' = (
          match blue with
          | (lives,bomb,score,power,charge,player) ->
            let _ = add_update (SetBombs(Blue,(bomb-1))) in
            (lives,bomb - 1,score,power,charge,player) ) in
        let _ = Gui.gui_clear_bullets bullets in
        (red,blue',npcs,[],power)
      else
        (red,blue,npcs,bullets,power) in
  let (red,blue,_,_,_) = game.data in
  if col = Red & (Player.can_bomb red) then
    (* signal the gui for a red bomb *)
    let _ = add_update (UseBomb(Red)) in
    { game with data = data'; red_inv = cBOMB_DURATION; red_bomb = true }
  else if col = Blue & (Player.can_bomb blue) then
    (* signal the gui for a red bomb *)
    let _ = add_update (UseBomb(Blue)) in
    { game with data = data'; blue_inv = cBOMB_DURATION; blue_bomb = true }
  else
    game

(* returns state of game *)
let check_result (data: game_data) (duration: float) : result =
  let check_score r_score b_score : result =
    if r_score > b_score then Winner(Red)
    else if b_score > r_score then Winner(Blue)
    else Tie in
  let check_duration duration : bool = duration <= 0. in
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
  | (0,0,duration,r_score,b_score) -> check_score r_score b_score
  | (r_lives,0,duration,r_score,b_score) -> Winner(Red)
  | (0,b_lives,duration,r_score,b_score) -> Winner(Blue)
  | (r_lives,b_lives,duration,r_score,b_score) ->
    if check_duration duration then check_score r_score b_score else Unfinished

let update_game (game : my_game) : my_game =
  let red_inv : int ref = ref game.red_inv in
  let blue_inv : int ref = ref game.blue_inv in
  let data' =
    match game.data with
    | (red,blue,npcs,bullets,powerups) ->
      let bullets' = Bullet.update bullets in
      let red' = Player.update_pos game.red_moves red in
      let blue' = Player.update_pos game.blue_moves blue in
      let red' = if not game.red_bomb then Player.add_charge red' else red' in
      let blue' = if not game.blue_bomb then Player.add_charge blue' else blue' in
      let rec handle_colls red' blue' bullet' lst =
        match lst with
        | [] -> (red',blue',bullet')
        | (hit, bull)::t ->
          if (hit.p_color = Red) then
            if game.red_inv <= 0 then
              let red' = Player.victim red' in
              let blue' = Player.shooter blue' in
              (* remove all bullets from the screen *)
              let _ = Gui.gui_clear_bullets bullet' in
              let bullet' = [] in
              let _ = red_inv := cINVINCIBLE_FRAMES in
              handle_colls red' blue' bullet' t
            else (*red is invincible delete bullets*)
              let bullet' = Bullet.remove_bullet bull bullet' in
              handle_colls red' blue' bullet' t
          else if (hit.p_color = Blue) then
            if game.blue_inv <= 0 then
              let blue' = Player.victim blue' in
              let red' = Player.shooter red' in
              (* remove all bullets from the screen *)
              let _ = Gui.gui_clear_bullets bullet' in
              let bullet' = [] in
              let _ = blue_inv := cINVINCIBLE_FRAMES in
              handle_colls red' blue' bullet' t
            else (*blue is invincible delete bullet*)
              let bullet' = Bullet.remove_bullet bull bullet' in
              handle_colls red' blue' bullet' t
          else
            handle_colls red' blue' bullet' t in
      let rec handle_grazs red' blue' bullets' lst =
        match lst with
        | [] -> (red',blue',bullets')
        | (gra, bull)::t ->
          if (gra.p_color = Red) then
            if not game.red_bomb then
              let red' = Player.grazed red' in
              handle_grazs red' blue' bullets' t
            else
              let _ = add_update (DeleteBullet(bull.b_id)) in
              let bullets' = Bullet.remove_bullet bull bullets' in
              handle_grazs red' blue' bullets' t
          else if (gra.p_color = Blue) then
            if not game.blue_bomb then
              let blue' = Player.grazed blue' in
              handle_grazs red' blue' bullets' t
            else
              let _ = add_update (DeleteBullet(bull.b_id)) in
              let bullets' = Bullet.remove_bullet bull bullets' in
              handle_grazs red' blue' bullets' t
          else
            handle_grazs red' blue' bullets' t in
      let (collisions,grazes) = Bullet.check_contacts (red',blue',npcs,bullets',powerups) in
      let (red',blue',bullets') = handle_colls red' blue' bullets' collisions in
      let (red',blue',bullets') = handle_grazs red' blue' bullets' grazes in
      (red',blue',npcs,bullets',powerups) in
  let red_moves' = match game.red_moves with | h::t -> t | _ -> [] in
  let blue_moves' = match game.blue_moves with | h::t -> t | _ -> [] in
  let duration' = game.duration -. cUPDATE_TIME in
  let game' = {
    duration = duration';
    data = data';
    red_moves = red_moves';
    blue_moves = blue_moves';
    red_inv = !red_inv - 1;
    blue_inv = !blue_inv - 1;
    red_bomb = 
      if !red_inv - 1 <= 0 then false 
      else if (game.red_bomb) then true
      else (* we are mercy invincible *) false;
    blue_bomb = 
      if !blue_inv - 1 <= 0 then false
      else if (game.blue_bomb) then true
      else false
     } in
  game'
(*end*)