open Definitions
open Constants
open Util
open Netgraphics
include Bullet
include Player
include Gamestate
include Gui

type game = Gamestate.my_game

(* let set_rmoves (g:game) (mv_lst : (direction * direction) list) : game =
  {g with red_moves = mv_lst} *)

let init_game () : game =
  let rx = 1./.8. *. float_of_int cBOARD_WIDTH in
  let ry = (float_of_int cBOARD_HEIGHT) /. 2. in
  let bx = 7./.8. *. float_of_int cBOARD_WIDTH in
  let by = (float_of_int cBOARD_HEIGHT) /. 2. in
  let p_red : player_char = {
    p_id = next_available_id ();
    p_pos = (rx, ry);
    p_focused = false ;
    p_radius = cHITBOX_RADIUS;
    p_color = Red
  } in
  (* put the red player onto the board *)
  let _ = add_update (AddPlayer((p_red.p_id,p_red.p_color,p_red.p_pos))) in
  let p_blue : player_char = {
    p_id = next_available_id ();
    p_pos = (bx, by);
    p_focused = false;
    p_radius = cHITBOX_RADIUS;
    p_color = Blue
  } in
  (* put the blue player onto the board *)
  let _ = add_update (AddPlayer((p_blue.p_id,p_blue.p_color,p_blue.p_pos))) in


  (*lives,bombs,score,power,charge,p_red*)
  let red = (cINITIAL_LIVES,cINITIAL_BOMBS,0,0,0,p_red) in
  (* put the red data on the board *)
  let _ = add_update (SetBombs(Red,cINITIAL_BOMBS)) in
  let _ = add_update (SetLives(Red,cINITIAL_LIVES)) in 
  let _ = add_update (SetScore(Red,0)) in
  let _ = add_update (SetPower(Red,0)) in
  let _ = add_update (SetCharge(Red,0)) in 

  let blue = (cINITIAL_LIVES,cINITIAL_BOMBS,0,0,0,p_blue) in
  (* put the blue data on the board *)
  let _ = add_update (SetBombs(Blue,cINITIAL_BOMBS)) in
  let _ = add_update (SetLives(Blue,cINITIAL_LIVES)) in 
  let _ = add_update (SetScore(Blue,0)) in
  let _ = add_update (SetPower(Blue,0)) in
  let _ = add_update (SetCharge(Blue,0)) in 

  let n_data = (red,blue,[],[],[]) in
  let new_game = {
    duration = cTIME_LIMIT;
    data = (n_data : game_data);
    red_moves = [];
    blue_moves = [];
    red_inv = 0;
    blue_inv = 0;
    red_bomb = false;
    blue_bomb = false
  } in
  new_game
  
let handle_time game =
  let game' = Gamestate.update_game game in
  let result = Gamestate.check_result game'.data game'.duration in
  (game',result)

let handle_action game col act =
  match act with
  | Move (dir_lst) -> Gamestate.handle_move game col dir_lst
  | Shoot (b_type,target,b_acc) -> Gamestate.handle_shoot game col b_type target b_acc 
  | Focus (f_bool) -> Gamestate.handle_focus game col f_bool
  | Bomb -> Gamestate.handle_bomb game col

let get_data game = game.data

