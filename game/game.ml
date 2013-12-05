open Definitions
open Constants
open Util
include Bullet
include Team
include Gamestate

type game = {
  duration : float;
  data : game_data;
  red_moves : direction list;
  blue_moves : direction list;
  red_inv : int;
  blue_inv : int;
  red_bomb : bool;
  blue_bomb : bool;
}


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
  let p_blue : player_char = {
    p_id = next_available_id ();
    p_pos = (bx, by);
    p_focused = false;
    p_radius = cHITBOX_RADIUS;
    p_color = Blue
  } in
  (*lives,bombs,score,power,charge,p_red*)
  let red = (cINITIAL_LIVES,cINITIAL_BOMBS,0,0,0,p_red) in
  let blue = (cINITIAL_LIVES,cINITIAL_BOMBS,0,0,0,p_blue) in
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
  let duration' = game.duration +. cUPDATE_TIME in
  let red_inv : int ref = ref game.red_inv in
  let blue_inv : int ref = ref game.blue_inv in
  let data' =
    match game.data with
    | (red,blue,npcs,bullets,powerups) ->
      let bullets' = Bullet.update bullets in
      let red' = Team.update_pos game.red_moves red' in
      let blue' = Team.update_pos game.blue_moves blue' in
      let red' = Team.add_charge red' in
      let blue' = Team.add_charge blue' in
      let rec handle_colls red' blue' bullets' lst =
        match lst with
        | [] -> (red',blue',bullets')
        | (hit, bull)::t ->
          if (hit.p_color = Red) then
            if game.red_inv <= 0 then
              let red' = Gamestate.victim red' in
              let blue' = Gamestate.shooter blue' in
              let bullets' = [] in
              let _ = red_inv := cINVINCIBLE_FRAMES in
              handle_colls red' blue' bullets' t
            else (*red is invincible delete bullet*)
              let bullets' = Bullet.remove_bullet bull bullets' in
              handle_colls red' blue' bullets' t
          else if (hit.p_color = Blue) then
            if game.blue_inv <= 0 then
              let blue' = Gamestate.victim blue' in
              let red' = Gamestate.shooter red' in
              let bullets' = [] in
              let _ = blue_inv := cINVINCIBLE_FRAMES in
              handle_colls red' blue' bullets' t
            else (*blue is invincible delete bullet*)
              let bullets' = Bullet.remove_bullet bull bullets' in
              handle_colls red' blue' bullets' t
          else
            let red' = red' in
            let blue' = blue' in
            let bullets' = bullets' in
            handle_colls red' blue' bullets' t in
      let rec handle_grazs red' blue' bullets' lst =
        match lst with
        | [] -> (red',blue',bullets')
        | (gra, bull)::t ->
          (* check for bomb invincibility *)
          if (gra.p_color = Red) then
            if not game.red_bomb then
              let red' = Gamestate.grazed red' in
              handle_grazs red' blue' bullets' t
            else
              let bullets' = Bullet.remove_bullet bull bullets' in
              handle_grazs red' blue' bullets' t
          else if (hit.p_color = Blue) then
            if not game.blue_bomb then
              let blue' = Gamestate.grazed blue' in
              handle_grazs red' blue' bullets' t
            else
              let bullets' = Bullet.remove_bullet bull bullets' in
              handle grazs red' blue' bullets' t
          else
            let red' = red' in
            let blue' = blue' in
            let bullets' = bullets' in
            handle_grazs red' blue' bullets' t in
      let (collisions,grazes) = Bullet.check_contacts (red',blue',npcs,bullets',powerups) in
      let (red',blue',bullets') = handle_colls red' blue' bullets' collisions in
      let (red',blue',bullets') = handle_grazs red' blue' bullets' grazes in
      (red',blue',npcs,bullets',powerups)
    | _ -> failwith "bad game.data" in
  let red_moves' = match game.red_moves with | h::t -> t | _ -> [] in
  let blue_moves' = match game.blue_moves with | h::t -> t | _ -> [] in
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
  (* check victory conditions *)
  let result = Gamestate.check_result game' duration' in
  (game',result)

let handle_action game col act =
  match act with
  | Move (dir_lst) -> Gamestate.handle_move game col dir_list
  | Shoot (b_type,b_pos,b_acc) -> Gamestate.handle_shoot game col b_type target b_acc 
  | Focus (f_bool) -> Gamestate.handle_focus game col f_bool
  | Bomb -> Gamestate.handle_bomb game col

let get_data game = game.data

