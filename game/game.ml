open Definitions
open Constants
open Util
open GameState

type game = {
  duration : float;
  data : game_data;
  red_moves : direction list;
  blue_moves : direction list;
  (* red_foc : bool;
  blue_foc : bool; *)
  red_inv : int;
  blue_inv : int;
  red_bomb : bool;
  blue_bomb : bool;
}

let init_game () : game =
  failwith "U.N. Owen wasn't her"

let handle_time game =
  let duration' = game.duration +. cUPDATE_TIME in
  let red_inv : int ref = ref game.red_inv in
  let blue_inv : int ref = ref game.blue_inv in
  let data' =
    match game.data with
    | (red,blue,npcs,bullets,powerups) ->
      let bullets' = Bullet.update bullets in
      (* let red' = Team.update_foc game.red_foc red in
      let blue' = Team.update_foc game.blue_foc blue in *)
      let red' = Team.update_pos game.red_moves red' in
      let blue' = Team.update_pos game.blue_moves blue' in
      let red' = Team.add_charge red' in
      let blue' = Team.add_charge blue' in
      let rec handle_colls red' blue' bullets' lst =
        match lst with
        | [] -> (red',blue',bullets')
        | hit::t ->
          if (hit.p_color = Red) & (game.red_inv <= 0) then
            let red' = GameState.victim red' in
            let blue' = GameState.shooter blue' in
            let bullets' = [] in
            let _ = red_inv := cINVINCIBLE_FRAMES in
            handle_colls red' blue' bullets' t
          else if (hit.p_color = Blue) & (game.blue_inv <= 0) then
            let blue' = GameState.victim blue' in
            let red' = GameState.shooter red' in
            let bullets' = [] in
            let _ = blue_inv := cINVINCIBLE_FRAMES in
            handle_colls red' blue' bullets' t in
          else
            let red' = red' in
            let blue' = blue' in
            let bullets' = bullets' in
            handle_colls red' blue' bullets' t in
      let rec handle_grazs red' blue' bullets' lst =
        match lst with
        | [] -> (red',blue',bullets')
        | gra::t ->
          (* check for bomb invincibility *)
          if (gra.p_color = Red) then
            let red' = GameState.grazed red' in
            handle_grazs red' blue' bullets' t
          else if (hit.p_color = Blue) then
            let blue' = GameState.grazed blue' in
            handle_grazs red' blue' bullets' t in
          else
            let red' = red' in
            let blue' = blue' in
            let bullets' = bullets' in
            handle_grazs red' blue' bullets' t in
      let (collisions,grazes) = Bullet.check_collisions (red',blue',npcs,bullets',powerups) in
      let (red',blue',bullets') = handle_colls red' blue' bullets' collisions in
      let (red',blue',bullets') = handle_grazs red' blue' bullets' collisions in
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
    (* red_bomb = if red_inv ;
    blue_bomb =   *)} in
  (* check victory conditions *)
  result = GameState.check_result game' duration' in
  (game',result)

let handle_action game col act =
  match act with
  | Move (dir_lst) -> GameState.handle_move game col dir_list
  | Shoot (b_type,b_pos,b_acc) -> GameState.handle_shoot game col b_type b_pos b_acc 
  | Focus (f_bool) -> GameState.handle_focus game col f_bool
  | Bomb -> GameState.handle_bomb game col

let get_data game = game.data

