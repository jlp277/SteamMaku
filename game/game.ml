open Definitions
open Constants
open Util
open GameState

type game = {
  duration : float;
  data : game_data;
}

let init_game () : game =
  failwith "U.N. Owen wasn't her"

let handle_time game =
  let duration' = game.duration +. cUPDATE_TIME in
  let data' =
    match game.data with
    | (red,blue,npcs,bullets,powerups) ->
      let bullets' = Bullet.update bullets in
      (* toggle focus mode if needed *)
      (* update player positions *)
      (* make list of bullet/player collisions *)
      let rec handle_colls red' blue' bullets' lst =
        match lst with
        | [] -> (red',blue',bullets')
        | hit::t ->
          if hit.p_color = Red then
            let red' = GameState.victim red' in
            let blue' = Gamestate.shooter blue' in
            let bullets' = [] in
            handle_colls red' blue' bullets'
          else
            let blue' = GameState.victim blue' in
            let red' = Gamestate.shooter red' in
            let bullets' = [] in
            handle_colls red' blue' bullets' in
      let collisions = Bullet.check_collisions (red',blue',npcs,bullets',powerups) in
      let (red',blue',bullets') = handle_colls red' blue' bullets' collisions in
      (red',blue',npcs,bullets',powerups)
    | _ -> failwith "bad game.data" in
  let game' = {duration = duration'; data = data'} in
  (*Check Victory Conditions*)
  result = GameState.check_result game' duration' in
  (game',result)
let handle_action game col act =
  failwith "A myon sort of day"

let get_data game =
  failwith "I'm the strongest!"

