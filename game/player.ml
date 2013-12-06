(* module Team : Team = struct *)

open Definitions
open Constants
open Util
open Netgraphics

let update_pos (dir_lst : (direction * direction) list) (team : team_data) : team_data =
  match team with
  | (lives,bomb,score,power,charge,player) ->
    let speed = match player.p_focused with
      | true -> cFOCUSED_SPEED
      | false -> cUNFOCUSED_SPEED in
    let (v_x,v_y) = match dir_lst with
      | h::t -> vector_of_dirs h (float_of_int speed)
      | [] -> vector_of_dirs (Neutral,Neutral) (float_of_int speed) in
    let p_pos' = match player.p_pos with
      | (x,y) ->
        let (x',y') = add_v (x,y) (v_x,v_y) in
        if in_bounds (x',y') then (x',y') else (x,y) in
    (* update the player position in the gui *)
    let _ = add_update (MovePlayer(player.p_id, p_pos')) in
    let player' = { player with p_pos = p_pos' } in
    (lives,bomb,score,power,charge,player')

let add_charge (team : team_data) : team_data = 
  let (lives, bombs, score, power, charge, player) = team in
  let charge' = charge + cCHARGE_RATE in
  if charge' > cCHARGE_MAX then
    (* update this players charge on the GUI *)
    let _ = add_update (SetCharge(player.p_color,cCHARGE_MAX)) in
    (lives, bombs, score, power, cCHARGE_MAX, player)
  else
    let _ = add_update (SetCharge(player.p_color,charge')) in
    (lives, bombs, score, power, charge', player)
(* end *)
