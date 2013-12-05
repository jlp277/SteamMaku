module Team = struct

open Definitions
open Constants
open Util

let update_pos (dir_lst : direction list) (team : team_data) : team_data =
  match team with
  | (lives,bomb,score,power,charge,player) ->
    let speed = match player.p_focused with
      | true -> cFOCUSED_SPEED
      | false -> cUNFOCUSED_SPEED in
    let (v_x,v_y) = match dir_lst with
      | h::t -> vector_of_dirs h speed
      | [] -> vector_of_dirs (Neutral,Neutral) speed in
    let p_pos' = match player.p_pos with
      | (x,y) ->
        let (x',y') = add_v (x,y) (v_x,v_y) in
        if in_bounds (x',y') then (x',y') else (x,y)
      | _ -> failwith "bad p_pos in update_pos" in
    let player' = { player with p_pos = p_pos' } in
    (lives,bomb,score,power,charge,player')
  | _ -> failwith "bad team_data in update_foc"

let add_charge (team : team_data) : team_data = 
  let (lives, bombs, score, power, charge, player) = team in
  let charge' = charge + cCHARGE_RATE in
  if charge' > cCHARGE_MAX then
    (lives, bombs, score, power, cCHARGE_MAX, player)
  else
    (lives, bombs, score, power, charge', player)
end
