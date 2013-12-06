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

(* updates player hit by enemy bullet *)
let victim (team : team_data) : team_data =
  match team with
  | (lives,bomb,score,power,charge,player) ->
    (*decrease this players lives in the gui*)
    let _ = add_update (SetLives(player.p_color,(lives-1))) in
    let _ = add_update (SetBombs(player.p_color,cINITIAL_BOMBS)) in
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

(* end *)
