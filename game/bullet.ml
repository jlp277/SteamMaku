(* module Bullet : Bullet = struct *)

open Definitions
open Constants
open Util
open Netgraphics

(*checks to see if the bullet b has collided with the player p
 *returns false if the bullet and the player are the same color
 *)
let check_collision (b : bullet) (p : player_char) : bool =
  if b.b_color = p.p_color then
    false
  else
    let dist = distance b.b_pos p.p_pos in
    let min_dist = float_of_int (b.b_radius+p.p_radius) in
    if dist < min_dist then true
    else false

(*checks to see if the bullet b has grazed the player (but not collided) p
 *returns false if the bullet and the player are the same color
 *)
let check_graze (b : bullet) (p : player_char) : bool =
  if b.b_color = p.p_color then
    false
  else
    let dist = distance b.b_pos p.p_pos in
    let coll_dist = float_of_int (b.b_radius+p.p_radius) in
    let min_dist = float_of_int cGRAZE_RADIUS in
    if coll_dist <= dist & dist < min_dist then true
    else false

(*the helper function used in check contacts uses the refs passed
 *in as accumulators
 *)
let rec check_help (p1 : player_char * bool) (p2 : player_char * bool)
  (colls : (player_char * bullet) list ref) 
  (graze : (player_char * bullet) list ref)
  (bullets : bullet list) : unit =

  let (p1_char, p1_bool) = p1 in
  let (p2_char, p2_bool) = p2 in
  match bullets with
  | [] -> ()
  | h::t -> begin
    if (check_collision h p1_char) & (check_collision h p2_char) then
      if p1_bool & p2_bool then
        (*add neither collision to colls*)
        check_help p1 p2 colls graze t
      else if p1_bool then
        (*only add p2's collision to colls*)
        let _ = colls := (p2_char, h) :: !colls in
        check_help p1 (p2_char, true) colls graze t
      else if p2_bool then
        (*only add p1's collision to colls*)
        let _ = colls := (p1_char, h) :: !colls in
        check_help (p1_char, true) p2 colls graze t
      else 
        (*add both collisions to colls*)
        let _ = colls := (p2_char, h) :: (p1_char,h) :: !colls in
        check_help (p1_char, true) (p2_char, true) colls graze t

    else if (check_collision h p1_char) & (check_graze h p2_char) then
      (*add p2's graze to graze list*)
      let _ = graze := (p2_char, h) :: !graze in
      if p1_bool then
        (*don't add p1's collision to colls*)
        check_help p1 p2 colls graze t
      else 
        (*do add the collision to colls*)
        let _ = colls := (p1_char, h) :: !colls in
        check_help (p1_char, true) p2 colls graze t

    else if (check_collision h p1_char) then
       if p1_bool then
        (*don't add p1's collision to colls*)
        check_help p1 p2 colls graze t
       else 
        (*do add the collision to colls*)
        let _ = colls := (p1_char, h) :: !colls in
        check_help (p1_char, true) p2 colls graze t
    else if (check_collision h p2_char) & (check_graze h p1_char) then
      (*add p1's graze to graze list*)
      let _ = graze := (p1_char, h) :: !graze in
      if p2_bool then
        (*don't add p2's collision to colls*)
        check_help p1 p2 colls graze t
      else 
        (*do add the collision to colls*)
        let _ = colls := (p2_char, h) :: !colls in
        check_help p1 (p2_char, true) colls graze t

    else if (check_collision h p2_char) then
       if p2_bool then
        (*don't add p1's collision to colls*)
        check_help p1 p2 colls graze t
       else 
        (*do add the collision to colls*)
        let _ = colls := (p2_char, h) :: !colls in
        check_help p1 (p2_char, true) colls graze t

    else if (check_graze h p1_char) & (check_graze h p2_char) then
      let _ = graze := (p1_char, h) :: (p2_char, h) :: !graze in
      check_help p1 p2 colls graze t

    else if (check_graze h p1_char) then
      let _ = graze := (p1_char, h) :: !graze in
      check_help p1 p2 colls graze t

    else if (check_graze h p2_char) then
      let _ = graze := (p2_char, h) :: !graze in
      check_help p1 p2 colls graze t

    else
      check_help p1 p2 colls graze t

  end

let check_contacts (data : game_data) 
  : (player_char * bullet) list * (player_char * bullet) list =

  let collisions : (player_char * bullet) list ref = ref [] in
  let grazes : (player_char * bullet) list ref = ref [] in
  let (team1, team2, _, bullets, _) = data in
  let (_,_,_,_,_,p1) = team1 in
  let (_,_,_,_,_,p2) = team2 in
  let _ = check_help (p1, false) (p2, false) collisions grazes bullets in
  (!collisions, !grazes)

(* update bullet position and velocity *)
let rec update (bullets : bullet list) : bullet list =
  match bullets with
  | [] -> []
  | h::t -> 
    let (vx, vy) = h.b_vel in
    let (ax, ay) = h.b_accel in
    let (px, py) = h.b_pos in
    let pos' = add_v (px, py) (vx, vy) in
    (* check to see if the bullet in in bounds*)
    if in_bounds pos' then
      (* update the position of the bullet on the gui *)
      let _ = add_update (MoveBullet(h.b_id,pos')) in
      let vel' = add_v (vx, vy) (ax, ay) in
      {h with b_pos = pos'; b_vel = vel'} :: update t
    else
      (* remove the bullet from the list and the gui *)
      let _ = add_update (DeleteBullet(h.b_id)) in
      update t

(*remove the bullet b from the bullet list lst*)
let rec remove_bullet (b : bullet) (lst : bullet list) : bullet list =
  match lst with
  | [] -> []
  | h::t ->
    if h.b_id = b.b_id then
      (* remove bullet from the gui *)
      let _ = add_update (DeleteBullet(b.b_id)) in
      t
    else
      h::(remove_bullet b t)

let build_targets_bubble acc orig_v =
  orig_v::acc

let build_targets_spread acc orig_v =
  let rec r_build_targets_spread acc' i =
    if i = cSPREAD_NUM+1 then acc'
    else
      let new_v = rotate_deg orig_v (float_of_int ((360/cSPREAD_NUM)*i)) in
      r_build_targets_spread (new_v::acc') (i+1) in
  r_build_targets_spread acc 0

let build_targets_trail acc orig_v =
  (rotate_deg orig_v (float_of_int cTRAIL_ANGLE))::
  (rotate_deg orig_v (float_of_int(360-cTRAIL_ANGLE)))::
  (orig_v)::acc

(* calculates velocity *)
let calc_vel vector speed : velocity =
  scale speed (unit_v vector)

(* calculates capped acceleration *)
let calc_acc acc : acceleration =
  if (magnitude acc) >= cACCEL_LIMIT then (0.,0.) else acc

(* end *)