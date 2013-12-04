module Bullet = struct

let check_contacts (data : game_data) 
  : (player_char * bullet) list * (player_char * bullet) list =

  let collisions : (player_char * bullet) list ref = ref [] in
  let grazes : (player_char * bullet) list ref = ref [] in
  let (team1, team2, _, bullets, _) = data in
  let (_,_,_,_,_,p1) = team1 in
  let (_,_,_,_,_,p2) = team2 in
  let _ = check_help (p1, false) (p2, false) collisions grazes bullets in
  (!collisions, !grazes)

(*the helper function used in check contacts uses the refs passed
 *in as accumulators
 *)
let rec check_help (p1 : player_char * bool) (p2 : player_char * bool)
  (colls : (player_char * bullet) list ref) 
  (graze : (player_char * bullet) list ref)
  (bullets : bullet list) : () =

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
      else if p2_bools then
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


(*checks to see if the bullet b has collided with the player p*)
let check_collision (b : bullet) (p : player_char) : bool =
  let dist = distance b.b_pos p.p_pos in
  let min_dist = b.b_radius + p.p_radius in
  if dist < min_dist then true
  else false

(*checks to see if the bullet b has grazed the player (but not collided) p*)
let check_graze (b : bullet) (p : player_char) : bool =
  let dist = distance b.b_pos p.p_pos in
  let coll_dist = b.b_radius + p.p_radius in
  let min_dist = cGRAZE_RADIUS in
  if coll_dist <= dist & dist < min_dist then true
  else false

(* update bullet position and velocity *)
let rec update (bullets : bullet list) : bullet list =
  match bullets with
  | [] -> []
  | h::t -> 
    let (vx, vy) = h.b_vel in
    let (ax, ay) = h.b_accel in
    let (px, py) = h.b_pos in
    let pos' = add_v (px, py) (vx, vy) in
    let vel' = add_v (vx, vy) (ax, ay) in
    {h with b_pos = pos'; b_vel = vel'} :: update t
end