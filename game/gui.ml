open Definitions
open Constants
open Netgraphics
open Util

(* helper function to add a list of bullets to the gui *)
let rec gui_add_bullets (bullets : bullet list) : unit =
  match bullets with
  | [] -> ()
  | h::t ->
    let _ = add_update (AddBullet(h.b_id, h.b_color, h.b_type, h.b_pos)) in
    gui_add_bullets t

(*method to remove all bullets from a bullet list from the gui*)
let rec gui_clear_bullets (bullets : bullet list) : unit =
  match bullets with
  | [] -> ()
  | h::t ->
    let _ = print_endline(string_of_int(h.b_id)) in
    let _ = add_update (DeleteBullet(h.b_id)) in
    gui_clear_bullets t