(* File: display.ml

   Copyright (C) 2008

     Christophe Troestler <Christophe.Troestler@umh.ac.be>
     WWW: http://math.umh.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.1 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

module type T =
sig
  include Labyrinth.T
  val success : unit -> unit
  val failure : unit -> unit
  val draw_path : dir list -> unit
  val close_when_clicked : unit -> unit
end

open Graphics

let (x0,y0) = (350, 350)                (* initial position *)
let square_length = 40 (* pixels, excluding walls *)
let robot_color = magenta
let wall_thickness = 2 (* pixels; wall are twice as thick *)
let wall_color = black
let explored_color = rgb 166 227 147    (* green *)
let cross_road_color = red
let path_color = rgb 49 147 192
let laby_structure = rgb 171 183 227
let text_font = "-*-times new roman-medium-r-normal--100-0-0-0-p-0-iso8859-1"
let goal_color = yellow
let text_success = "Trouv� !"
let failure_color = red
let text_failure = "Pas de sortie !"

let dx = square_length + 2 * wall_thickness
let dy = dx

module Make(L : Labyrinth.T) : T =
struct
  include L

  let draw_square ((x,y) as xy) =
    let px = x0 + x * dx + wall_thickness
    and py = y0 + y * dy + wall_thickness in
    set_color (match status xy with
               | `Explored | `Cross_roads -> explored_color
               | `Non_explored ->  background);
    fill_rect px py square_length square_length;
    if status xy = `Cross_roads then begin
      (* Add a special mark on "crossroads" *)
      set_color cross_road_color;
      moveto px py;  rlineto square_length square_length;
      moveto px (py + square_length);  rlineto square_length (- square_length)
    end

  (** Draw the robot according to its state in [L]. *)
  let draw_robot () =
    let d = 3 in
    let d2 = 2 * d in
    let w = square_length / 2 in
    let poly = match robot_dir() with
      | `N -> [| (d2,d); (w, square_length - d); (square_length - d2,d) |]
      | `S -> [| (d2, square_length - d); (w,d);
                 (square_length - d2, square_length - d) |]
      | `W -> [| (square_length - d, d2); (d, w);
                 (square_length - d, square_length - d) |]
      | `E -> [| (d,d2); (square_length - d, w); (d, square_length - d) |] in
    let (x,y) = robot_pos() in
    let px = x0 + x * dx + wall_thickness
    and py = y0 + y * dy + wall_thickness in
    set_color robot_color;
    fill_poly (Array.map (fun (x,y) -> (x + px, y + py)) poly)
  ;;

  let () =
    (* Initialize the graphic window *)
    open_graph " 700x700";
    set_window_title "Labyrinth - visualisation";
    (* Draw lines to suggest the labyrinth *)
    set_color laby_structure;
    for x = -7 to 7 do
      moveto (x0 + x * dx) (y0 - 7 * dy);  rlineto 0 (14 * dy)
    done;
    for y = -7 to 7 do
      moveto (x0 - 7 * dx) (y0 + y * dy);  rlineto (14 * dx) 0
    done;
    (* Draw initial robot *)
    set_line_width 1;
    draw_square (robot_pos());
    draw_robot()

  let draw_wall (rx,ry) (d: dir) b =
    set_color (if b then wall_color else explored_color);
    let px = x0 + rx * dx and py = y0 + ry * dy in
    match d with
    | `N ->
        let x = px + wall_thickness
        and y = py + wall_thickness + square_length in
        fill_rect x y square_length (2 * wall_thickness)
    | `S ->
        let x = px + wall_thickness
        and y = py - wall_thickness in
        fill_rect x y square_length (2 * wall_thickness)
    | `W ->
        let x = px - wall_thickness
        and y = py + wall_thickness in
        fill_rect x y (2 * wall_thickness) square_length
    | `E ->
        let x = px + wall_thickness + square_length
        and y = py + wall_thickness in
        fill_rect x y (2 * wall_thickness) square_length

  (* Redefine some functions of [L] to add a graphical animation *)

  (* @override *)
  let set_wall (d: dir_rel) b =
    L.set_wall d b;
    draw_wall (robot_pos()) (abs_dir d) b

  (* @override *)
  let move (dir: dir_rel) =
    let old_pos = robot_pos() in
    L.move dir;
    draw_square old_pos;                (* clear current square *)
    (* draw_square (robot_pos()); *)
    (* We need to redraw all neighboring squares because they may have
       been updated by the move.  Redraw also the non-walls paths in
       order to erase a possible path. *)
    let redraw (d,p) =
      draw_square p;
      if wall_on old_pos d = `False then draw_wall old_pos d false in
    List.iter redraw (Coord.nbh old_pos);
    draw_robot();
  ;;

  (* New functions
   ***********************************************************************)

  let draw_final color text =
    let (x,y) = robot_pos() in
    let px = x0 + x * dx + wall_thickness
    and py = y0 + y * dy + wall_thickness in
    set_color color;
    fill_rect px py square_length square_length;
    draw_robot();
    (try set_font text_font with _ -> ()); (* ignore nonexistent font *)
    set_color black;
    let (w,h) = text_size text in
    moveto x0 y0;  rmoveto (-w / 2) (-h /2);  draw_string text_success

  let success () = draw_final goal_color text_success
  let failure () = draw_final failure_color text_failure

  let draw_path dirs =
    let pos = robot_pos() in
    let path (l,p) d = let p' = Coord.move p d in (p' :: l, p') in
    let squares, _ = List.fold_left path ([pos], pos) dirs in
    let mid_square = wall_thickness + square_length / 2 in
    let xy = List.map (fun (x,y) ->
                         (x0 + x * dx + mid_square, y0 + y * dy + mid_square)
                      ) squares in
    set_line_width 3;
    set_color path_color;
    draw_poly (Array.of_list xy);
    set_line_width 1



  let close_when_clicked () =
    ignore(wait_next_event [Button_down])
end


(* Local Variables: *)
(* compile-command: "make -k display.cmo" *)
(* End: *)

