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
let laby_structure = rgb 171 183 227
let goal_color = yellow
let text_font = "-*-times new roman-medium-r-normal--100-0-0-0-p-0-iso8859-1"
let text_success = "Trouvé !"

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
    set_window_title "Labyrinth";
    (* Draw lines to suggest the labyrinth *)
    set_color laby_structure;
    for x = -7 to 7 do
      moveto (x0 + x * dx) (y0 - 7 * dy);  rlineto 0 (14 * dy)
    done;
    for y = -7 to 7 do
      moveto (x0 - 7 * dx) (y0 + y * dy);  rlineto (14 * dx) 0
    done;
    (* Draw initial robot *)
    draw_square (robot_pos());
    draw_robot()

  (* Redefine some functions of [L] to add a graphical animation *)

  (* @override *)
  let set_wall (d: dir_rel) b =
    L.set_wall d b;
    set_color (if b then wall_color else explored_color);
    let (rx,ry) = robot_pos() in
    let px = x0 + rx * dx and py = y0 + ry * dy in
    match abs_dir d with
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


  (* @override *)
  let move (d: dir_rel) =
    let old_pos = robot_pos() in
    L.move d;
    draw_square old_pos;                (* clear current square *)
    draw_square (robot_pos());
    draw_robot();
  ;;

  (* New functions *)
  let success () =
    let (x,y) = robot_pos() in
    let px = x0 + x * dx + wall_thickness
    and py = y0 + y * dy + wall_thickness in
    set_color goal_color;
    fill_rect px py square_length square_length;
    draw_robot();
    set_font text_font;
    set_color black;
    let (w,h) = text_size text_success in
    moveto x0 y0;  rmoveto (-w / 2) (-h /2);  draw_string text_success

  let close_when_clicked () =
    ignore(wait_next_event [Button_down])
end


(* Local Variables: *)
(* compile-command: "make -k display.cmo" *)
(* End: *)

