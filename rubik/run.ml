(* File: run.ml

   Copyright (C) 2008

     Julie de Pril <julie_87@users.sourceforge.net>
     Dany Maslowski <dan_86@users.sourceforge.net>
     Marc Ducobu <el_marcu@users.sourceforge.net>

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.1 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

open Printf
open Rubik
module Motor = Mindstorm.Motor

let conn = let bt =
  if Array.length Sys.argv < 2 then (
    printf "%s <bluetooth addr>\n" Sys.argv.(0);
    exit 1;
  )
  else Sys.argv.(1) in Mindstorm.connect_bluetooth bt

module Brick =
struct
  let conn = conn
  let motor_fighter = Motor.a
  let motor_hand = Motor.b
  let motor_pf = Motor.c
  let push_hand_port = `S2
  let push_fighter_port = `S1
  let cog_is_set_left = true
end

module M = Translator.Make(Brick)

(********* Graphical part *********)
open Graphics

let x0 = 10
and y0 = 20
and len_sq = 30
let cube_colors = (blue, magenta, yellow, red, white, green)

let () =
  let img_width = x0 + 12 * len_sq + 180 in
  let img_high = y0 + 9 * len_sq in
  open_graph (sprintf " %ix%i-100+50" img_width img_high);
  set_window_title "Rubik cube";
  set_color (rgb 219 219 219);
  fill_rect 0 0 img_width img_high


let display_cube cube = Display.cube x0 y0 cube_colors len_sq cube

(* Compute a movement of the cube and display graphically the
   resulting cube. *)
let mul_and_print move c m =
  let cube = Cubie.mul c (move  m) in
  display_cube cube;
  Unix.sleep 2;
  cube


let () =
  let cubie = Init_color.create_rubik (M.face_iter) in
  display_cube cubie;

  let moves = [F,3; R,2; U,1; B,3; D,1; L,2; R,3; U,2; R,3; B,1;
               F,3; F,1; R,1; U,3; B,1; D,2; L,3; B,2 ] in
  (* Patterns from http://www.math.ucf.edu/~reid/Rubik/patterns.html *)
  (* let moves = [F,2; B,2; R,2; L,2; U,2; D,2] in *)
  (* let moves = [U,3; L,3; U,3; F,3; R,2; B,3; R,1; F,1; U,1; B,2;
               U,1; B,3; L,1; U,3; F,1; U,1; R,1; F,3] in *)
  (*let moves = [R,2; L,3; D,1; F,2; R,3; D,3; R,3; L,1; U,3; D,1; R,1;
               D,1; B,2; R,3; U,1; D,2] in*)

  let moves = List.map (fun m -> Cubie.move (Move.make m)) moves in
  let cube = List.fold_left Cubie.mul Cubie.id moves in
  display_cube cube;


  let solution = Solver.find_first cube in


  (********* Physical part *********)
  (*  List.iter M.make (List.map Phase1.Move.generator seq1);
      List.iter M.make (List.map Phase2.Move.generator seq2)*)

(*   flush stdout; *)
  ignore(wait_next_event [Button_down])

