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
open Graphics
open Display_base

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

let () =
  open_graph "";
  (********* Initialization of the cubie *********)
  let (cubie, (cU, cL, cF, cR, cB, cD)) =
    Init_color.create_rubik (M.face_iter) in


  (********* Graphical part *********)
  let g_ref = geom in
  let g = {
    xy0 = (10.,10.);
    width = g_ref.width;
    height = g_ref.height;
    angle = g_ref.angle;
  } in
  let c = {
    color_F = cF;
    color_B = cB;
    color_L = cL;
    color_R = cR;
    color_U = cU;
    color_D = cD;
    color_lines = black
  } in

  let print_cubie cub = cube ~geom:g ~colors:c cub in

  open_graph "";
  clear_graph();
  print_cubie cubie;
  ignore(wait_next_event [Key_pressed]);

  (********* Resolution part *********)
  let solution = Solver.find_first cubie in

  let print_and_do c s =
    let status = wait_next_event[Poll] in
    if status.button then
      ignore(wait_next_event[Button_down]);
    let next = Cubie.mul c (Cubie.move (Move.make s)) in
    print_cubie next;
    M.make s;
    next
  in

  let cubie = List.fold_left print_and_do cubie solution in

  print_cubie cubie;
  ignore(wait_next_event [Key_pressed])
