(* File: run.ml

   Copyright (C) 2008

     Christophe Troestler <chris_77@users.sourceforge.net>
     WWW: http://math.umh.ac.be/an/software/

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

(* module Search = A_star *)
module Search = Ida
module Solver1 = Search.Make(Phase1)
module Solver2 = Search.Make(Phase2)

(********* Helpers *********)
let move1 = Cubie.move
let move2 = Phase2.Move.move

let string gen s =
  let string_of_move (g,i) = sprintf "%c%i" (char_of_generator g) i in
  String.concat " " (List.map (fun m -> string_of_move(gen m)) s)

(********* Modules for the physical part *********)
module Motor = Mindstorm.Motor

(* let conn = let bt = *)
(*   if Array.length Sys.argv < 2 then ( *)
(*     printf "%s <bluetooth addr>\n" Sys.argv.(0); *)
(*     exit 1; *)
(*   ) *)
(*   else Sys.argv.(1) in Mindstorm.connect_bluetooth bt *)

(* module C = *)
(* struct *)
(*   let conn = conn *)
(*   let motor_fighter = Motor.a *)
(*   let motor_hand = Motor.b *)
(*   let motor_pf = Motor.c *)
(*   let push_hand_port = `S2 *)
(*   let push_fighter_port = `S1 *)
(*   let cog_is_set_left = true *)
(* end *)

(*module M = Translator.Make(C)*)

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

let mul_and_print move c m =
  let cube = Cubie.mul c (move  m) in
(*   print_cube cube; *)
(*   Unix.sleep 2; *)
  cube

(********* Test *********)
let () =
  let moves = [F,3; R,2; U,1; B,3; D,1; L,2; R,3; U,2; R,3; (*B,1; F,3; F,1;
                                                              R,1; U,3; B,1; D,2; L,3; B,2*)] in
  let moves = List.map (fun m -> Cubie.move (Move.make m)) moves in
  let cube = List.fold_left Cubie.mul Cubie.id moves in
  display_cube cube;

  (********* Phase 1 *********)
  let cubeP1 = Phase1.of_cube cube in

  printf "Sequence phase 1:\n%!";
  let seq1 = Solver1.search_seq_to_goal cubeP1 Phase1.max_moves in
  List.iter (fun sol -> printf "%s\n" (string Phase1.Move.generator sol)) seq1;

  (********* Phase 2 *********)
  let apply1 sol = List.fold_left (fun c m -> Cubie.mul c (move1 m)) cube sol in
  let cubesP2 = List.map (fun s -> (Phase2.of_cube(apply1 s)), s) seq1 in

  printf "Sequence phase 2:\n%!";
  (* let seq2 = Solver2.search_seq_to_goal cubeP2 Phase2.max_moves in *)
  let seq2 = Solver2.multiple_search (List.map fst cubesP2) Phase2.max_moves in
  List.iter (fun (_,sol) ->
               printf "%s\n" (string Phase2.Move.generator sol)
            ) seq2;

  let solutions =
    List.map (fun (init, sol) -> (List.assoc init cubesP2, sol)) seq2 in

  let apply2 cube sol =
    List.fold_left (fun c m -> Cubie.mul c (move2 m)) cube sol in

  List.iter begin fun (s1,s2) ->
    let goal = apply2 (apply1 s1) s2 in
    printf "%s | %s => %s\n%!"
      (string Phase1.Move.generator s1)
      (string Phase2.Move.generator s2)
      (if Cubie.is_identity goal then "OK" else "KO");
    display_cube goal;
    ignore(wait_next_event [Button_down])
  end solutions;


  (********* Physical part *********)
  (*  List.iter M.make (List.map Phase1.Move.generator seq1);
      List.iter M.make (List.map Phase2.Move.generator seq2)*)

(*   flush stdout; *)
(*   ignore(wait_next_event [Button_down]) *)

