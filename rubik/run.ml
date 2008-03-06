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

module Search = A_star
module Solver1 = Search.Make(Rubik.Phase1)
module Solver2 = Search.Make(Rubik.Phase2)

(********* Helpers *********)

let print gen s =
  let string_of_move (g,i) = sprintf "%c%i" (char_of_generator g) i in
  let s = String.concat " " (List.map (fun m -> string_of_move(gen m)) s) in
  printf "Moves: %s\n%!" s


(********* Modules for the physical part *********)
module Motor = Mindstorm.Motor

let conn =
  let bt =
    if Array.length Sys.argv < 2 then (
      printf "%s <bluetooth addr>\n" Sys.argv.(0);
      exit 1;
    )
    else Sys.argv.(1) in
  Mindstorm.connect_bluetooth bt

module C =
struct
  let conn = conn
  let motor_fighter = Motor.a
  let motor_hand = Motor.b
  let motor_pf = Motor.c
  let push_hand_port = `S2
  let push_fighter_port = `S1
  let cog_is_set_left = true
end

module M = Translator.Make(C)

(********* Test *********)
let () =
  let moves = [F,3; R,2; U,1; B,3; D,1; L,2; R,3; U,2; F,2; B,1; L,3; F,1;
               R,1; U,3; B,1; D,2; L,3; B,2] in
  let moves = List.map (fun m -> Cubie.move (Move.make m)) moves in
  let cube = List.fold_left Cubie.mul Cubie.id moves in

  (********* Phase 1 *********)
  let cubeP1 = Phase1.of_cube cube in

  printf "Sequence phase 1: \n%!";
  let seq1 = Solver1.search_seq_to_goal cubeP1 Phase1.max_moves in
  print Phase1.Move.generator seq1;


  let cube2 =
    List.fold_left (fun c m -> Cubie.mul c (Cubie.move m)) cube seq1 in

  Gc.major();

  (********* Phase 2 *********)
  let cubeP2 = Phase2.of_cube cube2 in

  printf "Sequence phase 2: \n%!";
  let seq2 = Solver2.search_seq_to_goal cubeP2 Phase2.max_moves in
  print Phase2.Move.generator seq2;

  let goal =
    List.fold_left (fun c m -> Cubie.mul c (Phase2.Move.move m)) cube2 seq2 in

  if Cubie.is_identity goal then printf "Wouhouuu!! \n%!";

  (********* Physical part *********)
  List.iter M.make (List.map Phase1.Move.generator seq1);
  List.iter M.make (List.map Phase2.Move.generator seq2)
