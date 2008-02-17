(* File: labyrinth.ml

   Copyright (C) 2008

     Marc Ducobu <el_marcu@users.sourceforge.net>

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

module type T =
sig
  type dir = [`N | `S | `E | `W]
  type dir_rel = [`Left | `Front | `Right | `Back]

  module Coord :
  sig
    type t = int*int
    val compare : t -> t -> int
    val nbh : t -> (dir*t) list
  end

  val nbh_explored : Coord.t -> (dir * Coord.t) list
  val nbh_unexplored : Coord.t -> (dir * Coord.t) list
  val wall_on : Coord.t -> dir -> [`True | `False | `Unknown]
  val status : Coord.t -> [`Explored | `Cross_roads | `Non_explored]

  val position : unit -> Coord.t
  val robot_dir : unit -> dir
  val rel_dir : dir -> dir_rel
  val abs_dir : dir_rel -> dir
  val set_wall : dir_rel -> [`True | `False | `Unknown] -> unit
  val move :  [`Left | `Front | `Right | `Back] -> unit
end

(*************************************************************************
 *                           Implementation
 *************************************************************************)

type dir = [`N | `S | `E | `W]
type dir_rel = [`Left | `Front | `Right | `Back]
type state = [`Explored | `Cross_roads | `Non_explored]
type wall  = [`True | `False | `Unknown]
type square = { mutable s_state : state;
                mutable wall_left: wall;
                mutable wall_top: wall; }

let taille_lab = 7

module Coord =
struct
  type t = int * int

  let compare (a, b) (c, d) =
    if (a < c) || ((a = c) && (b < d)) then -1
    else if (a = c) && (b = d) then 0
    else 1

  let nbh (i,j) =
    [(`N, (i+1, j)); (`S, (i-1, j)); (`E, (i, j+1)); (`W, (i, j-1))]
end

(* Global state of this module: what we know of the labyrinth and the
   state of the robot. *)
let lab =
  let n = 2 * taille_lab + 1 in
  Array.init n (fun _ -> Array.init n (fun _ -> { s_state = `Non_explored;
                                                wall_left = `Unknown;
                                                wall_top = `Unknown; } ))

let current_pos = ref (taille_lab, taille_lab)

let robot_orient = ref `N

let verif_in_lab (i,j) =
  if i > 2 * Array.length lab || i < 0 || j > 2 * Array.length lab.(0) || j < 0
  then failwith "Position not in the labyrinth"
;;

let wall_on ((i,j) as pos) d =
  verif_in_lab pos;
  match d with
  | `N -> lab.(i).(j).wall_top
  | `S -> lab.(i-1).(j).wall_top
  | `W -> lab.(i).(j).wall_left
  | `E -> lab.(i).(j+1).wall_left

let status ((i,j) as pos) =
  verif_in_lab pos;
  lab.(i).(j).s_state

let position () =
  let (i,j) = !current_pos in (i - taille_lab, j - taille_lab)

let robot_dir () = !robot_orient

(* Attribute numbers (mod 4) to the directions in a clockwise fashion. *)
let int_of_dir = function `N -> 0 | `E -> 1 | `S -> 2 | `W -> 3
let int_of_dir_rel = function `Front -> 0 | `Right -> 1 | `Back -> 2 | `Left -> 3

let rel_dir (dir:dir) : dir_rel =
  match (int_of_dir dir - int_of_dir !robot_orient + 4) mod 4 with
  | 0 -> `Front
  | 1 -> `Right
  | 2 -> `Back
  | _ -> `Left

let abs_dir (dir:dir_rel) : dir =
  match (int_of_dir_rel dir + int_of_dir !robot_orient) mod 4 with
  | 0 -> `N
  | 1 -> `E
  | 2 -> `S
  | _ -> `W

let nbh_explored ((i,j) as pos0) =
  verif_in_lab pos0;                   (* raise exn if not in lab *)
  let add_if_explored (dir:dir) pos nbh =
    if wall_on pos0 dir = `False && status pos = `Explored then
      (dir, pos) :: nbh
    else nbh in
  let nbh = add_if_explored `N (i+1,j) [] in
  let nbh = add_if_explored `S (i-1,j) nbh in
  let nbh = add_if_explored `E (i,j+1) nbh in
  add_if_explored `W (i,j-1) nbh

let nbh_unexplored ((i,j) as pos0) =
  verif_in_lab pos0;                   (* raise exn if not in lab *)
  let add_if_unexplored dir pos nbh =
    if wall_on pos0 dir <> `True && status pos = `Non_explored then
      (dir, pos) :: nbh
    else nbh in
  let nbh = add_if_unexplored `N (i+1,j) [] in
  let nbh = add_if_unexplored `S (i-1,j) nbh in
  let nbh = add_if_unexplored `E (i,j+1) nbh in
  add_if_unexplored `W (i,j-1) nbh


let set_wall (d:dir_rel) (w:wall) =
  let (i,j) = !current_pos in
  match abs_dir d with
  | `N -> lab.(i).(j).wall_top <- w
  | `S -> lab.(i-1).(j).wall_top <- w
  | `W -> lab.(i).(j).wall_left <- w
  | `E -> lab.(i).(j+1).wall_left <- w

let move d =
  let explored =
    List.fold_left (fun a (d,p) -> a && (wall_on !current_pos d = `True
                                       || status p <> `Non_explored)
                   ) true (Coord.nbh !current_pos) in
  let (i,j) = !current_pos in
  lab.(i).(j).s_state <- if explored then `Explored else `Cross_roads;
  let d_abs = abs_dir d in
  robot_orient := d_abs;
  current_pos := (match d_abs with
                  | `N -> (i+1, j)
                  | `S -> (i-1, j)
                  | `W -> (i, j-1)
                  | `E -> (i, j+1))



(* Local Variables: *)
(* compile-command: "make -k labyrinth.cmo" *)
(* End: *)
