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
    val move :  t -> dir -> t
    val nbh : t -> (dir*t) list
  end

  val nbh_explored : Coord.t -> (dir * Coord.t) list
  val nbh_unexplored : Coord.t -> (dir * Coord.t) list
  val wall_on : Coord.t -> dir -> [`True | `False | `Unknown]
  val status : Coord.t -> [`Explored | `Cross_roads | `Non_explored]

  val robot_pos : unit -> Coord.t
  val robot_dir : unit -> dir
  val rel_dir : dir -> dir_rel
  val abs_dir : dir_rel -> dir
  val set_wall : dir_rel -> bool -> unit
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
                mutable wall_W: wall;
                mutable wall_N: wall; }

module Coord =
struct
  type t = int * int

  let compare (a, b) (c, d) =
    if (a < c) || ((a = c) && (b < d)) then -1
    else if (a = c) && (b = d) then 0
    else 1

  let move (x,y) = function
    | `N -> (x, y+1)
    | `S -> (x, y-1)
    | `W -> (x-1, y)
    | `E -> (x+1, y)

  let nbh xy =
    let c = move xy in [(`N, c `N); (`S, c `S); (`E, c `E); (`W, c `W)]
end

(* For the current realisation, it is enough but in general a more
   extensible datastructure is needed.  We have chosen this for simplicity. *)
let taille_lab = 8
let i0 = taille_lab
let j0 = taille_lab

(* Global state of this module: what we know of the labyrinth and the
   state of the robot. *)
let lab =
  let n = 2 * taille_lab + 1 in
  let make_square _ = { s_state = `Non_explored;
                        wall_W = `Unknown;
                        wall_N = `Unknown; } in
  Array.init n (fun _ -> Array.init n make_square)

let current_pos = ref (0, 0)

let robot_orient = ref `N

let lab_coord (x,y) =
  let i = i0 + x and j = j0 + y in
  if i > Array.length lab || i < 0 || j > Array.length lab.(0) || j < 0
  then failwith "Position not in the labyrinth matrix";
  (i,j)
;;

let wall_on xy d =
  let (i,j) = lab_coord xy in
  match d with
  | `N -> lab.(i).(j).wall_N
  | `S -> lab.(i).(j-1).wall_N
  | `W -> lab.(i).(j).wall_W
  | `E -> lab.(i+1).(j).wall_W

let status xy =
  let (i,j) = lab_coord xy in
  lab.(i).(j).s_state

let robot_pos () = !current_pos

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

let nbh_explored xy0 =
  let add_if_explored nbh (dir, xy) =
    if wall_on xy0 dir = `False && status xy <> `Non_explored then
      (dir, xy) :: nbh
    else nbh in
  List.fold_left add_if_explored [] (Coord.nbh xy0)

let nbh_unexplored xy0 =
  let add_if_unexplored nbh (dir, xy) =
    if wall_on xy0 dir <> `True && status xy = `Non_explored then
      (dir, xy) :: nbh
    else nbh in
  List.fold_left add_if_unexplored [] (Coord.nbh xy0)

let set_wall (d:dir_rel) w =
  let (i,j) = lab_coord !current_pos in
  let w = if w then `True else `False in
  match abs_dir d with
  | `N -> lab.(i).(j).wall_N <- w
  | `S -> lab.(i).(j-1).wall_N <- w
  | `W -> lab.(i).(j).wall_W <- w
  | `E -> lab.(i+1).(j).wall_W <- w

let opposite dir = match dir with
  | `N -> `S
  | `S -> `N
  | `E -> `W
  | `W -> `E


(* return [true] if neighboor are all explored, [dir_to_dont_check]
   is the absolute direction of the square to don't check. For the leaving
   square function it's the square where the robot is going to and for
   updating x-road fuction it's the square where the robot is.

   If a neighboor of [xy] is not explored the robot will return to the [xy]
   to explore the not explored neighboor. *)
let fully_explored xy dir_to_dont_check =
  let explored (dir,xy_e) =
    dir = dir_to_dont_check || wall_on xy dir = `True
    || status xy_e <> `Non_explored in
  List.fold_left (fun hist dir_xy -> hist && explored dir_xy) true (Coord.nbh xy)

let move d =
  let d_abs = abs_dir d in
  let (i,j) = lab_coord !current_pos in
  lab.(i).(j).s_state <-
    if fully_explored !current_pos d_abs then `Explored else `Cross_roads;
  robot_orient := d_abs;
  current_pos := Coord.move !current_pos d_abs;
  List.iter (fun (d, xy) ->
               let(i,j) = lab_coord xy in
               if status xy = `Cross_roads then
                 lab.(i).(j).s_state <-
                   if fully_explored xy (opposite d) then `Explored
                   else `Cross_roads;
            ) (Coord.nbh !current_pos)


(* Local Variables: *)
(* compile-command: "make -k labyrinth.cmo" *)
(* End: *)
