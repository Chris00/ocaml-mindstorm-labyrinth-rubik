(* File: labyrinth.mli

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


(** Current knowledge of the labyrinth and of the robot position. *)

(** The signature of a "Labyrinth" module. *)
module type T =
sig
  type dir = [`N | `S | `E | `W]
  type dir_rel = [`Left | `Front | `Right | `Back]

  module Coord :
  sig
    type t = int * int
        (** Position [(x,y)] of the robot relative to its initial
            position.  (The coordinates of the initial position thus
            being [(0,0)]).  *)

    val compare : t -> t -> int
      (** Lexicographic comparison function. *)
    val nbh : t -> (dir * t) list
      (** [Coord.nbh xy] return the directions and coordinates of all
          neighbors of [xy]. *)
  end

  val nbh_explored : Coord.t -> (dir * Coord.t) list
  val nbh_unexplored : Coord.t -> (dir * Coord.t) list
  val wall_on : Coord.t -> dir -> [`True | `False | `Unknown]
  val status : Coord.t -> [`Explored | `Cross_roads | `Non_explored]

  val robot_pos : unit -> Coord.t
    (** [robot_pos()] returns the current position [(x,y)] of the
        robot relative to its initial position. *)
  val robot_dir : unit -> dir
    (** [robot_dir()] returns the current orientation of the front of
        the robot.  *)
  val rel_dir : dir -> dir_rel
  val abs_dir : dir_rel -> dir

  val set_wall : dir_rel -> bool -> unit
    (** [set_wall d] store in the labyrinth whether or not there is a
        wall in the direction [d] of the current position of the
        robot.  *)

  val move : dir_rel -> unit
end

include T
