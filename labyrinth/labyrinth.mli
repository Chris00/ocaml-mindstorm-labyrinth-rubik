(* File : labyrinth.mli *)

module type T =
sig
  type dir = [`N | `S | `E | `W]
  type dir_rel = [`Left | `Front | `Right | `Back]

  module Coord :
  sig
    type t = int * int
    val compare : t -> t -> int
    val nbh : t -> (dir*t) list
  end

  val nbh_explored : Coord.t -> (dir * Coord.t) list
  val nbh_unexplored : Coord.t -> (dir * Coord.t) list
  val wall_on : Coord.t -> dir -> [`True | `False | `Unknown]
  val status : Coord.t -> [`Explored | `Cross_roads | `Non_explored]

  val position : unit -> Coord.t
    (** [position()] returns the current position of the robot
        relative to its initial position. *)
  val robot_dir : unit -> dir
  val rel_dir : dir -> dir_rel
  val abs_dir : dir_rel -> dir
  val set_wall : dir_rel -> [`True | `False | `Unknown] -> unit
  val move : dir_rel -> unit
end

include T
