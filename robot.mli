(* File: robot.mli

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

type t
  (** Mutable robot state to be used in an event loop. *)

val make : unit -> t
  (** Make a new robot (with its own event loop). *)

val run : t -> unit
  (** [run r] runs the robot [r] (i.e. starts its event loop). *)

type 'a meas
  (** Holds a measure of type ['a] from the robot. *)

val meas : t -> (unit -> 'a) -> 'a meas
  (** [meas r get] define a new measure for the robot [r], [get()]
      being executed when this measure is needed by [r]. *)

val event : 'a meas -> ('a -> bool) -> ('a -> unit) -> unit
  (** [event m cond f] schedules the [f v] to be executed when the
      value [v] of the measure [m] satisfies [cond v]. *)
