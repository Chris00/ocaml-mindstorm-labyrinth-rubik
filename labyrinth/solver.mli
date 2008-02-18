(* File: solver.mli

   Copyright (C) 2008

     Julie de Pril <julie_87@users.sourceforge.net>

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


(** Functor using a [Labyrinth] module to develop some strategies to
    find the exit of a labyrinth.  *)
module Make(C: sig
              val conn : Mindstorm.bluetooth Mindstorm.conn
              val motor_left : Mindstorm.Motor.port
              val motor_right : Mindstorm.Motor.port
              val motor_ultra : Mindstorm.Motor.port
              val light_port : Mindstorm.Sensor.port
              val ultra_port : Mindstorm.Sensor.port
              val switch_port : Mindstorm.Sensor.port

              module Labyrinth : Labyrinth.T
            end) :
sig

  val run_loop : unit -> unit

  val next_case_to_explore : unit -> Labyrinth.dir list
    (** [next_case_to_explore()] returns the possible directions... *)

  type cont = unit -> unit
    (** Continuations taken by the fonctions. *)

  val look_wall_back : cont -> unit
    (** [look_wall_back k] makes the robot look behind it,
        and then continues with [k]. *)

  val look_walls : cont  -> unit
    (** [look_walls k] makes the robot look around in the 3 directions,
        and then continues with [k]. *)

  val follow_path: cont -> Labyrinth.dir list -> unit
    (** [follow_path k p] makes the robot follow the path [p], and then
        continues with [k]. *)

  val go_left : cont -> unit
    (** [go_left k] makes the robot go to the square on the left of it,
        and then continues with [k]. *)

  val go_right : cont -> unit
    (** [go_right k] makes the robot go to the square on the right of it,
        and then continues with [k]. *)

  val go_straight : cont -> unit
    (** [go_straight k] makes the robot go to the square in front of it,
        and then continues with [k]. *)

  val go_back : cont -> unit
    (** [go_back k] makes the robot go to the square behind it,
        and then continues with [k]. *)

end
