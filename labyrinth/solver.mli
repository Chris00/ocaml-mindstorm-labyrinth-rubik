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
              val conn : 'a Mindstorm.conn
              val motor_left : Mindstorm.Motor.port
              val motor_right : Mindstorm.Motor.port
              val motor_ultra : Mindstorm.Motor.port
              val light_port : Mindstorm.Sensor.port
              val ultra_port : Mindstorm.Sensor.port

              module Labyrinth : Labyrinth.T
            end) :
sig

  val next_case_to_explore : unit -> Labyrinth.dir_rel list
    (** [next_case_to_explore()] return the possible directions... *)

  type cont = unit -> unit
    (** Continuations taken by the fonctions. *)

  val look_walls : cont  -> unit

  val follow_path: cont -> Labyrinth.dir_rel list -> unit

  val go_left : cont -> unit

  val go_right : cont -> unit

  val go_straight : cont -> unit

  val go_back : cont -> unit

end
