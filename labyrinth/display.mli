(* File: display.mli

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

module type T =
sig
  include Labyrinth.T

  val success : unit -> unit
    (** Call this function when the robot is on the final square.  *)

  val draw_path : dir list -> unit
    (** [draw_path] draw the path [p] from the current position on top
        of the labyrinth. *)

  val close_when_clicked : unit -> unit
    (** Pause until the graphic windows in clicked. *)
end

(** This functor add some visualisation (in an OCaml graphics window)
    to the Labyrinth operations.  A way to signal that we have found
    the labyrinth exit is also provided. *)
module Make : functor (L:Labyrinth.T) -> T
