(* File: rubik.mli

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

(** Rubik cube solvers. *)

type generators = F | B | L | R | U | D
    (** Standard Rubik group genrrators.  The letters respectively
        stand for the clockwise rotations of the faces `Front',
        `Back', `Left', `Right', `Up' and `Down'. *)


(** Various coordinate systems with corresponding multipliction
    tables.  *)
module Coord :
sig
  (** The {{:http://kociemba.org/math/cubielevel.htm}cubies
      coordinates}.  *)
  module Cubies :
  sig
    type t
      (** A permutation of the Rubik cube (i.e. an element of the Rubik
          group).  This is immutable. *)

    val mul : t -> generators -> t
      (** Right multiply by a generator. *)

    val is_identity : t -> bool
      (** The premutation is the Rubik cube home state i.e., the
          identity (neutral element of the group). *)
  end
end
