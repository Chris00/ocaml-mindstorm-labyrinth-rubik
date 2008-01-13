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

type conn = Mindstorm.bluetooth Mindstorm.conn

class ['a, 'b, 'c, 'd] event_loop :
  (conn -> 'a) -> (conn -> 'b) -> (conn -> 'c) -> (conn -> 'd) -> conn ->
object
  method addS1 : ('a -> bool) -> ('a -> unit) -> unit
  method addS2 : ('b -> bool) -> ('b -> unit) -> unit
  method addS3 : ('c -> bool) -> ('c -> unit) -> unit
  method addS4 : ('d -> bool) -> ('d -> unit) -> unit
  method run : unit -> unit

  method private reset : unit
end
