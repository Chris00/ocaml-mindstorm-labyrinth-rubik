(* File: display_base.mli

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

open Rubik

type color = int
    (** RGB color compatible with the [Graphic] module. *)

type geom = {
  xy0 : float * float;
  (** Coordinates of the bottom left corner of smaller rectangle
      surrounding the cube figure. *)
  width : float;
  (** The width of a (non slanted) facelet *)
  height : float;
  (** The height of a (non slanted) facelet *)
  angle : int;
  (** Angle, in degrees, of the right and up faces *)
  color_F : color;
  color_B : color;
  color_L : color;
  color_R : color;
  color_U : color;
  color_D : color;
  color_lines : color;
  (** The colors of the 6 faces and of the lines.  *)
}


val geom : geom
  (** Default values for the cube representation. *)

val cube : ?geom:geom -> Cubie.t -> unit

val cube_tikz : out_channel -> ?geom:geom -> Cubie.t -> unit
