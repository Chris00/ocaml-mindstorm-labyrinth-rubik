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

(** Standard Rubik group generators.  The letters respectively stand
    for the clockwise rotations of the faces `Front', `Back', `Left',
    `Right', `Up' and `Down'. *)
type generator = F | B | L | R | U | D


(** {2 Coordinate systems}

    Various coordinate systems with corresponding multipliction
    tables.  *)

(** The {{:http://kociemba.org/math/cubielevel.htm}cubies
    coordinates}.  *)
module Cubie :
sig
  type corner =
    | URF                               (** Up Right Front *)
    | UFL                               (** Up Front Left *)
    | ULB                               (** Up Left Back *)
    | UBR                               (** Up Back Right *)
    | DFR                               (** Down Front Right *)
    | DLF                               (** Down Left Front *)
    | DBL                               (** Down Back Left *)
    | DRB                               (** Down Right Back *)
  (** Naming of the 8 corner cubies.
      Here is a "graphical" representation of the naming scheme:
      {v
            ULB ---- UBR
             /|       /|
            / |      / |
           / DBL    /  |
         ULF ---- URF DRB
          |  /     |  /
          | /      | /
          |/       |/
         DLR ---- DFR			v}
      *)

  (** The 12 edge cubies. *)
  type edge =
    | UR                                (** Up Right *)
    | UF                                (** Up Front *)
    | UL                                (** Up Left *)
    | UB                                (** Up Back *)
    | DR                                (** Down Right *)
    | DF                                (** Down Front *)
    | DL                                (** Down Left *)
    | DB                                (** Down Back *)
    | FR                                (** Front Right *)
    | FL                                (** Front Left *)
    | BL                                (** Back Left *)
    | BR                                (** Back Right *)

  type t
    (** Construct a scrambling of the Rubik cube (i.e. a permutation
        of the home state, i.e. an element of the Rubik group).  It is
        immutable. *)

  val make : corner: (corner * int) list -> edge: (edge * bool) list -> t
    (** Construct a permutation of the Rubik cube.

        The [corner] argument is the list of corners and their
        orientation (0, 1, or 2) by which URF, UFL, ULB, UBR, DFR,
        DLF, DBL, DRB are replaced (in that order).  For example, for
        the [F] move, the URF cubie is replaced by the ULF and rotated
        120 degrees clockwise, so corner is [[(ULF,1); (DLF,2);
        (ULB,0); (UBR,0); (URF,2); (DFR,1); (DBL,0); (DRB,0)]].

        The [edge] argument is the same as the [corner] but for edge
        cubies (where the orientation is [true] or [false] depending
        of whether the edge is flipped or not).  The order in which
        the replacements are given is UR, UF, UL, UB, DR, DF, DL, DB,
        FR, FL, BL, BR.

        @raise Invalid_argument if the data does not describe a valid
        permuation of the cube.  *)

  val corner : t -> corner -> corner * int
    (** [corner cube c] returns the corner and its orientation by which
        [c] is replaced in the [cube]. *)
  val edge : t -> edge -> edge * bool
    (** [edge cube e] returns the edge and its flip state by which [e]
        is replaced in the [cube]. *)

  val move : generator -> t
    (** [move g] the  generator [g] expressed at the cubie level. *)

  val id : t
    (** The home state of the Rubik cube (i.e. the identity of the
        Rubik group).  *)

  val mul : t -> t -> t
    (** [mul g1 g2] is the group multiplication of [g1] and [g2].  It
        corresponds to permute the home cube with [g1] and then apply
        to the result the permutation represented by [g2]. *)

  val inv : t -> t
    (** [inv g] inverse permutation. *)

  val is_identity : t -> bool
    (** The premutation is the Rubik cube home state i.e., the
        identity (neutral element of the group). *)
end
