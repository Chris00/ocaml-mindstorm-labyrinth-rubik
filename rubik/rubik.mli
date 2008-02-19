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

(** Rubik cube group encodings. *)


(** {2 Rubik group generators} *)

(** Standard Rubik group generators. *)
type generator =
  | F     (** 90° CW turn of the {i front} face *)
  | B     (** 90° CW turn of the {i back} face *)
  | L     (** 90° CW turn of the {i left} face *)
  | R     (** 90° CW turn of the {i right} face *)
  | U     (** 90° CW turn of the {i up} face *)
  | D     (** 90° CW turn of the {i down} face *)

type move
  (** The basic moves that the generators allow: F, F^2, F^3, B, B^2,
      B^3, L, L^2, L^3,...  Remember that X^4 = 1 for all generators X. *)

val move : generator * int -> move
  (** [move (g, i)] returns the move corresponding to [g^i].
      @raise Invalid_Argument if [i < 1] or [i > 3]. *)

val generator : move -> generator * int
  (** [generator] is the inverse function of {!Rubik.move}. *)


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

  val move : move -> t
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


(** Coordinates are "tags" labelling the right cosets Hg of a (not
    necessarily normal) subgroup H.  A fast multiplication table is
    build to handle the multiplication by a generator to the right. *)
module type Coordinate =
sig
  type t
    (** Compact form of the coordinate for fast computations. *)

  val of_cube : Cubie.t -> t
    (** [of_cube cube] returns the coordinate of the [cube].  Note
        that this function is (usually) not one-to-one, i.e. these
        coordinates only give a partial characterization of a cube.  *)

  val mul : t -> move -> t
    (** [mul c m] apply the move [m] to the coordinate [c] i.e. right
        multiply the element [c] of the group by [m] (this is a coset
        so any element of the group with coordinate [c] will give the
        same coordinates for [c * m]).  BEWARE: before using this, you
        need to run {!Rubik.CornerO.initialize}. *)

  val initialize : ?file:string -> unit -> unit
    (** [initialize()] build the tables needed by [mul].

        @param file if the file exists, assumes it contains the table
        computed by a previous run.  If it does not exist, create it
        and save the computed tables. *)
end


(** Orientation of the corner cubies (requires initialisation). *)
module CornerO : Coordinate

(** Edge orientation (requires initialisation). *)
module EdgeO : Coordinate
