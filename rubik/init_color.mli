open Rubik
open Graphics

(** Initialize the rubik state and creating a representation of it!*)

type colorf = Red | Green | Yellow | White | Orange | Blue
(** Color of faces *)

module Face :
  (** Abstact a face of a rubik.
    Each facelets of a face is numbered in this way :
    -------------
    | 0 | 1 | 2 |
    -------------
    | 3 | 4 | 5 |
    -------------
    | 6 | 7 | 8 |
    -------------
  *)
sig
  type t
    (** Each face is define by nine colors  *)

  val coord : int -> int * int
    (** Give the coordinate and the absissa
        of the position [id] in the matrix face *)

  val id : int * int -> int
    (** Return the number of the facelets from an absissa and a coordinate *)

end

module Color :
(** Random funcion about the color face *)
sig
  val rgb_components : Graphics.color -> int * int * int
    (** Return the rgb components of a [Graphics.color] *)

  val name : int * int * int -> colorf
    (** Return form the rgb components [rgb], the name of the color *)

  val color_of : generator -> colorf
    (** Return the color of a face *)

  val color_fid : generator * int ->   colorf
    (** Return the color of [(face,id)] wehre [face] is a face of the rubik
        and [id] the identification number of the square.*)
end

module Pick :
(** Functions to pick the color on a snapshot. The picking area are squares of
    14px with 30 px between them. The left bottom corner of the left bottom
    square is on (43,23)  *)
sig
  val abs : int -> int
    (** absissa of the left bottom face square. [x] is equal to the absissa of
  [Face.coord_abs fid *)

  val ord : int -> int
    (** Ordinate of the left bottom face square. [x] is equal to the absissa of
        [Face.coord_abs fid] *)

  val pick_point : color array array -> int -> int -> (int * int * int) list
    (** Return a list of the rgb components of points belonging to a 14px
        square. [x0] is the absissa of the left bottom corner of the square
        and [y0] the ordinate. [snapshot] is a matrix representing the image.*)

  val take_face : Snapshot.webcam -> generator -> int -> unit
    (** Take the color of the face [face] with the orientation [orient]
        from the [snapshot] and save the data! *)
end

val find_orientation : colorf array -> colorf array -> int
  (** Returns the orientation of a corner or an edge [ce] in the place [pl].
      It returns [3] if it's impossible to place the corner*)

val find : colorf array -> (colorf array * 'a) list -> 'a * int

val  order : colorf array list -> (colorf array * 'a) list
  -> ('a * int) list

val corner_list_replacement : unit -> (Cubie.corner * int) list
  (** List of the corners and their orientation who replace in the real cubie
      this corners : URF, UFL, ULB, UBR, DFR, DLF, DBL, DRB
      This list is used for Rubik.Cubie.make which initialize
      the cubie for a solving search*)

val edge_list_replacement : unit -> (Cubie.edge * int) list
  (** List of the edges and their orientation who replace in the real cubie
      this edges : UR, UF, UL, UB, DR, DF, DL, DB, FR, FL, BL, BR
      This list is used for Rubik.Cubie.make which initialize
      the cubie for a solving search*)

val create_rubik : unit -> Cubie.t
  (** Takes the color of the rubik and create the Cubie to be solve! *)
