open Rubik

(** Initialize the rubik state : taking snapshot of the real rubik
    and creating a representation of it!*)

(** Color of faces of rubik *)
type colorf = Red | Green | Yellow | White | Orange | Blue

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
module Face :
sig
  type t
    (** Each face is define by nine colors  *)

  val coord_abs : int -> int * int
    (** Give the coordinate and the absissa
        of the position [id] in the matrix face *)

  val id : int * int -> int
    (** Return the number of the facelets from an absissa and a coordinate *)
end

(** Way to access to the snapshot_file *)
val snapshot_file : string

(** Returns the orientation of a corner or an edge [ce] in the place [pl].
     It returns [3] if it's impossible to place the corner*)
val find_orientation : colorf array -> colorf array -> int

val find : colorf array -> (colorf array * 'a) list -> 'a * int

val  order : colorf array list -> (colorf array * 'a) list
  -> ('a * int) list

(** List of the corners and their orientation who replace in the real cubie
    this corners : URF, UFL, ULB, UBR, DFR, DLF, DBL, DRB
    This list is used for Rubik.Cubie.make which initialize
    the cubie for a solving search*) 
val corner_list_replacement : unit -> (Cubie.corner * int) list

(** List of the edges and their orientation who replace in the real cubie
    this edges : UR, UF, UL, UB, DR, DF, DL, DB, FR, FL, BL, BR
    This list is used for Rubik.Cubie.make which initialize
    the cubie for a solving search*) 
val edge_list_replacement : unit -> (Cubie.edge * int) list

(** Takes the color of the rubik and saves the data for working with.*)
val take_rubik_color : unit -> unit

(** Graphical mode to fit the webcam in good position.
    Blue squares are the picking-up colors area*)
val set_up_web_cam : unit -> unit
