(** Module giving the tool to print a rubie on a computer screen.*)

type color6 = color * color * color * color * color * color
(** Defines the six color of a cubie.*)

val cube :  color6 -> int -> int
  -> (Cubie.t -> Cubie.corner array -> Cubie.edge array -> ())
(** [initial_cubie_color lgth_sq angle] returns a function who, given a cubie, an array of corners and an array of edges, prints on the screen the edges and the corners of the cubie. [lght_sq] defines the lenght of the squares and [angle] the angle of the inclinaison of the upper face!*)
