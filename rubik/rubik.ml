
(* www.geometer.org/rubik/group.pdf *)
(* http://www-cs-staff.Stanford.EDU/~knuth/preprints.html
   Efficient representation of perm groups. (Sims's algorithm)
   http://www-cs-staff.stanford.edu/~knuth/papers/erpg.tex.gz *)
(* http://www.math.ucf.edu/~reid/Rubik/index.html

   e.g. http://www.math.ucf.edu/~reid/Rubik/optimal_solver.html

   http://kociemba.org/cube.htm
   http://bj.middlebury.edu/~plubans/report

   Supposedly easier introduction to Kociemba algo:
   http://www.geocities.com/jaapsch/puzzles/compcube.htm

   http://cadigweb.ew.usna.edu/~wdj/papers/rubik.pdf
   http://cadigweb.ew.usna.edu/~wdj/rubik_nts.htm

*)

type generators = F | B | L | R | U | D

(** Symmetric group of the cube.  Some Rubik cube coordinates are
    "reduced" using this symmetry group. *)
module Sym =
struct
  type t = int

end


module Coord =
struct

  (** The Cubies level is a simple way of describing a cube position
      (i.e. an element of the Rubik group).  This level is intended to
      communicate with the "outside world" and for basic manipulation.
      It does not need to be as efficient as subsequent coordinate
      systems.  *)
  module Cubies =
  struct


  end


end
