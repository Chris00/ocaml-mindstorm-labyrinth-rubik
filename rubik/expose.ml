(* Generate the graphics for the talk *)

open Rubik
module D = Display_base

(* Left associative *)
let ( >> ) c m = Cubie.mul c (Cubie.move(Move.make m))

let geom = { D.geom with D.width = 1.; height = 1. }


let save fname ?(geom=geom) cube =
  let fh = open_out ("expose-" ^ fname ^ ".tex") in
  D.cube_tikz fh ~geom cube;
  close_out fh


let () =
  save "id" Cubie.id;

  save "F1" (Cubie.id >> (F,1));
  save "F2" (Cubie.id >> (F,2));
  save "F3" (Cubie.id >> (F,3));

  save "F1R2" (Cubie.id >> (F,1) >> (R,2));

  save "F1B2" (Cubie.id >> (F,1) >> (B,2));

  save "F1R2U3" (Cubie.id >> (F,1) >> (R,2) >> (U,3));


(* Local Variables: *)
(* compile-command: "make -k expose.exe" *)
(* End: *)
