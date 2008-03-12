(* Generate the graphics for the talk *)

open Rubik
module D = Display_base

let geom = { D.geom with D.width = 1.; height = 1. }


let save fname ?(geom=geom) cube =
  let fh = open_out ("expose-" ^ fname ^ ".tex") in
  D.cube_tikz fh ~geom cube;
  close_out fh


let () =
  save "id" Cubie.id



(* Local Variables: *)
(* compile-command: "make -k expose.exe" *)
(* End: *)
