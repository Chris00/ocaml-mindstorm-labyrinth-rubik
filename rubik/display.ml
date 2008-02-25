open Graphics
open Printf
open Rubik

type color6 = color * color * color * color * color * color

let pi = 4. *. atan(1.) (*returns pi *)
let rad angle = float_of_int(angle) *. pi /. 180. (*returns [angle] in radian*)

let round x = truncate(x +. 0.5) (*rounds a float*)

type vector = int * int (*Represents a vector of a 2D plane*)

type def_face = {
  (*Each face is define by some values saying where and how drawing it!*)
  x0 : int; (*The abscissa of the bottom left corner of the face*)
  y0 : int; (*The ordinate of the bottom left corner of the face*)
  vect_r : vector; (*Defines a face right vector*)
  vect_u : vector (*Defines a face upper vector*)
}

let cube (c_U, c_L, c_F, c_R, c_B, c_D) lgth_sq angle =
  let angle = rad angle in
  open_graph (" " ^ string_of_int(9 * lgth_sq)
              ^ "x" ^  string_of_int(9 * lgth_sq));
  let caract_U = { (*Defines the feature of the Upper face*)
    x0 = 3 * lgth_sq;
    y0 = 6 * lgth_sq;
    vect_r = (lgth_sq, 0);
    vect_u = (lgth_sq * round (cos angle), lgth_sq * round (sin angle))
  }
  in
  let caract_L = { (*Defines the feature of the Left face*)
    x0 = 0;
    y0 = 3 * lgth_sq;
    vect_r = (lgth_sq, 0);
    vect_u = (0, lgth_sq)
  }
  in
  let caract_F = { (*Defines the feature of the Front face*)
    x0 = 3 * lgth_sq;
    y0 = 3 * lgth_sq;
    vect_r = (lgth_sq, 0);
    vect_u = (0, lgth_sq)
  }
  in
  let caract_R = { (*Defines the feature of the Right face*)
    x0 = 6 * lgth_sq;
    y0 = 3 * lgth_sq;
    vect_r = (lgth_sq * round (cos (pi -. angle))
                , lgth_sq * round (sin (pi -. angle)));
    vect_u = (0, lgth_sq)
  }
  in
  let caract_B = { (*Defines the feature of the Back face*)
    x0 = (6 + 3 * round  (cos (pi -. angle)))  * lgth_sq;
    y0 = (3 + 3 * round (sin(pi -. angle))) * lgth_sq ;
    vect_r = (lgth_sq, 0);
    vect_u = (0, lgth_sq)
  }
  in
  let caract_D = { (*Defines the feature of the Down face*)
    x0 = 3 * lgth_sq;
    y0 = 0;
    vect_r = (lgth_sq, 0);
    vect_u = (0, lgth_sq)
  }
  in
  let give_from face = match  face with 
      (*Giving the feature of the face [face]*)
    |`U -> caract_U
    |`L -> caract_L
    |`F -> caract_F
    |`R -> caract_R
    |`B -> caract_B
    |`D -> caract_D
  in
  let color_of face = match  face with
      (*Giving the color of the face [face]*)
    |`U -> c_U
    |`L -> c_L
    |`F -> c_F
    |`R -> c_R
    |`B -> c_B
    |`D -> c_D
  in
  let draw_square face id color =
    (* Draw the square [id] of the face [face] in the color [color]
       For a face the id is define in this way
       -------------
       |   |   |   |
       | 1 | 2 | 3 |
       |   |   |   |
       | 4 | 5 | 6 |
       |   |   |   |
       | 7 | 8 | 9 |
       |   |   |   |
       ------------- *)
    set_color (color);
    let x0 = (give_from face).x0 + (id mod 3) * fst((give_from face).vect_r)
      + ((id-1)/3) * fst((give_from face).vect_u)
    and y0 = (give_from face).x0 + (id mod 3) * snd((give_from face).vect_r)
      + ((id-1)/3) *  snd((give_from face).vect_u) in
    let coord_square = [|
      (x0, y0);
      (x0 + fst((give_from face).vect_u), y0 + snd((give_from face).vect_u));
      (x0 + fst((give_from face).vect_u) + fst((give_from face).vect_r),
       y0 + snd((give_from face).vect_u) + snd((give_from face).vect_r));
      (x0 + fst((give_from face).vect_r), y0 + snd((give_from face).vect_r))
    |] in
    fill_poly coord_square;
    set_color black;
    draw_poly coord_square
  in
  let def_corner c = match c with (*Attribute to each corner face and id*)
    | Cubie.URF ->  [| (`U,9); (`R,1); (`F,3) |]
    | Cubie.UFL ->  [| (`U,7); (`F,1); (`L,3) |]
    | Cubie.ULB ->  [| (`U,1); (`L,1); (`B,3) |]
    | Cubie.UBR ->  [| (`U,3); (`B,1); (`R,3) |]
    | Cubie.DFR ->  [| (`D,3); (`F,9); (`R,7) |]
    | Cubie.DLF ->  [| (`D,1); (`L,9); (`F,7) |]
    | Cubie.DBL ->  [| (`D,7); (`B,9); (`L,7) |]
    | Cubie.DRB ->  [| (`D,9); (`R,9); (`B,7) |]
  in
  let def_edge e = match e with (*Attribute to each edga face and id*)
    | Cubie.UR -> [| (`U,6); (`R,2) |]
    | Cubie.UF -> [| (`U,8); (`F,2) |]
    | Cubie.UL -> [| (`U,4); (`L,2) |]
    | Cubie.UB -> [| (`U,2); (`B,2) |]
    | Cubie.DR -> [| (`D,6); (`R,8) |]
    | Cubie.DF -> [| (`D,2); (`F,8) |]
    | Cubie.DL -> [| (`D,4); (`L,3) |]
    | Cubie.DB -> [| (`D,8); (`B,8) |]
    | Cubie.FR -> [| (`F,6); (`R,4) |]
    | Cubie.FL -> [| (`F,4); (`L,6) |]
    | Cubie.BL -> [| (`B,6); (`L,4) |]
    | Cubie.BR -> [| (`B,4); (`R,6) |]
  in
  let draw_corner position (structure, orientation) =
    (*Draws the corner [structure] at the position [position]
      with the orientation [orientation] *)
    let pos = def_corner position in
    let struc = def_corner structure in
    Array.iteri (fun it (face, id) ->
                      let color = color_of
                        (fst struc.((it + orientation) mod 3)) in
                      draw_square  face id color) pos in

  let draw_edge position (structure, orient) =
    (*Draws the edge [structure] at the position [position]
      with the orientation [orientation] *)
    let pos = def_edge position in
    let struc = def_edge structure in
    Array.iteri (fun it (face, id) ->
                      let color = color_of
                        (fst struc.((it + (if orient then 0 else 1)) mod 2)) in
                      draw_square face id color) pos in
  fun cube corner_list edge_list ->
    Array.iter (fun pos ->
                  let stru = Cubie.corner cube pos in
                  draw_corner pos stru) corner_list;
    Array.iter (fun pos ->
                  let stru = Cubie.edge cube pos in
                  draw_edge pos stru) edge_list


let () =
  let test = cube (red, blue, green, white, black, yellow) 30 30 in
  let c_list =
    [|Cubie.URF; Cubie.UFL; Cubie.ULB; Cubie.UBR;
     Cubie.DFR; Cubie.DLF; Cubie.DBL; Cubie.DRB|] in
  let e_list =
    [|Cubie.UR; Cubie.UF; Cubie.UL; Cubie.UB;
      Cubie.DR; Cubie.DF; Cubie.DL; Cubie.DB;
      Cubie.FR; Cubie.FL; Cubie.BL; Cubie.BR|] in
  test Cubie.id c_list e_list;
  ignore(wait_next_event [Button_down])
