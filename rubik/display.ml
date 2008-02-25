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

let cube x0 y0 (c_U, c_L, c_F, c_R, c_B, c_D) lgth_sq angle =
  let color_bg = rgb 219 219 219 in (* the back ground color *)
  let color_struct = rgb 0 0 0 in (* the color of the structure *)
  let angle = rad angle in
  let img_width = x0 + 12 * lgth_sq in
  let img_high = y0 + 9 * lgth_sq in
  open_graph (" " ^ string_of_int(img_width)
              ^ "x" ^  string_of_int(img_high));
  set_color color_bg;
  fill_rect 0 0 img_width img_high;
  let caract_U = { (*Defines the feature of the Upper face*)
    x0 = x0 + 3 * lgth_sq;
    y0 = y0 + 6 * lgth_sq;
    vect_r = (lgth_sq, 0);
    vect_u = (round (float lgth_sq *. cos angle),
              round (float lgth_sq *. sin angle))
  }
  in
  let caract_L = { (*Defines the feature of the Left face*)
    x0 = x0;
    y0 = y0 + 3 * lgth_sq;
    vect_r = (lgth_sq, 0);
    vect_u = (0, lgth_sq)
  }
  in
  let caract_F = { (*Defines the feature of the Front face*)
    x0 = x0 + 3 * lgth_sq;
    y0 = y0 + 3 * lgth_sq;
    vect_r = (lgth_sq, 0);
    vect_u = (0, lgth_sq)
  }
  in
  let caract_R = { (*Defines the feature of the Right face*)
    x0 = x0 + 6 * lgth_sq;
    y0 = y0 + 3 * lgth_sq;
    vect_r = round (float lgth_sq *. cos angle)
                , round (float lgth_sq *. sin angle);
    vect_u = (0, lgth_sq)
  }
  in
  let caract_B = { (*Defines the feature of the Back face*)
    x0 = x0 + 6 * lgth_sq + 3 * round (cos angle *. float lgth_sq);
    y0 = y0 + 3 * lgth_sq + 3 * round (sin angle *. float lgth_sq);
    vect_r = (lgth_sq, 0);
    vect_u = (0, lgth_sq)
  }
  in
  let caract_D = { (*Defines the feature of the Down face*)
    x0 = x0 + 3 * lgth_sq;
    y0 = y0;
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
       | 1 | 2 | 3 |
       -------------
       | 4 | 5 | 6 |
       -------------
       | 7 | 8 | 9 |
       ------------- *)
    set_color (color);
    let x0 = (give_from face).x0
      + (((id-1) mod 3) * fst((give_from face).vect_r))
      + ((2-(id-1)/3) * fst((give_from face).vect_u))
    and y0 = (give_from face).y0
      + (((id-1) mod 3) * snd((give_from face).vect_r))
      + ((2-(id-1)/3) *  snd((give_from face).vect_u)) in
    let coord_square = [|
      (x0, y0);
      (x0 + fst((give_from face).vect_u), y0 + snd((give_from face).vect_u));
      (x0 + fst((give_from face).vect_u) + fst((give_from face).vect_r),
       y0 + snd((give_from face).vect_u) + snd((give_from face).vect_r));
      (x0 + fst((give_from face).vect_r), y0 + snd((give_from face).vect_r))
    |] in
    fill_poly coord_square;
    set_color color_struct;
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
    | Cubie.DL -> [| (`D,4); (`L,8) |]
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
                        (fst struc.((it - orientation + 3) mod 3)) in
                      draw_square  face id color) pos
  in
  let draw_edge position (structure, fliped) =
    (*Draws the edge [structure] at the position [position] who is [fliper]
    or not!*)
    let pos = def_edge position in
    let struc = def_edge structure in
    Array.iteri (fun it (face, id) ->
                      let color = color_of
                        (fst struc.((it + (if fliped then 1 else 0)) mod 2)) in
                      draw_square face id color) pos
  in
  let draw_structure =
    (*Draws the rubie without color! *)
    Array.iter (fun face ->
                  Array.iter (fun id ->
                                 draw_square face id color_bg;
                  ) [|1; 2; 3; 4; 6; 7; 8; 9|]
    ) [|`U; `R; `L; `F; `B; `D|]
  in
  fun cube corner_list edge_list ->
    (* creating the structure *)
    draw_structure;
    (* puting color on the square middle face *)
    draw_square `U  5 c_U;
    draw_square `R  5 c_R;
    draw_square `L  5 c_L;
    draw_square `F  5 c_F;
    draw_square `B  5 c_B;
    draw_square `D  5 c_D;
    Array.iter (fun pos ->
                  let stru = Cubie.corner cube pos in
                  draw_corner pos stru) corner_list;
    Array.iter (fun pos ->
                  let stru = Cubie.edge cube pos in
                  draw_edge pos stru) edge_list

(* test function *)
let () =
  let test = cube 60 120 (blue, magenta, yellow, red, white, green) 30 45 in
  let c_list =
    [|Cubie.URF; Cubie.UFL; Cubie.ULB; Cubie.UBR;
      Cubie.DFR; Cubie.DLF; Cubie.DBL; Cubie.DRB|] in
  let e_list =
    [|Cubie.UR; Cubie.UF; Cubie.UL; Cubie.UB;
      Cubie.DR; Cubie.DF; Cubie.DL; Cubie.DB;
      Cubie.FR; Cubie.FL; Cubie.BL; Cubie.BR|] in
  test (Cubie.move (Move.make (B, 1))) c_list e_list;
  ignore(wait_next_event [Button_down])