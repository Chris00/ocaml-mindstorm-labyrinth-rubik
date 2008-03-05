open Graphics
open Printf
open Ppm
open Rubik

(** Initialize the rubik state taking snapshot of the real rubik!*)

type colorf = Red | Green | Yellow | White | Orange | Blue

module Face =

struct
  type t = colorf array array

  let coord_abs id = id mod 3, 2 - id / 3

  let id (x,y) = x + y * 3

  (* array of all representing the upper face *)
  let u = Array.make_matrix 3 3 Red
  let r = Array.make_matrix 3 3 Red
  let f = Array.make_matrix 3 3 Red
  let l = Array.make_matrix 3 3 Red
  let d = Array.make_matrix 3 3 Red
  let b = Array.make_matrix 3 3 Red
end

(*where the snapshot file is saved *)
let snapshot_file = "/Users/marku/Desktop/ocaml.ppm"

(*return for a Graphics.color the rgb components*)
let rgb_components c =
  (c lsr 16) land 0xFF,
  (c lsr 8) land 0xFF,
  c land 0xFF

(*returns from the rgb components the colorf *)
let color_name x =
  let (r,g,b) = x in
  if r > 130
  then (* we have red orange white or yellow *)
    if b > (r / 5)
    then White
    else if g < (r / 5)
    then Red
    else if g < 3 * (r / 5)
    then  Orange
    else Yellow
  else
    if b < 10 then Green
    else Blue

(* returns the color of a face *)
let color_of face = match face with
  |U -> Face.u.(1).(1)
  |R -> Face.r.(1).(1)
  |F -> Face.f.(1).(1)
  |L -> Face.l.(1).(1)
  |D -> Face.d.(1).(1)
  |B -> Face.b.(1).(1)

(* returns the color of the element [id] of the face [face] *)
let color_fid (face,id) =
  let f = (match face with
         |U -> Face.u
         |R -> Face.r
         |F -> Face.f
         |L -> Face.l
         |D -> Face.d
         |B -> Face.b) in
  let (x,y) = Face.coord_abs id in
  f.(x).(y)

(* list of all the corner of the cube *)
let corner_list = [ Cubie.URF; Cubie.UFL; Cubie.ULB; Cubie.UBR;
                    Cubie.DFR; Cubie.DLF; Cubie.DBL; Cubie.DRB ]

(* list of all the edge of the cube *)
let edge_list = [ Cubie.UR; Cubie.UF; Cubie.UL; Cubie.UB;
                  Cubie.DR; Cubie.DF; Cubie.DL; Cubie.DB;
                  Cubie.FR; Cubie.FL; Cubie.BL; Cubie.BR]

(* definition of the face designing the corner in the clockwise sens *)
let corner_set = function
  | Cubie.URF -> [| U; R; F |]
  | Cubie.UFL -> [| U; F; L |]
  | Cubie.ULB -> [| U; L; B |]
  | Cubie.UBR -> [| U; B; R |]
  | Cubie.DFR -> [| D; F; R |]
  | Cubie.DLF -> [| D; L; F |]
  | Cubie.DBL -> [| D; B; L |]
  | Cubie.DRB -> [| D; R; B |]

(* definition of the face designing the edge in the clockwise sens *)
let edge_set = function
  | Cubie.UR -> [| U; R |]
  | Cubie.UF -> [| U; F |]
  | Cubie.UL -> [| U; L |]
  | Cubie.UB -> [| U; B |]
  | Cubie.DR -> [| D; R |]
  | Cubie.DF -> [| D; F |]
  | Cubie.DL -> [| D; L |]
  | Cubie.DB -> [| D; B |]
  | Cubie.FR -> [| F; R |]
  | Cubie.FL -> [| F; L |]
  | Cubie.BL -> [| B; L |]
  | Cubie.BR -> [| B; R |]


(* return for each corner his design in term of face - id *)
let corner_def = function
  | Cubie.URF -> [| (U,8); (R,0); (F,2) |]
  | Cubie.UFL -> [| (U,6); (F,0); (L,2) |]
  | Cubie.ULB -> [| (U,0); (L,0); (B,2) |]
  | Cubie.UBR -> [| (U,2); (B,0); (R,2) |]
  | Cubie.DFR -> [| (D,2); (F,8); (R,6) |]
  | Cubie.DLF -> [| (D,0); (L,8); (F,6) |]
  | Cubie.DBL -> [| (D,6); (B,8); (L,6) |]
  | Cubie.DRB -> [| (D,8); (R,8); (B,6) |]

(* return for each corner his design in term of face - id *)
let edge_def = function
  | Cubie.UR -> [| (U,5); (R,1) |]
  | Cubie.UF -> [| (U,7); (F,1) |]
  | Cubie.UL -> [| (U,3); (L,1) |]
  | Cubie.UB -> [| (U,1); (B,1) |]
  | Cubie.DR -> [| (D,5); (R,7) |]
  | Cubie.DF -> [| (D,1); (F,7) |]
  | Cubie.DL -> [| (D,3); (L,7) |]
  | Cubie.DB -> [| (D,7); (B,7) |]
  | Cubie.FR -> [| (F,5); (R,3) |]
  | Cubie.FL -> [| (F,3); (L,5) |]
  | Cubie.BL -> [| (B,5); (L,3) |]
  | Cubie.BR -> [| (B,3); (R,5) |]


let rec find_orientation ce pl =
  let lgth = Array.length ce in
  let harmony orient =
    (* returns true if the corner or edge with the orientation [orient]
       fit in the place (which is given by a corner or a an edge*)
  let rec iter it = match it with
    |lgth -> true
    |_ ->if (pl.(it) = ce.((it+orient) mod lgth)) then iter (it+1)
      else false in
  iter 0 in
  let rec iter it = match it with
    |lgth -> 3
    |_ ->if harmony it then it
      else iter (it +1) in
  iter 0

let find ce ce_list =
  (* find the place and the orientation of a corner or an edge [ce] in
   the list [ce_list] *)
  let rec iter ce_l = match ce_l with
    |[] -> failwith "No place find for a corner/edge"
    |(el_color, el_return) :: li ->
       let orient = find_orientation ce el_color in
       if orient <> 3 then el_return, orient
       else iter li
  in iter ce_list

(* Order a list of corners or edges in good position to represents a
   real cubie and create it with Rubik.Cubie.make *) 
let order list_to_find place_list  =
  let rec iter ret t_o = match t_o with
    |[] -> ret
    |el :: li -> iter ((find el place_list) :: ret) li
  in iter [] list_to_find

let corner_list_replacement _ =
  let color_place = List.map (fun corner ->
                                Array.map (fun face ->
                                             color_of face)
                                  (corner_set corner), corner) corner_list in
  let color_corner = List.map (fun corner ->
                                 Array.map (fun face_id ->
                                              color_fid face_id)
                                   (corner_def corner)) corner_list in
  order color_corner color_place

let edge_list_replacement _ =
  let color_place = List.map (fun edge ->
                                Array.map (fun face ->
                                             color_of face)
                                  (edge_set edge), edge) edge_list in
  let color_corner = List.map (fun edge ->
                                 Array.map (fun face_id ->
                                              color_fid face_id)
                                   (edge_def edge)) edge_list in
  order color_corner color_place

let take_cube_color _ =
  let corner_list_ordered = corner_list_replacement () in
  let edge_list_ordered = edge_list_replacement () in
  let elo = List.map (fun (a,i) -> (a, (i = 1))) edge_list_ordered in
  let cubie = Cubie.make corner_list_ordered elo in
  cubie

(* coordinate of the left bottom picking color squares *)
let coord x =
  43 + 30 * x

(* absissa of the left bottom picking color squares *)
let abs y =
  23 + 30 * y

let set_up_web_cam _ =
  let img = Ppm.as_matrix_exn snapshot_file in
  let height = Array.length img
  and width = Array.length img.(0) in
  open_graph (sprintf " %ix%i"  width height);
  set_color (rgb 0 255 242);
  print_endline "press a key when it's ok!";
  let fill_checking_zone x y =
    fill_rect (coord x) (abs y) 14 14 in

  (* refreshing function drawing blue squares on the cubie. *)
  let rec refresh () =
    (* take a new snapshot *)
    let img = Ppm.as_matrix_exn snapshot_file in
    draw_image (make_image img) 0 0;
    Array.iter (fun x ->
                  Array.iter (fun y -> fill_checking_zone x y) [|0;1;2|]
               ) [|0;1;2|];
    Unix.sleep 1;
    if not (key_pressed ()) then refresh ()
  in refresh ()

(* picking the color on (x0,y0) on the snapshot *)
let pick_point x0 y0 =
  let rec pcy y ret =
    let rec pcx x retour = match x with
      |3 -> retour
      |_ ->
         pcx (x+1) (rgb_components (point_color (x0 + 8 + x*7) (y0 + 8 + y*7))
                    :: retour)
    in
    match y with
    |3 -> ret
    |_ -> pcy (y+1) ((pcx 0 []) :: ret) in
  List.concat (pcy 0 [])

let ( +! ) (x1,y1,z1) (x2,y2,z2) = (x1 + x2, y1 + y2, z1 + z2)

let ( /! ) (x,y,z) a = (x/a ,y/a, z/a)

let average list_color =
  let rec itern lc sum number_el = match lc with
    |[] -> sum /! number_el
    |el :: li -> itern li (sum +! el) (number_el+1)
  in itern list_color (0,0,0) 0

(* taking the color of a face *)
let take_face face =
  let f = (match face with
         |U -> Face.u
         |R -> Face.r
         |F -> Face.f
         |L -> Face.l
         |D -> Face.d
         |B -> Face.b) in
  let fill_matrix_square x y =
    f.(x).(y) <-color_name (average (pick_point (coord x) (abs y)));
  in
  Array.iter (fun x ->
                Array.iter (fun y -> fill_matrix_square x y) [|0;1;2|]
             ) [|0;1;2|]

let take_rubik_color () =
  ()
