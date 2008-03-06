open Graphics
open Printf
open Ppm
open Rubik
open Snapshot

(** Initialize the rubik state taking snapshot of the real rubik!*)

type colorf = Red | Green | Yellow | White | Orange | Blue

module Face =
struct
  type t = colorf array array

  let coord id = id mod 3, 2 - id / 3

  let id (x,y) = x + y * 3

  let rotate (x,y) rot = match (rot mod 4) with
    |0 -> x, y
    |1 -> y, 2-x
    |2 -> 2-x, 2-y
    |_ -> 2-y, x

  (* array of all representing the upper face *)
  let u = Array.make_matrix 3 3 Red
  let r = Array.make_matrix 3 3 Red
  let f = Array.make_matrix 3 3 Red
  let l = Array.make_matrix 3 3 Red
  let d = Array.make_matrix 3 3 Red
  let b = Array.make_matrix 3 3 Red
end

module Color =
struct
  let rgb_components (c:Graphics.color) =
    (c lsr 16) land 0xFF,
    (c lsr 8) land 0xFF,
    c land 0xFF

  let name rgb =
    let (r,g,b) = rgb in
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
    let (x,y) = Face.coord id in
    f.(x).(y)
end


module Pick =
struct
  let abs x =
    43 + 30 * x

  let ord y =
    23 + 30 * y

  let pick_point snapshot x0 y0 =
    let rec pick_y y ret =
      let rec pick_x x retour = match x with
        |3 -> retour
        |_ ->
           pick_x (x+1)
             (Color.rgb_components
                (snapshot.(x0 + 8 + x*7).(y0 + 8 + y*7)) :: retour
             )
      in
      match y with
      |3 -> ret
      |_ -> pick_y (y+1) ((pick_x 0 []) :: ret) in
    List.concat (pick_y 0 [])

  let ( +! ) (x1,y1,z1) (x2,y2,z2) = (x1 + x2, y1 + y2, z1 + z2)

  let ( /! ) (x,y,z) a = (x/a ,y/a, z/a)

  (* compute the average vector of a vector list *)
  let average list_color =
    let rec itern lc sum number_el = match lc with
      |[] -> sum /! number_el
      |el :: li -> itern li (sum +! el) (number_el+1)
    in itern list_color (0,0,0) 0

  let take_face webcam face orient =
    let snapshot = Snapshot.take webcam in
    let f = (match face with
           |U -> Face.u
           |R -> Face.r
           |F -> Face.f
           |L -> Face.l
           |D -> Face.d
           |B -> Face.b) in
    let fill_matrix_square x y =
      let (i,j) = Face.rotate (x,y) orient in
      f.(i).(j) <- Color.name
        (average (pick_point snapshot (abs x) (ord y)));
    in
    Array.iter (fun x ->
                  Array.iter (fun y -> fill_matrix_square x y) [|0;1;2|]
               ) [|0;1;2|]
end

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
                                             Color.color_of face)
                                  (corner_set corner), corner) corner_list in
  let color_corner = List.map (fun corner ->
                                 Array.map (fun face_id ->
                                              Color.color_fid face_id)
                                   (corner_def corner)) corner_list in
  order color_corner color_place

let edge_list_replacement _ =
  let color_place = List.map (fun edge ->
                                Array.map (fun face ->
                                             Color.color_of face)
                                  (edge_set edge), edge) edge_list in
  let color_corner = List.map (fun edge ->
                                 Array.map (fun face_id ->
                                              Color.color_fid face_id)
                                   (edge_def edge)) edge_list in
  order color_corner color_place

let create_rubik _ =
  let webcam = Snapshot.start () in
  (*Translator.face_iter (take_face webcam);*)
  Snapshot.stop webcam;
  let corner_list_ordered = corner_list_replacement () in
  let edge_list_ordered = edge_list_replacement () in
  let elo = List.map (fun (a,i) -> (a, (i = 1))) edge_list_ordered in
  let cubie = Cubie.make corner_list_ordered elo in
  cubie
