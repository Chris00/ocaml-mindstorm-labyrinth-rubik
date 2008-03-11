open Graphics
open Printf
open Ppm
open Rubik
open Snapshot

module Motor = Mindstorm.Motor

let conn = let bt =
  if Array.length Sys.argv < 2 then (
    printf "%s <bluetooth addr>\n" Sys.argv.(0);
    exit 1;
  )
  else Sys.argv.(1) in Mindstorm.connect_bluetooth bt

module C =
struct
  let conn = conn
  let motor_fighter = Motor.a
  let motor_hand = Motor.b
  let motor_pf = Motor.c
  let push_hand_port = `S2
  let push_fighter_port = `S1
  let cog_is_set_left = true
end

module M = Translator.Make(C)

(** Initialize the rubik state taking snapshot of the real rubik!*)

type colorf = Red | Green | Yellow | White | Orange | Blue

module Color =
struct
  let color_graphics = function
    |Red -> red
    |Green -> green
    |Yellow -> yellow
    |White -> white
    |Orange -> magenta
    |Blue -> blue

  let rgb_components (c:Graphics.color) =
    (c lsr 16) land 0xFF,
    (c lsr 8) land 0xFF,
    c land 0xFF

  let min a b =
    if a > b then b
    else a

  let min3 a b c =
    min a (min b c)

  let max a b =
    if a > b then a
    else b

  let max3 a b c =
    max a (max b c)

  (* according to wikipedia [http://en.wikipedia.org/wiki/HSL_color_space]
     we can find the hue from the rbg in this way *)
  let hue r g b =
    if r = g && g = b then 0
    else if r >= g && r >= b (* red is the maximun *)
    then int_of_float (360. +. 60. *. float (g - b) /. float (r - (min g b)))
      mod 360
    else if g >= r && g >= b (* green is the maximum *)
    then int_of_float(120. +. 60. *. float (b - r) /. float(g - (min b r)))
      (* blue is the maximum *)
    else int_of_float(240. +. 60. *. float (r - g) /. float (b - (min r g)))

  let lightness r g b =
    int_of_float (float ((max3 r g b + min3 r g b) / 2) /. 2.55)

  let saturation r g b =
    let l = lightness r g b in
    if l <= 127 then int_of_float(
      float (max3 r g b - min3 r g b) /. 2. /. (float l /. 100.)
    )
    else int_of_float(
      float (max3 r g b - min3 r g b) /. 2. /.(1. -. (float l /. 100.))
    )

  let name rgb =
    let (r,g,b) = rgb in
    if lightness r g b > 80 || saturation r g b < 20 then White
    else
      let h = hue r g b in
      if h <= 26 || h > 300 then Red
      else if h > 26 && h <= 45 then Orange
      else if h > 45 && h <= 75 then Yellow
      else if h > 75 && h < 180 then Green
      else Blue

  let to_string = function
    |Red -> "R"
    |Green -> "G"
    |Yellow -> "Y"
    |White -> "W"
    |Orange -> "O"
    |Blue -> "B"

end

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
  let r = Array.make_matrix 3 3 Green
  let f = Array.make_matrix 3 3 Yellow
  let l = Array.make_matrix 3 3 White
  let d = Array.make_matrix 3 3 Blue
  let b = Array.make_matrix 3 3 Orange

  let color_of face = match face with
    |U -> u.(1).(1)
    |R -> r.(1).(1)
    |F -> f.(1).(1)
    |L -> l.(1).(1)
    |D -> d.(1).(1)
    |B -> b.(1).(1)

  (* returns the color of the element [id] of the face [face] *)
  let color_fid (face,id) =
    let f = (match face with
             |U -> u
             |R -> r
             |F -> f
             |L -> l
             |D -> d
             |B -> b) in
    let (x,y) = coord id in
    f.(x).(y)

  let to_string face =
    let rec ry y return =
      let rec rx x ret = match x with
        |3 -> ret
        |_ -> rx (x+1) (ret ^ (" " ^ Color.to_string f.(x).(y)) ^ " -") in
      match y with
      |3 -> return ^ "\n-------------\n-"
      |_ ->  ry (y+1) (rx 0 (return ^ "\n-------------\n-")) in
    ry 0 ""
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
                (snapshot.(Array.length snapshot -y0- y*7).(x0 + x*7)) :: retour
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

  let take_face face orient =
    let webcam = Snapshot.start () in
    let snapshot = Snapshot.take webcam in
    Snapshot.stop webcam;
    let f = (match face with
           |U -> Face.u
           |R -> Face.r
           |F -> Face.f
           |L -> Face.l
           |D -> Face.d
           |B -> Face.b) in
    let fill_matrix_square x y =
      let (i,j) = Face.rotate (x,y) orient in
      f.(x).(y) <- Color.name
        (average (pick_point snapshot (abs i) (ord j)));
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

(* returns true if the corner or edge with the orientation [orient]
   fit in the place (which is given by a corner or a an edge*)
let harmony ce pl orient =
  let lgth = Array.length ce in
  let rec iter ret i =
    if i = lgth then ret
    else iter (ret && (pl.((i+orient) mod lgth) = ce.(i))) (i+1)
  in iter true 0

let find_orientation ce pl =
  let lgth = Array.length ce in
  let rec iter it =
    if it = lgth then 3
    else
      if harmony ce pl it then it
      else iter (it +1)
  in iter 0

let find pl ce_list =
  (* finds the corner or the edge from the list [ce_list] which fit in
     the place [pl] *)
  let rec iter ce_l = match ce_l with
    |[] -> failwith "No place find for a corner/edge"
    |(el_color, el_return) :: li ->
       let orient = find_orientation el_color pl in
       if orient <> 3 then el_return, orient
       else iter li
  in iter ce_list

(* Order a list of corners or edges in good position to represents a
   real cubie and create it with Rubik.Cubie.make *)
let order place_list corner_list =
  let rec iter ret t_o = match t_o with
    |[] -> List.rev ret
    |el :: li -> iter ((find el corner_list) :: ret) li
  in iter [] place_list

let corner_list_replacement _ =
  let color_place = List.map (fun corner ->
                                Array.map (fun face ->
                                             Face.color_of face)
                                  (corner_set corner)) corner_list in
  let color_corner = List.map (fun corner ->
                                 Array.map (fun face_id ->
                                              Face.color_fid face_id)
                                   (corner_def corner), corner) corner_list in
  order color_place color_corner

let edge_list_replacement _ =
  let color_place = List.map (fun edge ->
                                Array.map (fun face ->
                                             Face.color_of face)
                                  (edge_set edge)) edge_list in
  let color_corner = List.map (fun edge ->
                                 Array.map (fun face_id ->
                                              Face.color_fid face_id)
                                   (edge_def edge), edge) edge_list in
  order color_place color_corner

let create_rubik _ =
  M.face_iter (Pick.take_face);
  let corner_list_ordered = corner_list_replacement () in
  let edge_list_ordered = edge_list_replacement () in
  let elo = List.map (fun (a,i) -> (a, (i = 1))) edge_list_ordered in
  let cubie = Cubie.make corner_list_ordered elo in
  cubie

(**let () =
  printf "t%!";
  let c = create_rubik () in
  let x0 = 10
  and y0 = 10
  and len_sq = 30 in
  let colors = (Color.color_graphics (Face.color_of U),
                Color.color_graphics (Face.color_of L),
                Color.color_graphics (Face.color_of F),
                Color.color_graphics (Face.color_of R),
                Color.color_graphics (Face.color_of B),
                Color.color_graphics (Face.color_of D)) in
  open_graph ("");
  Display.cube x0 y0 colors len_sq c;
  ignore(wait_next_event [Button_down])*)
