
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

(* Enrich List *)
module List = struct
  include List
  let iteri f l =
    let rec iter i = function
      | [] -> ()
      | a :: tl -> f i a; iter (succ i) tl in
    iter 0 l
end



type generator = F | B | L | R | U | D

(** Symmetric group of the cube.  Some Rubik cube coordinates are
    "reduced" using this symmetry group. *)
module Sym =
struct
  type t = int

end


(** The Cubies level is a simple way of describing a cube position
    (i.e. an element of the Rubik group).  This level is intended to
    communicate with the "outside world" and for basic manipulation.
    It is not as efficient as subsequent coordinate systems.  *)
module Cubie =
struct
  type corner = URF | UFL | ULB | UBR | DFR | DLF | DBL | DRB
  let int_of_corner = function
    | URF -> 0 | UFL -> 1 | ULB -> 2 | UBR -> 3
    | DFR -> 4 | DLF -> 5 | DBL -> 6 | DRB -> 7
  let corner_int = [| URF; UFL; ULB; UBR; DFR; DLF; DBL; DRB |]
  let corner_name = [| "URF"; "UFL"; "ULB"; "UBR"; "DFR"; "DLF"; "DBL"; "DRB" |]

  type edge = UR | UF | UL | UB | DR | DF | DL | DB | FR | FL | BL | BR
  let int_of_edge = function
    | UR -> 0 | UF -> 1 | UL -> 2 | UB -> 3 | DR -> 4 | DF -> 5
    | DL -> 6 | DB -> 7 | FR -> 8 | FL -> 9 | BL -> 10 | BR -> 11
  let edge_int = [| UR; UF; UL; UB; DR; DF; DL; DB; FR; FL; BL; BR |]
  let edge_name = [| "UR"; "UF"; "UL"; "UB"; "DR"; "DF";
                     "DL"; "DB"; "FR"; "FL"; "BL"; "BR" |]

  type t = {
    corner_perm: int array;
    corner_rot: int array;              (* 0,1,2: CW rotation of 120° *)
    edge_perm: int array;
    edge_flip: int array;               (* 0: not flipped, 1: flipped *)
  }

  let unsafe_make corners edges =
    let corner_perm = Array.make 8 (-1)
    and corner_rot = Array.make 8 0 in
    List.iteri (fun i (c,o) ->
                  corner_perm.(i) <- int_of_corner c;
                  corner_rot.(i) <- o
               ) corners;
    let edge_perm = Array.make 12 (-1)
    and edge_flip = Array.make 12 0 in
    List.iteri (fun i (e,o) ->
                  edge_perm.(i) <- int_of_edge e;
                  edge_flip.(i) <- o;
               ) edges;
    { corner_perm = corner_perm; corner_rot = corner_rot;
      edge_perm = edge_perm; edge_flip = edge_flip }

  let make ~corner ~edge =
    if List.length corner <> 8 then invalid_arg "Rubik.Cubie.make: 8 corners!";
    if List.length edge <> 12 then invalid_arg "Rubik.Cubie.make: 12 edges!";
    (* Check injectivity & orientations valuesa *)
    let corner_seen = Array.make 8 false in
    List.iter (fun (c,o) ->
                 let ci = int_of_corner c in
                 if corner_seen.(ci) then
                   invalid_arg("Rubik.Cubie.make: " ^ corner_name.(ci)
                               ^ " present twice");
                 corner_seen.(ci) <- true;
                 if o < 0 || o > 2 then
                   invalid_arg("Rubik.Cubie.make: " ^ corner_name.(ci)
                               ^ " orientation must be 0, 1, or 2");
              ) corner;
    let edge_seen = Array.make 12 false in
    List.iter (fun (e,_) ->
                 let ei = int_of_edge e in
                 if edge_seen.(ei) then
                   invalid_arg("Rubik.Cubie.make: " ^ edge_name.(ei)
                               ^ " present twice");
              ) edge;
    (* Make sure the orientation is globally coherent *)
    if (List.fold_left (fun s (_,o) -> s + o) 0 corner) mod 3 <> 0 then
      invalid_arg "Rubik.Cubie.make: incoherent orientation of corner cubies";
    let edge = List.map (fun (e,o) -> (e, if o then 1 else 0)) edge in
    if (List.fold_left (fun s (_,o) -> s + o) 0 edge) land 0x1 <> 0 then
      invalid_arg "Rubik.Cubie.make: incoherent orientation of edge cubies";
    unsafe_make corner edge

  let corner cube c =
    let ci = int_of_corner c in
    (corner_int.(cube.corner_perm.(ci)), cube.corner_rot.(ci))
  let edge cube e =
    let ei = int_of_edge e in
    (edge_int.(cube.edge_perm.(ei)), cube.edge_flip.(ei) <> 0)

  (* The elements corresponding to the generators (a clockwise move of
     the corresponding face). *)
  let move_F =
    unsafe_make [UFL,1; DLF,2; ULB,0; UBR,0; URF,2; DFR,1; DBL,0; DRB,0]
      [UR,0;  FL,1;  UL,0;UB,0;DR,0;  FR,1;  DL,0;DB,0;  UF,1; DF,1;  BL,0;BR,0]
  let move_B =
    unsafe_make [URF,0; UFL,0; UBR,1; DRB,2; DFR,0; DLF,0; ULB,2; DBL,1]
      [UR,0;UF,0;UL,0;  BR,1;  DR,0;DF,0;DL,0;  BL,1;  FR,0;FL,0;  UB,1; DB,1]
  let move_L =
    unsafe_make [URF,0; ULB,1; DBL,2; UBR,0; DFR,0; UFL,2; DLF,1; DRB,0]
      [UR,0;UF,0;  BL,0;  UB,0;DR,0;DF,0;  FL,0;  DB,0;FR,0;  UL,0; DL,0;  BR,0]
  let move_R =
    unsafe_make [DFR,2; UFL,0; ULB,0; URF,1; DRB,1; DLF,0; DBL,0; UBR,2]
      [FR,0;  UF,0;UL,0;UB,0;  BR,0;  DF,0;DL,0;DB,0;  DR,0;  FL,0;BL,0;  UR,0]
  let move_U =
    unsafe_make [UBR,0; URF,0; UFL,0; ULB,0; DFR,0; DLF,0; DBL,0; DRB,0]
      [UB,0; UR,0; UF,0; UL,0;  DR,0;DF,0;DL,0;DB,0;FR,0;FL,0;BL,0;BR,0]
  let move_D =
    unsafe_make [URF,0; UFL,0; ULB,0; UBR,0; DLF,0; DBL,0; DRB,0; DFR,0]
      [UR,0;UF,0;UL,0;UB,0;  DF,0; DL,0; DB,0; DR,0; FR,0;FL,0;BL,0;BR,0]
  let move = function
    | F -> move_F | B -> move_B | L -> move_L
    | R -> move_R | U -> move_U | D -> move_D

  (* Group operations *)
  let id = {
    corner_perm = Array.init 8 (fun i -> i); (* 8 corners *)
    corner_rot = Array.make 8 0;
    edge_perm = Array.init 12 (fun x -> x); (* 12 edges *)
    edge_flip = Array.make 12 0;
  }

  let is_identity cube = cube = id

  let mul a b = {
    corner_perm = Array.init 8 (fun x -> a.corner_perm.(b.corner_perm.(x)));
    corner_rot =
      (let o x = (a.corner_rot.(b.corner_perm.(x)) + b.corner_rot.(x)) mod 3 in
       Array.init 8 o);
    edge_perm = Array.init 12 (fun x -> a.edge_perm.(b.edge_perm.(x)));
    edge_flip =
      (let o x = (a.edge_flip.(a.edge_perm.(x)) + b.edge_flip.(x)) land 0x1 in
       Array.init 12 o);
  }

  (* inverse in Z/3Z *)
  let inv3 x = if x = 0 then 0 else 3 - x

  let inv cube =
    let corner_perm = Array.make 8 (-1) in
    for i = 0 to 7 do corner_perm.(cube.corner_perm.(i)) <- i done;
    let edge_perm = Array.make 12 (-1) in
    for i = 0 to 11 do edge_perm.(cube.edge_perm.(i)) <- i done;
    {
      corner_perm = corner_perm;
      corner_rot = Array.init 8 (fun i -> inv3 cube.corner_rot.(i));
      edge_perm = edge_perm;
      edge_flip = cube.edge_flip;         (* in Z/2Z, -x = x *)
    }

end

