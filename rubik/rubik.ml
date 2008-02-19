
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

open Bigarray

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

let generator = [| F; B; L; R; U; D |]

type move = int
    (* F -> 0, F^2 -> 1, F^3 -> 2, B -> 3, B^2 -> 4,..., D^3 -> 17.
       It is important that the fact that is is an int is known by the
       submodules because it will be used as an index of move
       tables. *)

let nmove = 18                         (* to iterate on the moves *)

(* We use [(g, e)] because that is how we want the solution of the
   rubik cube to be presented. *)
let move (g, e) =
  if e < 1 || e > 3 then invalid_arg "Rubik.move: exponent must be 1, 2, or 3";
  match g with
  | F -> e - 1
  | B -> e + 2
  | L -> e + 5
  | R -> e + 8
  | U -> e + 11
  | D -> e + 14

let generator m = (generator.(m / 3), (m mod 3) + 1)


(** Symmetry group of the cube.  Some Rubik cube coordinates are
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
  let ncorners = 8

  type edge = UR | UF | UL | UB | DR | DF | DL | DB | FR | FL | BL | BR
  let int_of_edge = function
    | UR -> 0 | UF -> 1 | UL -> 2 | UB -> 3 | DR -> 4 | DF -> 5
    | DL -> 6 | DB -> 7 | FR -> 8 | FL -> 9 | BL -> 10 | BR -> 11
  let edge_int = [| UR; UF; UL; UB; DR; DF; DL; DB; FR; FL; BL; BR |]
  let edge_name = [| "UR"; "UF"; "UL"; "UB"; "DR"; "DF";
                     "DL"; "DB"; "FR"; "FL"; "BL"; "BR" |]
  let nedges = 12

  type t = {
    corner_perm: int array;             (* int => easy perm composition *)
    corner_rot: int array;              (* 0,1,2: CW rotation of 120° *)
    edge_perm: int array;
    edge_flip: int array;               (* 0: not flipped, 1: flipped *)
  }

  let unsafe_make corners edges =
    let corner_perm = Array.make ncorners (-1)
    and corner_rot = Array.make ncorners 0 in
    List.iteri (fun i (c,o) ->
                  corner_perm.(i) <- int_of_corner c;
                  corner_rot.(i) <- o
               ) corners;
    let edge_perm = Array.make nedges (-1)
    and edge_flip = Array.make nedges 0 in
    List.iteri (fun i (e,o) ->
                  edge_perm.(i) <- int_of_edge e;
                  edge_flip.(i) <- o;
               ) edges;
    { corner_perm = corner_perm; corner_rot = corner_rot;
      edge_perm = edge_perm; edge_flip = edge_flip }

  let make ~corner ~edge =
    if List.length corner <> ncorners then
      invalid_arg "Rubik.Cubie.make: 8 corners!";
    if List.length edge <> nedges then
      invalid_arg "Rubik.Cubie.make: 12 edges!";
    (* Check injectivity & orientations valuesa *)
    let corner_seen = Array.make ncorners false in
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
    let edge_seen = Array.make nedges false in
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

  (* Group operations *)
  let id = {
    corner_perm = Array.init ncorners (fun i -> i);
    corner_rot = Array.make ncorners 0;
    edge_perm = Array.init nedges (fun x -> x);
    edge_flip = Array.make nedges 0;
  }

  let is_identity cube = cube = id

  let mul a b = {
    corner_perm =
      Array.init ncorners (fun x -> a.corner_perm.(b.corner_perm.(x)));
    corner_rot =
      (let o x = (a.corner_rot.(b.corner_perm.(x)) + b.corner_rot.(x)) mod 3 in
       Array.init ncorners o);
    edge_perm = Array.init nedges (fun x -> a.edge_perm.(b.edge_perm.(x)));
    edge_flip =
      (let o x = (a.edge_flip.(a.edge_perm.(x)) + b.edge_flip.(x)) land 0x1 in
       Array.init nedges o);
  }

  (* inverse in Z/3Z *)
  let inv3 x = if x = 0 then 0 else 3 - x

  let inv cube =
    let corner_perm = Array.make ncorners (-1) in
    for i = 0 to ncorners - 1 do corner_perm.(cube.corner_perm.(i)) <- i done;
    let edge_perm = Array.make nedges (-1) in
    for i = 0 to nedges - 1 do edge_perm.(cube.edge_perm.(i)) <- i done;
    {
      corner_perm = corner_perm;
      corner_rot = Array.init ncorners (fun i -> inv3 cube.corner_rot.(i));
      edge_perm = edge_perm;
      edge_flip = cube.edge_flip;       (* in Z/2Z, -x = x *)
    }

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

  (* MAKE SURE to respect the encoding order of {!Rubik.move}. *)
  let move_table =
    let move_F2 = mul move_F move_F
    and move_B2 = mul move_B move_B
    and move_L2 = mul move_L move_L
    and move_R2 = mul move_R move_R
    and move_U2 = mul move_U move_U
    and move_D2 = mul move_D move_D in
    [| move_F; move_F2; mul move_F2 move_F;
       move_B; move_B2; mul move_B2 move_B;
       move_L; move_L2; mul move_L2 move_L;
       move_R; move_R2; mul move_R2 move_R;
       move_U; move_U2; mul move_U2 move_U;
       move_D; move_D2; mul move_D2 move_D |]

  let move (m: move) = move_table.(m)

  let () = assert(Array.length move_table = nmove)
end


module type Coordinate =
sig
  type t
  val of_cube : Cubie.t -> t
  val mul : t -> move -> t
  val initialize : ?file:string -> unit -> unit
end

(** Common structure of coordinates functorized.  (This is performance
    critical, so if it turns out that the code is too slow, one may
    inline the functor by hand...) *)
module MakeCoord(C: sig
                   val length : int
                   val of_cube : Cubie.t -> int
                     (** Returns the coordinate of a cube. *)
                   val to_cube : int -> Cubie.t
                     (** Generate a cube with the orientation
                         represented by the number [o]. *)
                 end) =
struct
  type t = int                          (* 0 .. length-1 *)

  let mul_table = Array2.create int16_unsigned c_layout C.length nmove

  let mul o m = mul_table.{o,m}

  let initialize_mul () =
    for o = 0 to length - 1 do
      let cube = C.to_cube o in
      for m = 0 to nmove - 1 do
        mul_table.{o,m} <- C.of_cube(Cubie.mul cube (Cubie.move m))
      done
    done

  let initialize ?file () =
    match file with
    | None -> initialize_mul()
    | Some fname ->
        if Sys.file_exists fname then begin
          let fh = open_in_bin fname in
          let tbl = input_value fh in   (* may segfault! *)
          Array2.blit tbl mul_table;
          close_in fh
        end
        else begin
          (* Compute and save the table *)
          initialize_mul();
          let fh = open_out_bin fname in
          output_value fh mul_table;
          close_out fh
        end
end


module CornerO = MakeCoord
  (struct
     (* t : 0 .. 2186 = 2^7 - 1 *)
     let length = 2187

     let of_cube cube =
       let n = ref 0 in
       for i = 0 to Cubie.ncorners - 2 do
         n := 3 * !n + cube.Cubie.corner_rot.(i)
       done;
       !n

     let to_cube o =
       let corner_rot = Array.make Cubie.ncorners (-1) in
       let o = ref o and s = ref 0 in
       for i = Cubie.ncorners - 2 downto 0 do
         let d = !o mod 3 in
         corner_rot.(i) <- d;
         s := !s + d;
         o := !o / 3
       done;
       (* (the sum of all orientations) mod 3 = 0 *)
       corner_rot.(Cubie.ncorners - 1) <- Cubie.inv3(!s mod 3);
       { Cubie.id with Cubie.corner_rot = corner_rot }
   end)


module EdgeO = MakeCoord
  (struct
     let length = 2048

     let of_cube cube =
       let n = ref 0 in
       for i = 0 to Cubie.nedges - 2 do
         n := 2 * !n + cube.Cubie.edge_flip.(i)
       done;
       !n

     let to_cube o =
       let edge_flip = Array.make Cubie.nedges (-1) in
       let o = ref o and s = ref 0 in
       for i = Cubie.ncorners - 2 downto 0 do
         let d = !o land 0x1 in         (* mod 2 *)
         edge_flip.(i) <- d;
         s := !s + d;
         o := !o lsr 2                  (* div 2 *)
       done;
       (* (the sum of all orientations) mod 2 = 0 *)
       edge_flip.(Cubie.nedges - 1) <- !s land 0x1; (* -x = x in Z/2Z *)
       { Cubie.id with Cubie.edge_flip = edge_flip }
   end)



module CornerP = MakeCoord
  (struct
     let length = 40320                  (* = 8! *)

     (* See http://kociemba.org/math/coordlevel.htm#cornpermdef *)
     let of_cube cube =
       let n = ref 0 in
       for i = Cubie.ncorners - 1 downto 1 do
         let c = cube.Cubie.corner_perm.(i) in
         let s = ref 0 in
         for j = i - 1 downto 0 do
           if cube.Cubie.corner_perm.(j) > c then incr s
         done;
         n := (!n + !s) * i
       done;
       !n

     let to_cube o =
       
   end)


module EdgeP = MakeCoord
  (struct
     let length = 479001600             (* = 12! *)

     let of_cube cube =
       let n = ref 0 in
       for i = Cubie.nedges - 1 downto 1 do
         let c = cube.Cubie.edge_perm.(i) in
         let s = ref 0 in
         for j = i - 1 downto 0 do
           if cube.Cubie.edge_perm.(j) > c then incr s
         done;
         n := (!n + !s) * i
       done;
       !n

     let to_cube o =
       
   end)


module UDSlice =
struct

end



(* Local Variables: *)
(* compile-command: "make -k rubik.cmo" *)
(* End: *)
