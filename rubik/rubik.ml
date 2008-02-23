(* File: rubik.ml

   Copyright (C) 2008

     Christophe Troestler <chris_77@users.sourceforge.net>
     WWW: http://math.umh.ac.be/an/software/

     Julie de Pril <julie_87@users.sourceforge.net>

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.1 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)


(** This module implements various datastructures (partially)
    representing the Rubik cube.

    The main inspiration for this code is the work of Herbert Kociemba
    <http://kociemba.org/cube.htm>.  If Kociemba writing is too
    technical to you, you may want to start with Peter Lubans prose
    <http://bj.middlebury.edu/~plubans/report> and follow with Jaap's
    Puzzle Page <http://www.geocities.com/jaapsch/puzzles/compcube.htm>
    but be sure to finally return to Kociemba pages as they contain
    the most precise explanations.

    On the mathematical theory of groups, using the Rubik cube as a
    motivation, you can consult:

    - David Joyner, Mathematics of the Rubik's cube,
    http://www.permutationpuzzles.org/rubik/webnotes/ (LaTeX tarball)
    http://cadigweb.ew.usna.edu/~wdj/rubik/

    - Tom Davis, Group Theory via Rubik's Cube,
    http://www.geometer.org/rubik/group.pdf

    Finally Michael Reid's Rubik's cube page
    <http://www.math.ucf.edu/~reid/Rubik/index.html> has a nice
    collection of links, in particular featuring his own optimal cube
    solver <http://www.math.ucf.edu/~reid/Rubik/optimal_solver.html>.
*)

(* http://www-cs-staff.Stanford.EDU/~knuth/preprints.html
   Efficient representation of perm groups. (Sims's algorithm)
   http://www-cs-staff.stanford.edu/~knuth/papers/erpg.tex.gz *)


open Bigarray

(** Helpers
 ***********************************************************************)

(* Enrich List *)
module List = struct
  include List
  let iteri f l =
    let rec iter i = function
      | [] -> ()
      | a :: tl -> f i a; iter (succ i) tl in
    iter 0 l
end

let max3 (i:int) j k =
  if i <= j then   if j <= k then k else j
  else (* i > j *) if i <= k then k else i

(* binomial coefficients *)
let choose n k =
  if n <= 0 || k < 0 || k > n then 0
  else
    let k = if k > n/2 then n-k else k in
    let fn = float n and fk = float k in
    let acc = ref 1. in
    for i = 0 to k-1 do
      let fi = float i in
      acc := !acc /. (fk -. fi) *. (fn -. fi);
    done;
    truncate(!acc +. 0.5)               (* round *)

(************************************************************************)

type generator = F | B | L | R | U | D

let generator = [| F; B; L; R; U; D |]


module Move =
struct

  type t = int
    (* F -> 0, F^2 -> 1, F^3 -> 2, B -> 3, B^2 -> 4,..., D^3 -> 17.
       It is important that the fact that is is an int is known by the
       submodules because it will be used as an index of move
       tables. *)

  let length = 18                         (* to iterate on the moves *)

  (* We use [(g, e)] because that is how we want the solution of the
     rubik cube to be presented. *)
  let make (g, e) =
    if e < 1 || e > 3 then
      invalid_arg "Rubik.move: exponent must be 1, 2, or 3";
    match g with
    | F -> e - 1
    | B -> e + 2
    | L -> e + 5
    | R -> e + 8
    | U -> e + 11
    | D -> e + 14

  let generator m = (generator.(m / 3), (m mod 3) + 1)

end


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
    corner_rot: int array;              (* 0,1,2: CW rotation of 120� *)
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
  let cube_of_move =
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

  let move (m: Move.t) = cube_of_move.(m)

  let () = assert(Array.length cube_of_move = Move.length)
end


module type Coordinate =
sig
  type t
  type move
  val of_cube : Cubie.t -> t
    (** Returns the coordinate of a cube. *)
  val initialize : ?file:string -> unit -> (t -> move -> t) * (t -> int)
end

(* Common coordinates structure.
   ----------------------------------------------------------------------

   We first wanted to functorize it but, apart from the possible
   performance penalty (this part of the code is critical), it is a
   bit heavy to parametrize by the Bigarray.kind -- no existential
   types => have to abstract (int,_,c_layout) Array.2 and redefine
   the needed needed array operations on the new type...

   Since eventually, only the initialize function is shared, a macro
   was deemed simpler.  For INITIALIZE_MUL, we assume that the following
   values are defined:
   - val length : int
   - val of_cube : Cubie.t -> int
   - val to_cube : int -> Cubie.t
     (* Generate a cube with the orientation represented by the number. *)
*)
DEFINE INITIALIZE_MUL(kind) =
  let mul_table = Array2.create kind c_layout length Move.length in
  for o = 0 to length - 1 do
    let cube = to_cube o in
    for m = 0 to Move.length - 1 do
      mul_table.{o,m} <- of_cube(Cubie.mul cube (Cubie.move m))
    done
  done;
  mul_table
;;
DEFINE INITIALIZE_PRUN(a) =
  let prun_table = Array1.create int8_unsigned c_layout length in
  (* The array [taken] enables us to know whether we have already taken a
     cube to compute its value in the prun_table or not. *)
  let taken = Array.make length false in
  (* [is_next m next] tells us if the move [next] can be applied after the
     move [m] or not. *)
  let is_next m next = let gen = fst(Move.generator m) in
                       next <> Move.make (gen,1) && next <> Move.make (gen,2)
                       && next <> Move.make (gen,3) in
  prun_table.{id} <- 0; (* This is the goal state. *)
  taken.(id) <- true;
  let rec fill_table n cond_move taken cube_coord =
    (* n counts the number of already taken cubes and cond_move is a
       condition to know whether a move can be applied or not.*)
    if n <= length then
      let cube = to_cube cube_coord in
      for m = 0 to Move.length - 1 do
        if cond_move m then
          let newc = of_cube (Cubie.mul cube (Cubie.move m)) in
          if not taken.(newc) then
            (prun_table.{newc} <- prun_table.{cube_coord} + 1;
             (* One more move to bring the cube back to the goal state. *)
             taken.(newc) <- true;
             fill_table (n+1) (fun next -> is_next m next) taken newc)
      done in
  fill_table 1 (fun _ -> true) taken id; (* The function [fun _ -> true] allows
                                        us to apply all the existing moves. *)
  prun_table
;;
(* This must be a macro so that the type of the tables is monomorphic and
   the compiler generates efficient access to them. *)
DEFINE INITIALIZE_FILE(initialize_mul, initialize_prun) =
  let mul_table, prun_table = match file with
    | None -> (initialize_mul(), initialize_prun())
    | Some fname ->
        if Sys.file_exists fname then begin
          let fh = open_in_bin fname in
          let mul_table : (int, _, c_layout) Array2.t = input_value fh in
          let prun_table : (int, _, c_layout) Array1.t = input_value fh in
          (* may segfault if the file does not contain the right values! *)
          close_in fh;
          (mul_table, prun_table)
        end
        else begin
          (* Compute and save the table *)
          let mul_table = initialize_mul() in
          let prun_table = initialize_prun() in
          let fh = open_out_bin fname in
          output_value fh mul_table;
          output_value fh prun_table;
          close_out fh;
          (mul_table, prun_table)
        end in
  (fun o m -> mul_table.{o,m}),          (* [mul] function hiding the table *)
  (fun o -> prun_table.{o})              (* pruning function *)
;;
DEFINE INITIALIZE(kind) =
  let initialize_mul () = INITIALIZE_MUL(kind) in
  let initialize_prun () = INITIALIZE_PRUN() in
  INITIALIZE_FILE(initialize_mul, initialize_prun)
;;

module CornerO =
struct
  type t = int                         (* 0 .. 2186 = 2^7 - 1 *)
  let length = 2187
  type move = Move.t

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

  let id = of_cube Cubie.id

  let initialize ?file () = INITIALIZE(int16_unsigned)
end


module EdgeO =
struct
  type t = int                          (* 0 .. 2047 = 2^11 - 1 *)
  let length = 2048
  type move = Move.t

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

  let id = of_cube Cubie.id

  let initialize ?file () = INITIALIZE(int16_unsigned)
end


(* See http://kociemba.org/math/coordlevel.htm#cornpermdef
   If p = [| p0; p1; ...; pj; ...; p(n-1) |], then
   si = #{ j < i | pj < pi }.  s0 = 0 so not computed.  The returned number
   is s1 1! + s2 2! + ... + s(n-1) (n-1)!

   Remark: contrarily to Kociemba we use pj < pi instead of pj > pi
   as then p(N-1) = s(N-1) instead of p(N-1) = N - 1 - s(N-1),... *)
let int_of_perm p n =
  let s = ref 0 in
  for i = n - 1 downto 1 do
    let pi = p.(i) in
    let si = ref 0 in
    for j = i - 1 downto 0 do if p.(j) < pi then incr si done;
    s := (!s + !si) * i
  done;
  !s

(* The inverse funtion of [int_of_perm], fill [p.(i0 .. i0+n-1]
   instead of returning the permutation. *)
let perm_of_int perm p i0 n =
  p.(i0) <- 0; (* s0 = 0 *)
  let perm = ref perm in
  for i = 1 to n - 1 do
    let i1 = i + 1 in
    let si = !perm mod i1 in
    let idx = i0 + i in
    p.(idx) <- si;
    for j = idx - 1 downto i0 do if p.(j) >= si then p.(j) <- p.(j) + 1 done;
    perm := !perm / i1;
  done

module CornerP =
struct
  type t = int
  let length = 40_320                  (* = 8! *)
  type move = Move.t

  let of_cube cube = int_of_perm cube.Cubie.corner_perm Cubie.ncorners

  let to_cube perm =
    let p = Array.make Cubie.ncorners 0 in
    perm_of_int perm p 0 Cubie.ncorners;
    { Cubie.id with Cubie.corner_perm = p }

  let id = of_cube Cubie.id

  let initialize ?file () = INITIALIZE(int16_unsigned)
end


module EdgeP =
struct
  type t = int
  let length = 479_001_600             (* = 12! *)
    (* WARNING: This module is implemented for the record.  The
       [length] is too large to actually build the multiplication array. *)
  type move = Move.t

  let of_cube cube = int_of_perm cube.Cubie.edge_perm Cubie.nedges

  let to_cube perm =
    let p = Array.make Cubie.nedges 0 in
    perm_of_int perm p 0 Cubie.nedges;
    { Cubie.id with Cubie.edge_perm = p }

  let id = of_cube Cubie.id

  let initialize ?file () = INITIALIZE(int)
end



module UDSlice =
struct
  type t = int                          (* 0 .. 494 = 12*11*10*9/4! - 1 *)
  let length = 495
  type move = Move.t

  (* Assume: FR < FL < BL < BR and to be after all other edges
     (for [Cubie.int_of_edge].  *)
  let fr = Cubie.int_of_edge Cubie.FR
  let fl = Cubie.int_of_edge Cubie.FL
  let bl = Cubie.int_of_edge Cubie.BL
  let br = Cubie.int_of_edge Cubie.BR

  let of_cube cube =
    let edge = cube.Cubie.edge_perm in
    let occupied = Array.make Cubie.nedges false in
    for i = 0 to Cubie.nedges - 1 do
      if edge.(i) >= fr then occupied.(i) <- true
    done;
    let s = ref 0
    and k = ref 3
    and n = ref (Cubie.nedges - 1) in
    while !k >= 0 do
      if occupied.(!n) then decr k else s := !s + choose !n !k;
      decr n;
    done;
    !s

  let id = of_cube Cubie.id

  let initialize_mul() =
    let mul_table = Array2.create int16_unsigned c_layout length Move.length in
    (* Generate all possibilities of placing the 4 UDSlice edges
       directly as a permutation [p]. *)
    let p = Array.init Cubie.nedges (fun i -> i) (* id perm *) in
    let exchange k l = let pk = p.(k) in p.(k) <- p.(l); p.(l) <- pk in
    for i1 = 0 to Cubie.nedges - 4 do
      exchange fr i1;
      for i2 = i1 + 1 to Cubie.nedges - 3 do
        exchange fl i2;
        for i3 = i2 + 1 to Cubie.nedges - 2 do
          exchange bl i3;
          for i4 = i3 + 1 to Cubie.nedges - 1 do
            exchange br i4;
            let c = { Cubie.id with Cubie.edge_perm = p } in
            for m = 0 to Move.length - 1 do
              mul_table.{of_cube c,m} <- of_cube(Cubie.mul c (Cubie.move m))
            done;
            exchange br i4;             (* undo *)
          done;
          exchange bl i3;
        done;
        exchange fl i2;
      done;
      exchange fr i1;
    done;
    mul_table

  let initialize_prun () =
    Array1.create int8_unsigned c_layout length (* FIXME: to complete *)

  let initialize ?file () = INITIALIZE_FILE(initialize_mul, initialize_prun)
end


module Phase1 =
struct
  type t = CornerO.t * EdgeO.t * UDSlice.t
      (* 2187 * 2048 * 495 = 2_217_093_120 possibilities *)
  type move = Move.t

  let of_cube c =
    (CornerO.of_cube c, EdgeO.of_cube c, UDSlice.of_cube c)

  let in_G1 (c,e,u) = c = 0 && e = 0 && u = 0

  (* FIXME: lazy initialize so they share the same matrices? *)
  let initialize ?file () =
    let file1, file2, file3 = match file with
      | None -> None, None, None
      | Some f ->
          Some(f ^ ".cornero"), Some(f ^ ".edgeo"), Some(f ^ "usd1") in
    let mulC, prunC = CornerO.initialize ?file:file1 ()
    and mulE, prunE = EdgeO.initialize ?file:file2 ()
    and mulU, prunU = UDSlice.initialize ?file:file3 () in
    let mul (c,e,u) m = (mulC c m, mulE e m, mulU u m)
    and prun (c,e,u) = max3 (prunC c) (prunE e) (prunU u) in
    (* FIXME: is this pruning method good enough? *)
    (mul, prun)
end

(* Authorized moves in the phase 2 of the algo (safety and possibly
   memory gains). *)
module Move2 =
struct
  include Move
    (* We do not distinguish them from Move for now because memory is
       not an issue.  (If we ever need more memory, move the phase 2
       moves first in Move.t.) *)

  external to_move : t -> Move.t = "%identity"
end


(* Permutation of Edges coordinates; only valid in phase 2 *)
module EdgeP2 =
struct
  type t = int                          (* 0 .. 40319 *)
  let length = 40320
  type move = Move2.t

  let nedges = Cubie.nedges - 4         (* U & D edges only *)

  (* ASSUME: FR, FL, BL, BR are given that last indices by [int_of_edge]. *)
  let of_cube cube = int_of_perm cube.Cubie.edge_perm nedges

  let to_cube perm =
    let p = Array.init Cubie.nedges (fun i -> i) in (* last 4 untouched *)
    perm_of_int perm p 0 nedges;
    { Cubie.id with Cubie.edge_perm = p }

  let id = of_cube Cubie.id

  let initialize ?file () = INITIALIZE(int16_unsigned)
end

(* Permutation of the 4 edge cubies; only valid in phase 2 *)
module UDSlice2 =
struct
  type t = int                          (* 0 .. 23 *)
  let length = 24
  type move = Move2.t

  let fr = Cubie.int_of_edge Cubie.FR
  let first = 0
  let nedges = 4

  (* Contrarily to http://kociemba.org/math/twophase.htm#phase2udslice
     these coordinates are not an extension of the {!Rubik.UDSlice} ones. *)
  let of_cube cube =
    int_of_perm (Array.sub cube.Cubie.edge_perm first nedges) nedges

  let to_cube perm =
    let p = Array.init Cubie.nedges (fun i -> i) in (* only last 4 touched *)
    perm_of_int perm p first nedges;
    { Cubie.id with Cubie.edge_perm = p }

  let id = of_cube Cubie.id

  let initialize ?file () = INITIALIZE(int16_unsigned)
end


module Phase2 =
struct
  type t = CornerP.t * EdgeP2.t * UDSlice2.t
      (* 40320 * 40320 * 24 = 39_016_857_600 possibilities *)
  type move = Move2.t
  external to_move : move -> Move.t = "%identity"

  let of_cube c =
    (CornerP.of_cube c, EdgeP2.of_cube c, UDSlice2.of_cube c)

  let is_identity (c,e,u) =
    c = CornerP.id && e = EdgeP2.id && u = UDSlice2.id

  (* FIXME: lazy initialize so they share the same matrices? *)
  let initialize ?file () =
    let file1, file2, file3 = match file with
      | None -> None, None, None
      | Some f ->
          Some(f ^ ".cornerp"), Some(f ^ ".edgep2"), Some(f ^ "usd2") in
    let mulC, prunC = CornerP.initialize ?file:file1 ()
    and mulE, prunE = EdgeP2.initialize ?file:file2 ()
    and mulU, prunU = UDSlice2.initialize ?file:file3 () in
    let mul (c,e,u) m = (mulC c m, mulE e m, mulU u m)
    and prun (c,e,u) = max3 (prunC c) (prunE e) (prunU u) in
    (mul, prun)
end



(* Local Variables: *)
(* compile-command: "make -k rubik.cmo" *)
(* End: *)
