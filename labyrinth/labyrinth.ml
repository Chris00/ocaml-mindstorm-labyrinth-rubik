(* File : labyrinth.ml *)

type dir = [`N | `S | `E | `W]
type dir_rel = [`Left | `Front | `Right | `Back]

let taille_lab = 7;

module Coord :
struct
  type t = int*int

  let compare c1 c2 =
    let (a, b) = c1 in
    let (c, d) = c2 in
    if((a < c) || ((a = c) && (b < d))) then -1
    else
      if ((a = c) && (b = d)) then 0
      else 1

  let nbh (i,j) =
    [(`N, ((i+1), j)); (`S, ((i-1), j)); (`E, (i, (j+1))); (`W, (i, (j-1)))]
end

let square_default = {state = `Non_explored;
                      wall_left = `Unknown;
                      wall_top = `Unknown;}

let lab = Array.make_matrix ((taille_lab * 2) + 1) ((taille_lab * 2) + 1)
  square_default

let current_pos = ref (taille_lab, taille_lab)

let robot_orient = ref `N

let verif_in_lab (i,j) =
  if(i > 2*(Array.length lab)  || i < 0
     || j > 2*(Array.length lab.(0) || j < 0
  then
    raise Not_In_The_Labyrinth;
  ();;

let nbh_explored (i,j) =
  verif_in_lab (i,j);
  let return = ref [] in
  if(i < 2*taille_lab && (wall_on (i,j) `N) = `False
      && (status (i+1,j) = `Explored)) then
    return := (`N, ((i+1), j)) :: !return;
  if(i > 0 && (wall_on (i,j) `S) = `False
      && (status (i-1,j) = `Explored)) then
    return := (`S, ((i-1), j)) :: !return;
  if(j < 2*taille_lab && (wall_on (i,j) `E) = `False
      && (status (i,j+1) = `Explored)) then
    return := (`E, (i, j+1)) :: !return;
  if(j > 0 && (wall_on (i,j) `W) = `False
      && (status (i,j-1) = `Explored)) then
    return := (`W, (i, j-1)) :: !return;
  !return;;

let nbh_unexplored (i,j) =
  verif_in_lab (i,j);
  let return = ref [] in
  if(i < 2*taille_lab && (wall_on (i,j) `N) != `True
      && (status (i+1,j) = `Non_explored)) then
    return := (`N, ((i+1), j)) :: !return;
  if(i > 0 && (wall_on (i,j) `S) != `True
      && (status (i-1,j) = `Non_explored)) then
    return := (`S, ((i-1), j)) :: !return;
  if(j < 2*taille_lab && (wall_on (i,j) `E) != `True
      && (status (i,j+1) = `Non_explored)) then
    return := (`E, (i, j+1)) :: !return;
  if(j > 0 && (wall_on (i,j) `W) != `True
      && (status (i,j-1) = `Non_explored)) then
    return := (`W, (i, j-1)) :: !return;
  !return;;

let wall_on (i,j) d =
  verif_in_lab (i,j);
  if d = `N
  then lab.(i).(j).wall_top
  else if d = `S
  then lab.(i-1).(j).wall_top
  else if d = `W
  then lab.(i).(j).wall_left
  else lab.(i).(j+1).wall_left

let status (i,j) =
  verif_in_lab (i,j);
  lab.(i).(j).state

let position () =
  !current_pos

let robot_dir () =
  !robot_orient

let rel_dir dir_convert =
  let x = ref 0 in
  if dir_convert = `E
  then x := 1;
  if dir_convert = `S
  then x := 2;
  if dir_convert = `w
  then x := 3;
  if !robot_orient = `E
  then x := !x + 3;
  if !robot_orient = `S
  then x := !x + 2;
  if !robot_orient = `W
  then x := !x + 1;
  x := x mod 4;
  match !x with
  | 0 -> `Front
  | 1 -> `Right
  | 2 -> `Back
  | _ -> `Left

let abs_dir dir_convert =
  let x = ref 0 in
  if dir_convert = `Right
  then x := 1;
  if dir_convert = `Back
  then x := 2;
  if dir_convert = `Left
  then x := 3;
  if !robot_orient = `E
  then x := !x + 1;
  if !robot_orient = `S
  then x := !x + 2;
  if !robot_orient = `W
  then x := !x + 3;
  x := x mod 4;
  match !x with
  | 0 -> `N
  | 1 -> `E
  | 2 -> `S
  | _ -> `W

let set_wall d w =
  let (i,j) = !current_pos in
  let d_abs = abs_dir d in
  if d_abs = `N then lab.(i).(j).wall_top = w
  else if d = `S then lab.(i-1).(j).wall_top = w
  else if d = `W then lab.(i).(j).wall_left = w
  else lab.(i).(j+1).wall_left = w

let move d =
  let (i,j) = !current_pos in
  let d_abs = abs_dir d in
  robot_orient := d_abs;
  if d_abs = `N then current_pos := (i+1, j)
  else if d = `S then current_pos := (i-1, j)
  else if d = `W then current_pos := (i, j-1)
  else current_pos := (i, j+1)
