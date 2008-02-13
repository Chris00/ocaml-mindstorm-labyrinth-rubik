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
    if(a < c)
    then -1
    else
      if(b<d)
      then -1
      else
        if(b>d)
        then 1
        else 0

  let nbh (i,j) =
    [(`N, ((i+1), j)); (`S, ((i-1), j)); (`E, (i, (j+1))); (`W, (i, (j-1)))]
end

let square_default = {state = `Non_explored;
                      wall_left = `Unknown;
                      wall_top = `Unknown;}
let lab = Array.make_matrix ((taille_lab * 2) + 1) ((taille_lab * 2) + 1)
  square_default

let current_pos = ref ((taille_lab, taille_lab):Coord.t)

let robot_orient = ref `N

let verif_in_lab (i,j) =
  if(i > taille_lab || i < (-1) * taille_lab || j > taille_lab || j < (-1)
     * taille_lab)
  then
    raise Not_In_The_Labyrinth;
  ();;

let nbh_explored (i,j) =
  verif_in_lab (i,j);
  let return = [] in
  if(i < taille_lab && (wall_on (i,j) `N) = `False
      && (status (i+1,j) = Explored)) then
    return = (`N, ((i+1), j)) :: return;
  if(i > ((-1) * taille_lab) && (wall_on (i,j) `S) = `False
      && (status (i-1,j) = Explored)) then
    return = (`S, ((i-1), j)) :: return;
  if(j < taille_lab && (wall_on (i,j) `E) = `False
      && (status (i,j+1) = Explored)) then
    return = (`E, (i, j+1)) :: return;
  if(j > ((-1) * taille_lab) && (wall_on (i,j) `W) = `False
      && (status (i,j-1) = Explored)) then
    return = (`W, (i, j-1)) :: return;
  return;;

let nbh_unexplored (i,j) =
  verif_in_lab (i,j);
  [];;

let wall_on (i,j) d =
  let retour = ref `Unknow in
  if d = `N
  then retour := lab.(i).(j).wall_top;
  if d = `S
  then retour := lab.(i-1).(j).wall_top;
  if d = `W
  then retour := lab.(i).(j).wall_left;
  if d = `E
  then retour := lab.(i).(j-1).wall_left;
  !retour

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
  let (i,j) = current_pos in
  if d = `N
  then lab.(i).(j).wall_top = `True;
  if d = `S
  then lab.(i-1).(j).wall_top = `True;
  if d = `W
  then lab.(i).(j).wall_left = `True;
  if d `E
  then lab.(i).(j-1).wall_left = `True;
  ();;

let move d =
  ();
