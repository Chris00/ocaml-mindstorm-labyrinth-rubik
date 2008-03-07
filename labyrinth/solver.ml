(* File: solver.ml

   Copyright (C) 2008

     Julie de Pril <julie_87@users.sourceforge.net>

     Christophe Troestler <chris_77@users.sourceforge.net>
     WWW: http://math.umh.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.1 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)


open Mindstorm

(* We parametrise the solver by the connections to the ports (so as to
   change them easily) and by the Labyrinth implementation (so as to
   enrich it esily with e.g. a graphical mode). *)
module Make(C: sig
              val conn : Mindstorm.bluetooth Mindstorm.conn
              val motor_left : Motor.port
              val motor_right : Motor.port
              val motor_ultra : Motor.port
              val light_port : Sensor.port
              val ultra_port : Sensor.port
              val switch_port1 : Sensor.port
              val switch_port2 : Sensor.port

              module Labyrinth : Display.T
            end) =
struct
  open C
  open Printf

  let stop () =
    Motor.set conn Motor.all (Motor.speed 0);
    Sensor.set conn light_port `Light_inactive `Pct_full_scale;
    Mindstorm.close conn;
    exit 0

  (** If the robot finds the exit of the labyrinth, it will call this
      function. *)
  let found_exit () =
    printf "found issue\n%!";
    Labyrinth.success();
    (* Leave the graph displayed (=> do not exit immediately) *)
    Labyrinth.close_when_clicked();
    stop()

  (** If the robot determines that there is no exit to the labyrinth,
      it will call this function. *)
  let no_exit_exists () =
    Labyrinth.failure();
    Mindstorm.Sound.play C.conn "Woops.rso" ~loop:true;
    Unix.sleep 3;
    Mindstorm.Sound.stop C.conn;
    Labyrinth.close_when_clicked();
    printf "no issue\n%!";
    stop()

  (** Continuations taken by the fonctions. *)
  type cont = unit -> unit

  (** Search the closest visited squares satisfying a condition
   ***********************************************************************)

  (* Node and the path from the stating node leading to it *)
  module CoordAndPath =
  struct
    type t = Labyrinth.Coord.t * Labyrinth.dir list
        (* A position in the labyrinth with an associated path saying
           how to reach it. *)

    let compare (p,_) (q,_) = Labyrinth.Coord.compare p q
      (* We only care about not visiting twice the same position, the
         paths do not matter. *)
  end

  module S = struct
    include Set.Make(CoordAndPath)
    let mem p set = mem (p, []) set
      (* We want to find whether [p] is in [set], no matter what the
         associated path is (since the comparison function does not
         care about the path, we can a use the dummy []). *)
  end

  (** [path_to_closer pos cond] search for a square [p] satisfying
      [cond p] as close as possible to the square [pos] while staying
      in the square already visited.  If no such square [p] exists,
      return [[]].  Otherwise, return a list of directions to reach it. *)
  let rec path_to_closer pos cond =
    assert(Labyrinth.status pos <> `Cross_roads);
    let init = S.singleton (pos,[]) in
    search cond init init

  (** [search cond n v] construct an increasing sequance of neighbors
      until a square [p] satisfying [cond p] is found or there is no
      more squares to explore.  [n] is the set of squares that are
      reached from the initial position by a minimal path of k moves
      (where k is the number of times [search] was recursively called)
      and [v] are all previous squares reachable from the initial
      position in k moves or less.  *)
  and search cond n v =
    if S.is_empty n then []
    else
      let n_cond = S.filter cond n in
      if S.is_empty n_cond then begin
        let nbh_of_n (q,path) curr =
          let nbh_q ((n_curr,v_curr) as curr) (dir,pos) =
            if S.mem pos v_curr then curr
            else
              let newp = (pos, dir :: path) in
              (S.add newp n_curr, S.add newp v_curr) in
          List.fold_left nbh_q curr (Labyrinth.nbh_explored q)
        in
        let (n_new,v_new) = S.fold nbh_of_n n (S.empty, v) in
        search cond n_new v_new
      end
      else List.rev(snd(S.choose n_cond))

  let next_square_to_explore ()  =
    let pos = Labyrinth.robot_pos() in
    match Labyrinth.nbh_unexplored pos with
    | (dir,_) :: _ -> [dir]
    | [] ->
        let is_x_roads (sq,_) = Labyrinth.status sq = `Cross_roads in
        match path_to_closer pos is_x_roads with
        | [] -> no_exit_exists()
        | p -> p

  (** Explore the labyrinth
   ***********************************************************************)

  let is_crossing a = a < 25
  let is_path a = a <= 45 && a >= 25
  let is_floor a = a > 45

  let r = Robot.make()

  let run_loop _ = Robot.run r

  let color = Robot.light conn light_port r
  let ultra = Robot.ultrasonic conn ultra_port r
  let touch  =
    Mindstorm.Sensor.set conn switch_port1 `Switch `Bool;
    Mindstorm.Sensor.set conn switch_port2 `Switch `Bool;
    let get port = (Mindstorm.Sensor.get conn port).Mindstorm.Sensor.scaled in
    Robot.meas r (fun () -> (get switch_port1) = 1 || (get switch_port2) = 1)

  let idle = Robot.meas r begin fun () ->
    let (state,_,_,_) = Motor.get conn motor_left in
    state.Motor.run_state = `Idle
  end
  let idle_ultra =
    Robot.meas r (fun () ->
                    let (state,_,_,_) = Motor.get conn motor_ultra in
                    state.Motor.run_state = `Idle)

  let speed motor ?tach_limit sp =
    Motor.set conn motor (Motor.speed ?tach_limit (-sp))

  (** The robot goes straight a little bit without testing if it is on a
      crossing. *)
  let go_straight_before_do tl k =
    Robot.event_is idle (fun _ -> k());
    speed motor_left ~tach_limit:tl 25;
    speed motor_right ~tach_limit:tl 25

  (** The robot goes to the next square, i.e. the next crossing. *)
  let rec go_next_square k =
    Robot.event_is touch (*if wall_on robot_pos() robot_dir() = false*)found_exit;
    Robot.event color is_crossing (fun _ -> k());
    let sp = if Random.bool() then 20 else -20 in
    Robot.event color is_floor (fun _ -> rectif k 20 sp);
    speed motor_left 30;
    speed motor_right 30

  (** The robot rectifies its trajectory to go back to the path. *)
  and rectif k tl sp =
    Robot.event_is touch found_exit;
    Robot.event color is_path (fun _ -> go_next_square k);
    Robot.event color is_crossing (fun _ -> k());
    Robot.event_is idle (fun _ -> rectif k (tl*2) (-sp));
    speed motor_left ~tach_limit:tl (-sp);
    speed motor_right ~tach_limit:tl sp

  (** The robot resets the position of its 'eyes'. *)
  let reset angle k =
    let v = Robot.read ultra in
    Robot.event_is idle_ultra (fun _  -> k v);
    speed motor_ultra ~tach_limit:(abs angle) (if angle >= 0 then 25 else -25)

  (** The robot turns its 'eyes' to look around. *)
  let see_ultra angle k =
    Robot.event_is idle_ultra (fun _ -> reset (-angle) k);
    speed motor_ultra ~tach_limit:(abs angle) (if angle >= 0 then 25 else -25)

  let look_left k =
    see_ultra (-220) (fun a -> Labyrinth.set_wall `Left (a <= 25); k())

  let look_right k =
    see_ultra 220 (fun a -> Labyrinth.set_wall `Right (a <= 25); k())

  let look_front k =
    let v = Robot.read ultra in Labyrinth.set_wall `Front (v <= 25);
    k()

  let look_wall_back k =
    see_ultra (-420) (fun a -> Labyrinth.set_wall `Back (a <= 25); k())

  (** The robot turns on itself. *)
  let turn tl sp k =
    Robot.event_is idle k;
    speed motor_left ~tach_limit:tl (-sp);
    speed motor_right ~tach_limit:tl sp

  let rec search_crossing tl sp k =
    Robot.event color is_crossing (fun _ -> k());
    Robot.event_is idle (fun _ -> search_crossing (tl*2) (-sp) k);
    speed motor_left ~tach_limit:tl (-sp);
    speed motor_right ~tach_limit:tl sp

  let go_back_before_do k =
    Robot.event_is idle (fun _ -> k());
    speed motor_left ~tach_limit:180 (-20);
    speed motor_right ~tach_limit:180 (-20)

  let look_walls k =
    speed motor_left 0;
    speed motor_right 0;
    let wall d =
      Labyrinth.wall_on (Labyrinth.robot_pos()) (Labyrinth.abs_dir d)  in
    if wall `Left = `Unknown then
      look_left (if wall `Right = `Unknown then
                   fun _ -> look_right (if wall `Front = `Unknown then
                                         fun _ -> look_front k else k)
                 else if wall `Front = `Unknown then
                   fun _ -> look_front k
                 else k)
    else if wall `Right = `Unknown then
      look_right (if wall `Front = `Unknown then (fun _ -> look_front k)
                  else k)
    else if wall `Front = `Unknown then
      look_front k
    else k()

  let go_straight k = Labyrinth.move `Front;
    go_straight_before_do 220 (fun _ -> go_next_square k)

  let go_left k = Labyrinth.move `Left;
    go_straight_before_do 180 (fun _ -> turn 180 25
      (fun _  -> go_straight_before_do 100 (fun _ -> go_next_square k)))

  let go_right k = Labyrinth.move `Right;
    go_straight_before_do 180 (fun _ -> turn 180 (-25)
      (fun _  -> go_straight_before_do 100 (fun _ -> go_next_square k)))

  let go_back k = Labyrinth.move `Back;
    go_straight_before_do 180 (fun _ -> turn 360 25
      (fun _  -> go_straight_before_do 100 (fun _ -> go_next_square k)))


  let rec follow_path k path =
    Labyrinth.set_current_path path;
    match path with
    | [] -> k()
    | d :: tl -> match Labyrinth.rel_dir d with
      | `Left -> go_left (fun _ -> follow_path k tl)
      | `Right -> go_right (fun _ -> follow_path k tl)
      | `Front -> go_straight (fun _ -> follow_path k tl)
      | `Back -> go_back (fun _ -> follow_path k tl)

end


(* Local Variables: *)
(* compile-command: "make -k solver.cmo" *)
(* End: *)
