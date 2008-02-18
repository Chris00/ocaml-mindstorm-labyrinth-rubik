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
              val switch_port : Sensor.port

              module Labyrinth : Display.T
            end) =
struct
  open C
  open Printf

  let stop _ =
    Motor.set conn Motor.all (Motor.speed 0);
    Sensor.set conn light_port `Light_inactive `Pct_full_scale;
    close conn

  (** If the robot determines that there is no exit to the labyrinth,
      it will call this function. *)
  let no_exit_lab () = (* Print on the screen that there is no issue *)
    (* Mindstorm.Sound.play C.conn "Woops.rso" ~loop:true;
    Unix.sleep 3;
    Mindstorm.Sound.stop C.conn;*)
    printf "no issue\n%!";
    stop();
    exit 1

  (** If the robot finds the exit of the labyrinth, it will call this
      function. *)
  let found_exit () =
    Labyrinth.success();
    (* Leave the graph displayed (=> do not exit immediately) *)
    Unix.sleep 60;                      (* FIXME *)
    stop();
    exit 0

  (** Continuations taken by the fonctions. *)
  type cont = unit -> unit

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
         care about the path, we can a use dummy []). *)
  end

  (** [path_to_closer pos cond] search for a square [p] satisfying
      [cond p] as close as possible to the square [pos] while staying
      in the square already visited.  If no such square [p] exists,
      return [[]].  Otherwise, return a list of directions to reach it. *)
  let rec path_to_closer pos cond =
    let init = S.singleton (pos,[]) in
    search cond init init

  (** [search cond n v] construct an increasing sequance of neighbors
      until a square [p] satisfying [cond p] is found or there is no
      more squares to explore.  [n] is the set of squares that are
      reached from the initial position by a minimal path of k moves
      (where k is the number of times [search] was called) and [v] are
      all previous squares reachable from the initial position in k
      moves or less.  *)
  and search cond n v =
    if S.is_empty n then []
    else let x_roads =
      S.filter (fun (sq,_) -> (Labyrinth.status sq) = `Cross_roads) n in
    if not(S.is_empty x_roads) then snd (S.choose x_roads)
    else let get_nbh (q,dl) (nG,vG) = let select_nbh (nS,vS) (d,p) =
      if S.mem p v then (nS,vS)
      else let newp = (p,List.rev (d :: dl)) in
      (S.add newp nS, S.add newp vS)
    in let l = Labyrinth.nbh_explored q in
    let (nF,vF) = List.fold_left select_nbh (S.empty,v) l in
    (S.union n nF, S.union v vF) in
    let (n_new,v_new) = S.fold get_nbh n (S.empty, S.empty) in
    search n_new v_new

  let next_case_to_explore ()  =
    let pos = Labyrinth.robot_pos() in
    let unexpl = Labyrinth.nbh_unexplored pos in
    match unexpl with
    | (dir,_) :: _ -> [dir]
    | [] ->
        let is_x_roads (sq,_) = Labyrinth.status sq = `Cross_roads in
        match path_to_closer pos is_x_roads with
        | [] -> no_exit_lab()
        | p -> p

  let is_crossing a = a < 30
  let is_path a = a < 45 && a > 30
  let is_floor a = a > 45

  let r = Robot.make()

  let run_loop _ = Robot.run r

  let color = Robot.light conn light_port r
  let ultra = Robot.ultrasonic conn ultra_port r
  let touch = Robot.touch conn switch_port r

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
  let go_straight_before_do k =
    Robot.event_is touch found_exit;
    Robot.event_is idle (fun _ -> k());
    speed motor_left ~tach_limit:180 40;
    speed motor_right ~tach_limit:180 40

  (** The robot goes to the next square, i.e. the next crossing. *)
  let rec go_next_square k =
    Robot.event_is touch found_exit;
    Robot.event color is_crossing (fun _ -> k());
    let sp = if Random.bool() then 30 else -30 in
    Robot.event color is_floor (fun _ -> rectif k 40 sp);
    speed motor_left 45;
    speed motor_right 45

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
    see_ultra (-200) (fun a -> Labyrinth.set_wall `Left (a <= 20); k())

  let look_right k =
    see_ultra 200 (fun a -> Labyrinth.set_wall `Right (a <= 20); k())

  let look_front k =
    let v = Robot.read ultra in Labyrinth.set_wall `Front (v <= 20);
    k()

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

  let look_wall_back k =
    search_crossing 20 30 (fun _ -> turn 360 30
      (fun _ -> Labyrinth.set_wall `Back ((Robot.read ultra) <= 20);
        turn 360 30 (fun _ -> search_crossing 20 30 k)))

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
    go_straight_before_do (fun _ -> go_next_square k)

  let go_left k = Labyrinth.move `Left;
    go_straight_before_do (fun _ -> turn 180 40 (fun _ -> go_next_square k))

  let go_right k = Labyrinth.move `Right;
    go_straight_before_do (fun _ -> turn 180 (-40) (fun _ -> go_next_square k))

  let go_back k = Labyrinth.move `Back;
    go_straight_before_do (fun _ -> turn 360 40 (fun _ -> go_next_square k))

  let rec follow_path k path = match path with
    | [] -> k()
    | d :: tl -> match (Labyrinth.rel_dir d) with
      | `Left -> go_left (fun _ -> follow_path k tl)
      | `Right -> go_right (fun _ -> follow_path k tl)
      | `Front -> go_straight (fun _ -> follow_path k tl)
      | `Back -> go_back (fun _ -> follow_path k tl)

end


(* Local Variables: *)
(* compile-command: "make -k solver.cmo" *)
(* End: *)
