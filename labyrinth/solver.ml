(* File : solver.ml *)

module Make(C: sig
  val conn : 'a Mindstorm.conn
  val motor_left : Mindstorm.Motor.port
  val motor_right : Mindstorm.Motor.port
  val motor_ultra : Mindstorm.Motor.port
  val light_port : Mindstorm.Sensor.port
  val ultra_port : Mindstorm.Sensor.port
end) =
struct

let make_rel_dir l = List.map Labyrinth.rel_dir l

let search acc l =
  match l with
  |[] -> []
  |sqr::tl ->
     if Labyrinth.status sqr = `Cross_roads then acc
     else search (fst sqr)::acc tl

let next_case_to_explore coord =
  let square = Labyrinth.position() in
  let unexpl = Labyrinth.nbh_unexplored square in
  match unexpl with
  | (dir,_) :: _ -> Labyrinth.rel_dir dir
  | [] ->  let l = search [] (Labyrinth.nbh_explored square) in
           if l = [] then ()(* Pas de sortie! *) 
           else make_rel_dir (List.rev l)

type cont = unit -> unit

let is_crossing a = a < 30
let is_path a = a < 45 && a > 30
let is_floor a = a > 45

let r = Robot.make()

let color = Robot.light C.conn C.light_port r
let ultra = Robot.ultrasonic C.conn C.ultra_port r

let idle = Robot.meas r (fun () ->
  let (state,_,_,_) = Motor.get C.conn C.motor_left in
  state.Motor.run_state = `Idle)
let idle_ultra =
  Robot.meas r (fun () ->
    let (state,_,_,_) = Motor.get C.conn C.motor_ultra in
    state.Motor.run_state = `Idle)

let speed motor ?tach_limit sp =
  Motor.set C.conn motor (Motor.speed ?tach_limit (-sp))

let go_straight_before_do k =
  Robot.event_is idle (fun _ -> k());
  speed C.motor_left ~tach_limit:180 40;
  speed C.motor_right ~tach_limit:180 40

let rec rectif k tl sp =
  Robot.event color is_path (fun _ -> go_next_square k);
  Robot.event color is_crossing (fun _ -> k());
  Robot.event_is idle (fun _ -> rectif k (tl*2) (-sp));
  speed C.motor_left ~tach_limit:tl (-sp);
  speed C.motor_right ~tach_limit:tl sp

and go_next_square k =
  Robot.event color is_crossing (fun _ -> k());
  Robot.event color is_floor (fun _ -> rectif k 40 sp);
  speed C.motor_left 45;
  speed C.motor_right 45

let turn tl sp k =
  Robot.event_is idle go_straight;
  speed C.motor_left ~tach_limit:tl (-sp);
  speed C.motor_right ~tach_limit:tl sp

let reset angle k =
  let v = Robot.read ultra in
  Robot.event_is idle_ultra (fun _  -> k v);
  speed C.motor_ultra ~tach_limit:(abs angle)
    (if angle >= 0 then 25 else -25)

let see_ultra angle k =
  Robot.event_is idle_ultra (fun _ -> reset (-angle) k);
  speed C.motor_ultra ~tach_limit:(abs angle)
    (if angle >= 0 then 25 else -25)

let look_left k = see_ultra (-200) begin fun a ->
  set_wall `Left !(a > 40); k() end

let look_right k = see_ultra 200 begin fun a ->
  set_wall `Right !(a > 40); k() end

let look_front k =
  let v = Robot.read ultra in set_wall `Front !(v > 40);
  k()

let look_walls k =
  speed C.motor_left 0;
  speed C.motor_right 0;
  let wall d = Labyrinth.wall_on Labyrinth.position() (Labyrinth.abs_dir d)  in
  if wall `Left = `Unknown then
    look_left (if wall `Right = `Unknown then
      fun _ -> look_right (if wall `Front = `Unknown then
        fun _ -> look_front k else k)
      else (if wall `Front = `Unknown then
        fun _ -> look_front k else k))
  else (if wall `Right = `Unknown then
      fun _ -> look_right (if wall `Front = `Unknown then
        fun _ -> look_front k else k)
      else (if wall `Front = `Unknown then
        fun _ -> look_front k else k))

let rec follow_path k path = match path with
    |[] -> k()
    |Left::tl -> go_left (fun _ -> follow_path k tl)
    |Right::tl -> go_right (fun _ -> follow_path k tl)
    |Straight::tl -> go_straight (fun _ -> follow_path k tl)
    |Back::tl -> go_back (fun _ -> follow_path k tl)

let go_left k = Labyrinth.move `Left;
    go_straight_before_do (fun _ -> turn 180 40 (go_next_square k))

let go_right k = Labyrinth.move `Right;
    go_straight_before_do (fun _ -> turn (-180) 40 (go_next_square k))

let go_straight k = Labyrinth.move `Front;
    go_straight_before_do (go_next_square k)

let go_back k = Labyrinth.move `Back;
    go_straight_before_do (fun _ -> turn 360 40 (go_next_square k))

end
