open Printf
module Sensor = Mindstorm.Sensor
module Motor = Mindstorm.Motor

let light_port = `S3
let ultrasonic_port = `S4
let motor_ultrasonic = Motor.c
let motor_left = Motor.a
let motor_right = Motor.b

module Run(C: sig val conn : Mindstorm.bluetooth Mindstorm.conn end) =
struct
  (* Le robot roule en suivant une ligne verte au sol (sur fond
     blanc).  S'il voit de la couleur noire, il regarde a droite et a
     gauche s'il y a des chemins possibles. *)

  (* Initialisation, at functor instantiation *)
  let () =
    Sensor.set C.conn light_port `Light_active `Pct_full_scale;
    let stop _ =
      Motor.set C.conn Motor.all (Motor.speed 0);
      Sensor.set C.conn light_port `Light_inactive `Pct_full_scale;
      Mindstorm.close C.conn;
      printf "\n";
      exit 0 in
    Sys.set_signal Sys.sigint (Sys.Signal_handle stop);
    printf "Press Ctrl-c to quit.\n%!"
  ;;

  let is_crossing a = a < 33
  let is_path a = a < 45 && a > 33
  let is_floor a = a > 45

  let speed motor ?tach_limit sp =
    Motor.set C.conn motor (Motor.speed ?tach_limit (-sp))

  let r = Robot.make()
  let color =
    Robot.meas r (fun () -> (Sensor.get C.conn light_port).Sensor.scaled)
  let idle = Robot.meas r (fun () ->
                             let (state,_,_,_) = Motor.get C.conn motor_left in
                             state.Motor.run_state = `Idle)
  let idle_ultra =
    Robot.meas r (fun () ->
                    let (state,_,_,_) = Motor.get C.conn motor_ultrasonic in
                    state.Motor.run_state = `Idle)

  let ultra_conn = Sensor.Ultrasonic.make C.conn ultrasonic_port
  let look () = Mindstorm.Sensor.Ultrasonic.get ultra_conn `Byte0

  let reset angle f g =
    let v = f() in
    Robot.event_is idle_ultra (fun _ -> g v);
    speed motor_ultrasonic ~tach_limit:(abs angle)
      (if angle >= 0 then 25 else (-25))

  let turn_and_do angle f g =
    Robot.event_is idle_ultra (fun _ -> reset (-angle) f g);
    speed motor_left 0;
    speed motor_right 0;
    speed motor_ultrasonic ~tach_limit:(abs angle)
      (if angle >= 0 then 25 else (-25))

  let rec rectif tl sp =
    Robot.event color is_path (fun _ -> go_straight());
    Robot.event color is_crossing (fun _ -> look_left());
    Robot.event_is idle (fun _ -> rectif (tl*2) (-sp));
    speed motor_left ~tach_limit:tl (-sp);
    speed motor_right ~tach_limit:tl sp

  and turn tl sp =
    Robot.event_is idle go_straight;
    speed motor_left ~tach_limit:tl (-sp);
    speed motor_right ~tach_limit:tl sp

  and go_straight_before_do f =
    Robot.event_is idle (fun _ -> f());
    speed motor_left ~tach_limit:180 40;
    speed motor_right ~tach_limit:180 40

  and go_straight () =
    let sp = if Random.bool() then 15 else -15 in
    Robot.event color is_floor (fun _ -> rectif 30 sp);
    Robot.event color is_crossing (fun _ -> look_left());
    speed motor_left 45;
    speed motor_right 45

  and look_left () =
    speed motor_left 0;
    speed motor_right 0;
    turn_and_do 90 look
      (fun a -> if a > 30 then go_straight_before_do (fun _ -> turn 180 40)
        else look_front())

  and look_front () =
    let v = look() in
    if v > 30 then go_straight_before_do go_straight else look_right()

  and look_right () =
    turn_and_do (-90) look
       (fun a -> if a > 30 then go_straight_before_do (fun _ -> turn 180 (-40))
         else turn 360 50)

  let run() =
    Sensor.Ultrasonic.set ultra_conn `Meas_cont;
    go_straight ();
    Robot.run r

end

let () =
  let bt =
    if Array.length Sys.argv < 2 then (
      printf "%s <bluetooth addr>\n" Sys.argv.(0);
      exit 1;
    )
    else Sys.argv.(1) in
  let conn = Mindstorm.connect_bluetooth bt in
  let module R = Run(struct let conn = conn end) in
  R.run()
