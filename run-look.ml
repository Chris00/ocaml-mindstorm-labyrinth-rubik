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

  let is_black a = a < 28
  let is_green a = a < 45 && a > 33
  let is_white a = a > 45

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

  let ultra =
    let u = Sensor.Ultrasonic.make C.conn ultrasonic_port in
    Sensor.Ultrasonic.set u `Meas_cont;
    Robot.meas r (fun _ -> Mindstorm.Sensor.Ultrasonic.get u `Byte0)

  let reset_pos_ultra bool f =
    Robot.event_is idle_ultra f;
    let (_,tc,_,_) = Motor.get C.conn motor_ultrasonic in
    speed motor_ultrasonic ~tach_limit:tc (if bool then (-25) else 25)

  let rec rectif tl sp =
    Robot.event color is_green (fun _ -> go_straight());
    Robot.event color is_black (fun _ -> look_left());
    Robot.event_is idle (fun _ -> rectif (tl*2) (-sp));
    speed motor_left ~tach_limit:tl (-sp);
    speed motor_right ~tach_limit:tl sp

  and turn tl sp =
    Robot.event_is idle go_straight;
    speed motor_left ~tach_limit:tl (-sp);
    speed motor_right ~tach_limit:tl sp

  and go_straightBeforeTurn90 _ =
    let sp = if Random.bool () then 15 else -15 in
    Robot.event color is_white (fun _ -> rectif 40 sp);
    Robot.event_is idle (fun _ -> turn 200 15);
    speed motor_left ~tach_limit:120 25;
    speed motor_right ~tach_limit:120 25

  and go_straight () =
    let sp = if Random.bool() then 15 else -15 in
    Robot.event color is_white (fun _ -> rectif 40 sp);
    Robot.event color is_black (fun _ -> look_left());
    speed motor_left 30;
    speed motor_right 30

  and look_left () =
    Robot.event ultra (fun dist -> dist > 40)
      (fun _ -> reset_pos_ultra true (fun _ -> turn 200 15));
    Robot.event_is idle_ultra (fun _ -> reset_pos_ultra true look_front);
    speed motor_left 0;
    speed motor_right 0;
    speed motor_ultrasonic ~tach_limit:90 25

  and look_front () =
    Robot.event ultra (fun d -> d > 40) (fun _ ->
                                          reset_pos_ultra false go_straight);
    Robot.event_is idle_ultra (fun _ -> reset_pos_ultra false look_right)
   (* speed motor_ultrasonic ~tach_limit:120 (-15)*)

  and look_right () =
    Robot.event ultra (fun d -> d > 40)
      (fun _ -> reset_pos_ultra false (fun _ -> turn 200 (-15)));
    Robot.event_is idle_ultra
      (fun _ -> reset_pos_ultra false (fun _ -> turn 400 15));
    speed motor_ultrasonic (-25) ~tach_limit:90

  let run() =
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
