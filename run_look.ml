open Printf
module Sensor = Mindstorm.Sensor
module Motor = Mindstorm.Motor

module Run(C: sig val conn : Mindstorm.bluetooth Mindstorm.conn end) =
struct
  (* Le robot roule en suivant une ligne verte au sol (sur fond blanc).
     S'il voit de la couleur noire, il regarde a droite et a gauche s'il y a
     des chemins possibles.
     S1 : ultrasonic sensor
     S2 : light sensor
     A : motor for ultrasonic sensor
     B : left motor
     C : right motor*)

  (* Initialisation, at functor instantiation *)
  let () =
    Sensor.set C.conn `S2 `Light_active `Pct_full_scale;
    let stop _ =
      Motor.set C.conn Motor.all (Motor.speed 0);
      Mindstorm.close C.conn;
      printf "\n";
      exit 0 in
    Sys.set_signal Sys.sigint (Sys.Signal_handle stop);
    printf "Press Ctrl-c to quit.\n%!"
  ;;

  let speed motor ?tach_limit sp =
    Motor.set C.conn motor (Motor.speed ?tach_limit (-sp))

  let r = Robot.make()
  let light = Robot.meas r (fun () -> (Sensor.get C.conn `S2).Sensor.scaled)
  let idle = Robot.meas r (fun () ->
                             let (state,_,_,_) = Motor.get C.conn Motor.b in
                             state.Motor.run_state = `Idle)
  let idleLight = Robot.meas r (fun () ->
                             let (state,_,_,_) = Motor.get C.conn Motor.a in
                             state.Motor.run_state = `Idle)
  let ultra =
    let u = Sensor.Ultrasonic.make C.conn `S1 in
    Sensor.Ultrasonic.set u `Meas_cont;
    Robot.meas r (fun _ -> Mindstorm.Sensor.Ultrasonic.get u `Byte0)

  let black a = a < 28
  let green a = (a < 45 && a > 33)
  let white a = a > 45

  let resetPosUltra bool f =
    Robot.event idleLight (fun v -> v) (fun _ -> f);
    let (_,tc,_,_) = Motor.get C.conn Motor.a in
    let sp = if bool then (-25) else 25 in
    speed Motor.a ~tach_limit:tc sp

  let rec rectif tl sp =
    Robot.event light (fun a -> (green a)) (fun _ -> go_straight());
    Robot.event light (fun a -> (black a)) (fun _ -> lookLeft());
    Robot.event idle (fun v -> v) (fun _ -> rectif (tl*2) (-sp));
    speed Motor.b ~tach_limit:tl (-sp);
    speed Motor.c ~tach_limit:tl sp

  and turn tl sp =
    Robot.event idle (fun v -> v) (fun _ -> go_straight());
    speed Motor.b ~tach_limit:tl (-sp);
    speed Motor.c ~tach_limit:tl sp

  and go_straightBeforeTurn90 _ =
    let sp = if Random.bool () then 15 else -15 in
    Robot.event light (fun a -> (white a)) (fun _ -> rectif 40 sp);
    Robot.event idle (fun v -> v) (fun _ -> turn 200 15);
    speed Motor.b ~tach_limit:120 25;
    speed Motor.c ~tach_limit:120 25

  and go_straight () =
    let sp = if Random.bool() then 15 else -15 in
    Robot.event light (fun a -> (white a)) (fun _ -> rectif 40 sp);
    Robot.event light (fun a -> (black a)) (fun _ -> lookLeft());
    speed Motor.b 30;
    speed Motor.c 30

  and lookLeft () =
    Robot.event ultra (fun dist -> dist > 40)
      (fun _ -> resetPosUltra true (turn 200 15));
    Robot.event idleLight (fun v -> v)
      (fun _ -> resetPosUltra true (lookFront()));
    speed Motor.b 0;
    speed Motor.c 0;
    speed Motor.a ~tach_limit:90 25

  and lookFront () =
    Robot.event ultra (fun dist -> dist > 40)
      (fun _ -> resetPosUltra false (go_straight()));
    Robot.event idleLight (fun v -> v)
      (fun _ -> resetPosUltra false (lookRight()));
   (* speed Motor.a ~tach_limit:120 (-15)*)

  and lookRight () =
    Robot.event ultra (fun dist -> dist > 40)
      (fun _ -> resetPosUltra false (turn 200 (-15)));
    Robot.event idleLight (fun v -> v)
      (fun _ -> resetPosUltra false (turn 400 15));
    speed Motor.a ~tach_limit:90 (-25)

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
