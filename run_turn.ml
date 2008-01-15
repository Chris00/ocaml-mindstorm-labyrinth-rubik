open Printf
module Sensor = Mindstorm.Sensor
module Motor = Mindstorm.Motor

module Run(C: sig val conn : Mindstorm.bluetooth Mindstorm.conn end) =
struct
  (* Le robot roule et s'il voit un mur, il ralentit, et il tourne.
     S1 : ultrasonic sensor
     B : left motor
     C : right motor
  *)

  let r = Robot.make()

  (* Initialize and create a measure for the ultrasonic sensor. *)
  let ultra =
    let u = Sensor.Ultrasonic.make C.conn `S1 in
    Sensor.Ultrasonic.set u `Meas_cont;
    Robot.meas r (fun _ -> Mindstorm.Sensor.Ultrasonic.get u `Byte0)

  let rec turn _ =
    Robot.event ultra (fun a -> a > 50) go_straight;
    Motor.set C.conn Motor.b (Motor.speed (-30));
    Motor.set C.conn Motor.c (Motor.speed 30)

  and go_straight s =
    Robot.event ultra (fun a -> a < 40) turn;
    Robot.event ultra (fun a -> a < 70) go_straight;
    Motor.set C.conn Motor.b (Motor.speed ((-s)/2));
    Motor.set C.conn Motor.c (Motor.speed ((-s)/2))

  let run() =
    go_straight 90;
    Robot.run r
end;;

let () =
  let bt =
    if Array.length Sys.argv < 2 then (
      printf "%s <bluetooth addr>\n" Sys.argv.(0);
      exit 1;
    )
    else Sys.argv.(1) in
  let module R = Run(struct
                       let conn = Mindstorm.connect_bluetooth bt
                     end) in
  printf "Press the button on the robot to stop.\n%!";
  R.run()
