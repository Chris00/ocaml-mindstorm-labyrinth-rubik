open Printf
open Mindstorm
module Sensor = Mindstorm.Sensor
module Motor = Mindstorm.Motor
open Robot

(* Le robot roule et s'il voit un mur, il ralentit, et il tourne *)
class run_turn conn =
  let ultra = Sensor.Ultrasonic.make conn `S1 in
  let () = Sensor.Ultrasonic.set ultra `Meas_cont in
  let sensorUltra _ = Mindstorm.Sensor.Ultrasonic.get ultra `Byte0 in
  let st1 = sensorUltra in
  let sta = (fun conn -> 0) in
object (self)
  inherit [int, int, int, int] event_loop st1  sta sta sta conn
    as event_loop (*argument en option?*)

  method turn _ =
    event_loop#addS1 (fun a -> a > 50) self#go_straight;
    Motor.set conn Motor.b (Motor.speed (-30));
    Motor.set conn Motor.c (Motor.speed 30)

  method go_straight s =
    event_loop#addS1 (fun a -> a < 40) self#turn;
    event_loop#addS1 (fun a -> a < 70) self#go_straight;
    Motor.set conn Motor.b (Motor.speed (s/2));
    Motor.set conn Motor.c (Motor.speed (s/2))

  (* @override *)
  method run() =
    self#go_straight 90;
    event_loop#run()
end;;

let () =
  let bt =
    if Array.length Sys.argv < 2 then (
      printf "%s <bluetooth addr>\n" Sys.argv.(0);
      exit 1;
    )
    else Sys.argv.(1) in
  let conn = Mindstorm.connect_bluetooth bt in
  let rp = new run_turn conn in
  printf "Press the button on the robot to stop.\n%!";
  rp#run()
