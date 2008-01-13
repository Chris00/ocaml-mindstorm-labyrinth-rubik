open Printf
open Mindstorm
module Sensor = Mindstorm.Sensor
module Motor = Mindstorm.Motor
open Robot

(* Le robot va tout droit; s'il touche un obstacle, il s'arrête. *)
class run_push conn =
  let () = Sensor.set conn `S1 `Switch `Bool in
  let sensorTouch port conn =
    let switch = Sensor.get conn port in
    switch.Sensor.scaled in
  let st1 = (sensorTouch `S1) in
  let sta = (fun conn -> 0) in
object (self)
  inherit [int, int, int, int] event_loop st1  sta sta sta conn
    as event_loop (*argument en option?*)

  method stop _ =
    Motor.set conn Motor.b (Motor.speed 0);
    Motor.set conn Motor.c (Motor.speed 0)

  method go_straight =
  event_loop#addS1 (fun a -> if(a=1) then true else false) self#stop;
    Motor.set conn Motor.b (Motor.speed 30);
    Motor.set conn Motor.c (Motor.speed 30)

  (* @override *)
  method run() =
    self#go_straight;
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
  let rp = new run_push conn in
  printf "Press the button on the robot to stop.\n%!";
  rp#run()
