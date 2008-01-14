open Printf
module Sensor = Mindstorm.Sensor
module Motor = Mindstorm.Motor

module Run(C: sig val conn : Mindstorm.bluetooth Mindstorm.conn end) =
struct
  (* Le robot va tout droit; s'il touche un obstacle, il s'arrête. *)

  (* Initialisation, at functor instantiation *)
  let () =
    Sensor.set C.conn `S1 `Switch `Bool;
  ;;

  let r = Robot.make()
  let touch = Robot.meas r (fun _ -> (Sensor.get C.conn `S1).Sensor.scaled = 1)


  let rec stop _ =
    Motor.set C.conn Motor.all (Motor.speed 0)

  and go_straight() =
    Robot.event touch (fun a -> a) stop;
    Motor.set C.conn Motor.b (Motor.speed 30);
    Motor.set C.conn Motor.c (Motor.speed 30)

  let run() =
    go_straight();
    Robot.run r
end

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
