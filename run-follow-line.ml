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
  (* The robot tries to follow a dark line *)

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

  let r = Robot.make()

  (* Measures *)
  let always = Robot.always r

  let color =
    Robot.meas r (fun () -> (Sensor.get C.conn light_port).Sensor.scaled)

  let is_idle motor =
    Robot.meas r (fun () ->
                 let (state,_,_,_) = Motor.get C.conn motor in
                 state.Motor.run_state = `Idle)
  let idle_ultra = is_idle motor_ultrasonic
  let idle = is_idle motor_left

  (* Handful shortcut *)
  let speed ?tach_limit ?sync m s =
    Motor.set C.conn m (Motor.speed s ?tach_limit ?sync)

  (* the robot Move its "head" left and right *)
  let say_hello k =
    speed motor_ultrasonic 30 ~tach_limit:140;
    Robot.event_is idle_ultra begin fun _ ->
      speed motor_ultrasonic (-30) ~tach_limit:280;
      Robot.event_is idle_ultra begin fun _ ->
        speed motor_ultrasonic 30 ~tach_limit:140;
        Robot.event_is idle_ultra k
      end
    end

  (* Move the robot left and right, take several measures, and
     calibrate according to the light/dark colors detected. *)
  let rec calibrate k =
    let colors = ref [] in
    speed motor_left 20 ~tach_limit:60;
    speed motor_right (-20) ~tach_limit:60;
    Robot.event_is always (fun _ -> colors := Robot.read color :: !colors);
    Robot.event_is idle (fun _ -> calibrate2 colors k)
  and calibrate2 colors k =
    speed motor_left (-20) ~tach_limit:120;
    speed motor_right 20 ~tach_limit:120;
    Robot.event_is always (fun _ -> colors := Robot.read color :: !colors);
    Robot.event_is idle (fun _ -> k colors)

  let run () =
    calibrate (fun _ -> ());
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

