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
  let color = Robot.light C.conn light_port r

  let motor m =
    Robot.meas r (fun () -> let state,_,_,_ = Motor.get C.conn m in state)
  let motor_ultra = motor motor_ultrasonic
  let wheel = motor motor_left
  let is_idle state = state.Motor.run_state = `Idle

  (* Handful shortcut *)
  let speed ?tach_limit ?sync m s =
    Motor.set C.conn m (Motor.speed s ?tach_limit ?sync)

  (* the robot Move its "head" left and right *)
  let say_hello k =
    speed motor_ultrasonic 30 ~tach_limit:140;
    Robot.event motor_ultra is_idle begin fun _ ->
      speed motor_ultrasonic (-30) ~tach_limit:280;
      Robot.event motor_ultra is_idle begin fun _ ->
        speed motor_ultrasonic 30 ~tach_limit:140;
        Robot.event motor_ultra is_idle k
      end
    end

  (* Simple histogram *)
  let rec hist = function
    | [] -> []
    | v :: tl ->
        let h = hist tl in
        let found = ref false in
        let h' = List.map (fun ((k,n) as kn) ->
                             if k = v then (found := true; (k,n+1))
                             else kn) h in
        let h' = if !found then h' else (v,1) :: h in
        List.sort (fun (k,_) (k',_) -> compare k k') h'

  (* Move the robot left and right, take several measures, and
     calibrate according to the light/dark colors detected. *)
  let rec calibrate k =
    let colors = ref [] in
    speed motor_left 30 ~tach_limit:60;
    speed motor_right (-30) ~tach_limit:60;
    Robot.event_is always (fun _ -> colors := Robot.read color :: !colors);
    Robot.event wheel is_idle (fun _ -> calibrate2 colors k)
  and calibrate2 colors k =
    speed motor_left (-10) ~tach_limit:120;
    speed motor_right 10 ~tach_limit:120;
    Robot.event_is always (fun _ -> colors := Robot.read color :: !colors);
    Robot.event wheel is_idle (fun _ -> calibrate3 colors k)
  and calibrate3 colors k =
    List.iter (fun (v,n) -> eprintf "(%i,%i) " v n) (hist !colors);
    eprintf "\n";
    let min_max (mi, mx) v = (min mi v, max mx v) in
    let cmin, cmax = List.fold_left min_max  (max_int, min_int) !colors in
    let d = (cmax - cmin) / 3 in
    let cdark = cmin + d
    and clight = cmax - d in
    let on_road v = v <= cdark
    and off_road v = v >= clight in
    (* Position the robot back on the road *)
    speed motor_left 10;
    speed motor_right (-10);
    Robot.event color on_road begin fun _ ->
      speed Motor.all 0;
      k on_road off_road
    end


  let run () =
    calibrate begin fun on_road off_road ->
      raise Exit
    end;
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

