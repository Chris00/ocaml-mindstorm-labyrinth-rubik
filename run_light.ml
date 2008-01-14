open Printf
module Sensor = Mindstorm.Sensor
module Motor = Mindstorm.Motor

module Run(C: sig val conn : Mindstorm.bluetooth Mindstorm.conn end) =
struct
  (* Le robot roule en suivant une ligne verte au sol (sur fond blanc).
     S'il voit de la couleur noire, il fait un quart de tour à droite
     et continue de suivre la ligne verte *)

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
    Motor.set C.conn motor (Motor.speed ?tach_limit sp)

  let r = Robot.make()
  let light = Robot.meas r (fun () -> (Sensor.get C.conn `S2).Sensor.scaled)
  let idle = Robot.meas r (fun () ->
                             let (state,_,_,_) = Motor.get C.conn Motor.b in
                             state.Motor.run_state = `Idle)

  let rec turn tl sp =
    Robot.event light (fun a -> a < 47 && a > 35) (fun _ -> go_straight());
    Robot.event idle (fun v -> v) (fun _ -> turn (tl*2) (-sp));
    speed Motor.b ~tach_limit:tl (-sp);
    speed Motor.c ~tach_limit:tl sp

  and turn45deg tl sp _ =
    Robot.event idle (fun v -> v) (fun _ -> go_straight());
    speed Motor.b ~tach_limit:tl (-sp);
    speed Motor.c ~tach_limit:tl sp

  and go_straight () =
    let sp = if Random.bool() then 15 else -15 in
    Robot.event light (fun a -> a > 50) (fun _ -> turn 40 sp);
    Robot.event light (fun a -> a < 35) go_straightBeforeTurn45;
    speed Motor.b (-25);
    speed Motor.c (-25)

  and go_straightBeforeTurn45 _ =
    let sp = if Random.bool () then 15 else -15 in
    Robot.event light (fun a -> a > 50) (fun _ -> turn 40 sp);
    Robot.event idle (fun v -> v) (turn45deg 200 15);
    speed Motor.b ~tach_limit:120 (-25);
    speed Motor.c ~tach_limit:120 (-25)

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
