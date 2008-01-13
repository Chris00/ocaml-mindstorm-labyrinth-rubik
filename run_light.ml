open Printf
open Mindstorm
open Random
module Sensor = Mindstorm.Sensor
module Motor = Mindstorm.Motor
open Robot

(* Le robot roule en suivant une ligne verte au sol (sur fond blanc).
   S'il voit de la couleur noire, il fait un quart de tour à droite et continue
   de suivre la ligne verte *)
class run_light conn =
  let () = Mindstorm.Sensor.set conn `S2 `Light_active `Pct_full_scale in
  let sensorLight conn = (Sensor.get conn `S2).Sensor.scaled in
  let st1 = sensorLight in
  let st2 conn =
    let (state,_,_,_) = Motor.get conn Motor.b in
    if state.Motor.run_state = `Idle then 1 else 0 in
  let sta = (fun conn -> 0) in
object(self)
  inherit [int, int, int, int] event_loop st1  st2 sta sta conn
    as event_loop (*argument en option?*)

  method turn tl sp  _ =
    event_loop#addS1 (fun a -> a < 47 && a > 35) self#go_straight;
    event_loop#addS2 (fun v -> v = 1) (self#turn (tl*2) (-sp));
    Motor.set conn Motor.b (Motor.speed ~tach_limit:tl (-sp));
    Motor.set conn Motor.c (Motor.speed ~tach_limit:tl sp)

  method turn45deg tl sp  _ =
    event_loop#addS2 (fun v -> v = 1)  self#go_straight;
    Motor.set conn Motor.b (Motor.speed ~tach_limit:tl (-sp));
    Motor.set conn Motor.c (Motor.speed ~tach_limit:tl sp)

  method go_straight _ =
    let sp = if Random.bool() then 15 else -15 in
    event_loop#addS1 (fun a -> a>50) (self#turn 40 sp);
    event_loop#addS1 (fun a -> a<35) self#go_straightBeforeTurn45;
    Motor.set conn Motor.b (Motor.speed (-25));
    Motor.set conn Motor.c (Motor.speed (-25))

  method go_straightBeforeTurn45 _ =
    let sp = if Random.bool () then 15 else -15 in
    event_loop#addS1 (fun a -> a>50) (self#turn 40 sp);
    event_loop#addS2 (fun v -> v=1) (self#turn45deg 200 15);
    Motor.set conn Motor.b (Motor.speed ~tach_limit:120 (-25));
    Motor.set conn Motor.c (Motor.speed ~tach_limit:120 (-25))

  (* @override *)
  method run() =
    self#go_straight 1;
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
  let rp = new run_light conn in
  let stop _ =
    Motor.set conn Motor.b (Motor.speed 0);
    Motor.set conn Motor.c (Motor.speed 0);
    Mindstorm.close conn;
    printf "\n";
    exit 0 in
  Sys.set_signal Sys.sigint (Sys.Signal_handle stop);
  printf "Press Ctrl-c to quit.\n%!";
  rp#run()
