(* File: rubik_solver.ml *)

open Mindstorm

(** We parametrise the solver by the connections to the ports (so as to
    change them easily) *)
module Make(C: sig
              val conn : Mindstorm.bluetooth Mindstorm.conn
              val motor_fighter : Motor.port
              val motor_hand : Motor.port
              val motor_pf : Motor.port
              val push_hand_port : Sensor.port
              val push_fighter_port : Sensor.port
            end) =

struct
  open C
  open Printf


  let stop () =
    Motor.set conn Motor.all (Motor.speed 0);
    close conn

  let r = Robot.make()

  let run_loop () = Robot.run r


  let cube_is_held = ref true
    (** To know if the cube is held or not by the hand. *)

  let cog_is_left = ref true
    (** To know if cogs are placed to turn left or right.
        This variable is used because there is a large space between the teeth 
        of cogs (we lose about 30 degrees when we chande the way of the 
        platform). *)

  let speed motor ?tach_limit sp =
    Motor.set conn motor (Motor.speed ?tach_limit (sp))

  let get_tc_pf ()= let (x,_,_,_) = Motor.get conn motor_pf in x

  let idle_hand =
    Robot.meas r (fun () ->
                    let (state,_,_,_) = Motor.get conn motor_hand in
                    state.Motor.run_state = `Idle)

  let idle_fighter =
    Robot.meas r (fun () ->
                    let (state,_,_,_) = Motor.get conn motor_fighter in
                    state.Motor.run_state = `Idle)
  let idle_pf =
    Robot.meas r (fun () ->
                    let (state,_,_,_) = Motor.get C.conn motor_pf in
                    state.Motor.run_state = `Idle)

  let running_pf =
    Robot.meas r (fun () ->
                    let (state,_,_,_) = Motor.get C.conn motor_pf in
                    state.Motor.run_state = `Running)

  let hand_push = Robot.touch C.conn push_hand_port r
  let fighter_push = Robot.touch C.conn push_fighter_port r

  (** Give an appropriate position to the cogs to turn left if the boolean 
      bool is true or to turn rigth otherwise. *)
  let set_cog bool k =
    if(bool = true) then(
      if !cog_is_left then k()
      else (Robot.event_is running_pf (fun _  -> k());
            cog_is_left := true ;
            speed motor_pf (-1)))
    else(
      if  !cog_is_left then (Robot.event_is running_pf (fun _  -> k());
                             cog_is_left := false;
                             speed motor_pf 1)
      else k())

  (** Move the hand to hold the cube *)
  let hold_rubik k =
    if !cube_is_held then k()
    else (Robot.event_is idle_hand (fun _  -> k());
          cube_is_held := true;
          speed motor_hand ~tach_limit:110 (-40))

  (** Move the hand to free the cube *)
  let free_rubik k =
    if !cube_is_held then (Robot.event_is hand_push
                             (fun _ -> (speed motor_hand 0;
                                        k()));
                           cube_is_held := false;
                           speed motor_hand (15))
    else k()

  (** Initialize the position of the fighther after having kicked the cube*)
  let init_fighter k =
    Robot.event_is fighter_push (fun _ -> (speed motor_fighter 0;
                                          k()));
    speed motor_fighter (-14)

  let kick k =
    hold_rubik (fun _ -> (Robot.event_is idle_fighter (fun _ -> init_fighter k);
                          speed motor_fighter ~tach_limit:90 100))

  (** Turn the platform slowly to adjust it with precision *)
  let rec turn_pf_17 tl v k =
    Robot.event_is idle_pf (fun _ -> (speed motor_pf 0;
                                       k()));
    speed motor_pf ~tach_limit:tl v

  let turn_pf qt k =
    let turn () = free_rubik (
      fun _  -> (let tl100 = qt*(-300) in
                 let tl17 = qt*(-331) in
                 if qt>0 then(
                   Robot.event_is idle_pf(fun _ -> turn_pf_17 (-tl17) (-17) k);
                   speed motor_pf ~tach_limit:(-tl100) (-100))
                 else(
                   Robot.event_is idle_pf(fun _ -> turn_pf_17 tl17 17 k);
                   speed motor_pf ~tach_limit:tl100 100)))in
    if qt>0 then set_cog true turn
    else set_cog false turn

  (** Rectify the platform after having turned the cube because there is a
      small error caused by a space between the cube and the hand *)
  let rectif_pf bool tl10 v10 k =
    set_cog (not bool) (fun _ -> (Robot.event_is idle_pf (fun _  -> k());
                                   speed motor_pf ~tach_limit:tl10 v10))

  (** Turn the platform slowly (the cube is held) to have a good precision *)
  let turn_rubik_30 bool tl30 tl10 v30 v10 k =
    set_cog bool (fun _ -> (Robot.event_is idle_pf
                              (fun _ -> rectif_pf bool tl10 v10 k);
                            speed motor_pf ~tach_limit:tl30 v30))

  (** Turn the platform (the cube is held) *)
  let turn_rubik bool tl100 tl30 tl10 v100 v30 v10 k =
    hold_rubik(fun _ -> Robot.event_is idle_pf
                 (fun _ -> turn_rubik_30 bool tl30 tl10 v30 v10 k);
                 speed motor_pf ~tach_limit:tl100 v100)

  let turn_rubik_left k =
    set_cog true (fun _ -> turn_rubik true 450 281 100 (-70) (-20) (10) k)
  let turn_rubik_right k =
    set_cog false (fun _ -> turn_rubik false 450 171 45 (70) (20) (-10) k)
  let turn_rubik_half k =
    set_cog true (fun _ -> turn_rubik true 900 462 100 (-70) (-20) (10) k)

end
