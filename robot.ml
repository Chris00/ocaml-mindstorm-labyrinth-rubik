(* File: robot.ml

   Copyright (C) 2008

     Christophe Troestler <chris_77@users.sourceforge.net>
     WWW: http://math.umh.ac.be/an/software/

     Julie de Pril <julie_87@users.sourceforge.net>
     Dany Maslowski <dan_86@users.sourceforge.net>
     Marc Ducobu <el_marcu@users.sourceforge.net>

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.1 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

type meas_common = {
  robot : t; (* robot to which the measure is associated *)
  get_value : unit -> unit; (* how to get a new value *)
  mutable is_needed : bool; (* iff is bound to >= 1 event (for the
                               current loop).  if [true], the value
                               must be fetched by the event loop. *)
  mutable is_up_to_date : bool; (* updated by the latest event loop
                                   reading or by "read". *)
  is_constant : bool; (* the measure is constant ; thus also executing a
                         callback bound to it does NOT erase events. *)
}
and t = {
  mutable meas : meas_common list;
  (* Measure functions (that need updating) associated to the robot. *)
  mutable events : (unit -> bool) list;
  (* conditions-callbacks to execute (see [event] below). *)
  mutable at_exit : (unit -> unit) list;
}

(* The value has to be cached in [meas] because the robot (of type
   [t]) needs a uniform access to all data fetching. *)
type 'a meas = {
  common : meas_common;
  mutable value : 'a option; (* The fetched value, if any *)
}


let make () = { meas = [];  events = [];  at_exit = [] }

let stop r =
  raise Exit (* quit the event loop and turn off sensors -- see [run]. *)

let remove_events r =
  List.iter (fun m -> m.is_needed <- false) r.meas;
  r.events <- []


let meas r get =
  let rec meas = { value = None; (* no value yet *)
                   common = meas_common }
  and meas_common = { robot = r;
                      get_value = get_value;
                      is_needed = false;
                      is_up_to_date = false;
                      is_constant = false;
                    }
  and get_value() =
    meas.value <- Some(get());
    meas_common.is_up_to_date <- true in
  r.meas <- meas_common :: r.meas;
  meas

let touch conn port r =
  Mindstorm.Sensor.set conn port `Switch `Bool; (* Transition_cnt?? *)
  meas r (fun () -> (Mindstorm.Sensor.get conn port).Mindstorm.Sensor.scaled = 1)

let touch_count conn port ?(transition=false) r =
  let mode = if transition then `Transition_cnt else `Period_counter in
  Mindstorm.Sensor.set conn port `Switch mode;
  meas r (fun () -> (Mindstorm.Sensor.get conn port).Mindstorm.Sensor.scaled)

let light conn port ?(on=true) r =
  let ty = if on then `Light_active else `Light_inactive in
  Mindstorm.Sensor.set conn port ty `Pct_full_scale;
  let turn_off() = Mindstorm.Sensor.set conn port `No_sensor `Raw in
  r.at_exit <- turn_off :: r.at_exit;
  meas r (fun () -> (Mindstorm.Sensor.get conn port).Mindstorm.Sensor.scaled)

let sound conn port ?(human=false) r =
  let ty = if human then `Sound_dba else `Sound_db in
  Mindstorm.Sensor.set conn port ty `Pct_full_scale;
  meas r (fun () -> (Mindstorm.Sensor.get conn port).Mindstorm.Sensor.scaled)

let ultrasonic conn port r =
  let u = Mindstorm.Sensor.Ultrasonic.make conn port in
  Mindstorm.Sensor.Ultrasonic.set u `Meas_cont;
  let turn_off() = Mindstorm.Sensor.Ultrasonic.set u `Off in
  r.at_exit <- turn_off :: r.at_exit;
  meas r (fun () -> Mindstorm.Sensor.Ultrasonic.get u `Byte0)


(* This measure always returns [true]. *)
let always r =
  { value = Some true;
    common = { robot = r;
               get_value = (fun () -> ());
               is_needed = false; (* does not matter *)
               is_up_to_date = true; (* => do not add it to [r.meas] *)
               is_constant = true;
             }
  }

let read m =
  let c = m.common in
  if not c.is_up_to_date then c.get_value(); (* => up to date *)
  match m.value with Some v -> v | None -> assert false
;;

let event meas cond f =
  let c = meas.common in
  c.is_needed <- true;
  (* [exec] returns [true] if the condition succeeded and executes the
     associated callback.  It returns [false] if the contition failed. *)
  let exec =
    if c.is_constant then
      (* Fetch the value once only and do not erase other events --
         thus return [false] to allow the execution of subsequent events. *)
      let v = match meas.value with Some v -> v | None -> assert false in
      (fun () -> f v; false)
    else
      (fun () ->
         (* Fetch the value once, so can be updated (by a different
            thread) without harm during the exec of this callback. *)
         let v = match meas.value with Some v -> v | None -> assert false in
         if cond v then (
           remove_events c.robot;
           f v;
           true)
         else false)
  in
  c.robot.events <- exec :: c.robot.events
;;

let event_is m f = event m (fun b -> b) (fun _ -> f())


let rec exec_first = function
  | [] -> ()
  | e :: tl -> if not(e()) then exec_first tl

let run r =
  try
    while true do
      List.iter (fun m ->
                   if m.is_needed then m.get_value() (* => up to date *)
                   else m.is_up_to_date <- false
                ) r.meas;
      if r.events = [] then
        failwith "Robot.run: no events (this would loop indefinitely)";
      exec_first(List.rev r.events)

    done
  with e ->
    (* Turn off sensors we know about (whenever possible). *)
    List.iter (fun f -> try f() with _ -> ()) r.at_exit;
    match e with
    | Exit -> () (* considered as an acceptable way to stop. *)
    | Unix.Unix_error(Unix.ENOTCONN, _, _)
    | Unix.Unix_error(Unix.ECONNRESET, _, _) -> failwith "Robot disconnected"
    | Failure _ as e -> raise e
    | e ->
        Printf.eprintf "Uncaught exception: %s\n%!" (Printexc.to_string e)



(* Local Variables: *)
(* compile-command: "make -k robot.cmo" *)
(* End: *)
