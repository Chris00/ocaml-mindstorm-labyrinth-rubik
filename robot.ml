(* File: robot.ml

   Copyright (C) 2008

     Christophe Troestler <Christophe.Troestler@umh.ac.be>
     WWW: http://math.umh.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.1 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

type t = {
  mutable get_meas : (bool ref * (unit -> unit)) list;
  (* The [bool ref] tels whether the value is needed, thus must be
     fetched.  This can change at each cycle. *)
  mutable events : (unit -> bool) list;
}

let make () = { get_meas = [];  events = [] }

let remove_events r =
  List.iter (fun (is_needed,_) -> is_needed := false) r.get_meas;
  r.events <- []


type 'a meas = {
  robot : t; (* robot to which the measure is associated *)
  mutable value : 'a option; (* The fetched value, if any *)
  is_bound : bool ref; (* iff >= 1 event is bound *)
}

let meas r get =
  let meas = { robot = r;
               value = None; (* no value yet *)
               is_bound = ref false } in
  let update() = meas.value <- Some(get()) in
  r.get_meas <- (meas.is_bound, update) :: r.get_meas;
  meas

let event meas cond f =
  meas.is_bound := true;
  (* [exec] returns [true] if the condition succeeded and executes the
     associated callback.  It returns [false] if the contition falied. *)
  let exec () =
    (* fetch then value once, so can be updated (by a different
       thread) without harm during the exec of this callback. *)
    let v = match meas.value with Some v -> v | None -> assert false  in
    if cond v then (
      remove_events meas.robot;
      f v;
      true)
    else false in
  meas.robot.events <- exec :: meas.robot.events
;;

let rec exec_first = function
  | [] -> ()
  | e :: tl -> if not(e()) then exec_first tl

let run r =
  while true do
    try
      List.iter (fun (need, get) -> if !need then get()) r.get_meas;
      if r.events = [] then
        failwith "Robot.run: no events (this would loop indefinitely)";
      exec_first(List.rev r.events)
    with _ -> ()
  done
    



(* Local Variables: *)
(* compile-command: "make -k robot.cmo" *)
(* End: *)
