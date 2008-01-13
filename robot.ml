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

type conn = Mindstorm.bluetooth Mindstorm.conn

(* The first argument of each constructor is a condition and the
   second one is a callback *)
type ('a1, 'a2, 'a3, 'a4) cond =
  | S1 of ('a1 -> bool) * ('a1 -> unit)
  | S2 of ('a2 -> bool) * ('a2 -> unit)
  | S3 of ('a3 -> bool) * ('a3 -> unit)
  | S4 of ('a4 -> bool) * ('a4 -> unit)

let some = function Some v -> v | None -> assert false


class ['a1, 'a2, 'a3, 'a4] event_loop
  (cs1: conn -> 'a1) (cs2: conn -> 'a2) (cs3: conn -> 'a3) (cs4: conn -> 'a4)
  conn =
object(self)
  val conn = conn
  val mutable check_s1 = false (* S1 in [ccb] *)
  val mutable check_s2 = false
  val mutable check_s3 = false
  val mutable check_s4 = false
  val mutable ccb = ([] : ('a1, 'a2, 'a3, 'a4) cond list)

  method addS1 cond cb = ccb <- S1(cond,cb) :: ccb; check_s1 <- true
  method addS2 cond cb = ccb <- S2(cond,cb) :: ccb; check_s2 <- true
  method addS3 cond cb = ccb <- S3(cond,cb) :: ccb; check_s3 <- true
  method addS4 cond cb = ccb <- S4(cond,cb) :: ccb; check_s4 <- true

  method private reset =
    check_s1 <- false; check_s2 <- false;
    check_s3 <- false; check_s4 <- false;
    ccb <- []

  method run() =
    let rec exec_cond v1 v2 v3 v4 l = match l with
      | [] -> () (* no condition true *)
      | S1(c,f) :: tl ->
          let v = some v1 in
          if c v then (self#reset; f v) else exec_cond v1 v2 v3 v4 tl
      | S2(c,f) :: tl ->
          let v = some v2 in
          if c v then (self#reset; f v) else exec_cond v1 v2 v3 v4 tl
      | S3(c,f) :: tl ->
          let v = some v3 in
          if c v then (self#reset; f v) else exec_cond v1 v2 v3 v4 tl
      | S4(c,f) :: tl ->
          let v = some v4 in
          if c v then (self#reset; f v) else exec_cond v1 v2 v3 v4 tl
    in
    while true do
      if ccb = [] then failwith "No conditions, would loop indefinitely";
      let v1 = if check_s1 then Some(cs1 conn) else None
      and v2 = if check_s2 then Some(cs2 conn) else None
      and v3 = if check_s3 then Some(cs3 conn) else None
      and v4 = if check_s4 then Some(cs4 conn) else None in
      exec_cond v1 v2 v3 v4 (List.rev ccb)
    done

end

