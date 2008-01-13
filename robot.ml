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

class ['a1, 'a2, 'a3, 'a4] event_loop
  (cs1: conn -> 'a1) (cs2: conn -> 'a2) (cs3: conn -> 'a3) (cs4: conn -> 'a4)
  conn =
object(self)
  val mutable ccb = [] (* Liste de (cond,callback,checksensor)*)

  method addS1 cond cb = ccb <- List.append ccb [(cond,cb,cs1)]
  method addS2 cond cb = ccb <- List.append ccb [(cond,cb,cs2)]
  method addS3 cond cb = ccb <- List.append ccb [(cond,cb,cs3)]
  method addS4 cond cb = ccb <- List.append ccb [(cond,cb,cs4)]

  method run() =
    while true do
      try
        match ccb with
        |[] -> ();
        | _ ->
            let (cond,cb,cs) = List.find (fun (c,_,cs) -> c (cs conn)) ccb in
            let v = cs conn in
            if cond v then (ccb <- []; cb v)
      with _ -> () (* ignore errors *)
    done

end

