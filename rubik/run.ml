(* File: run.ml

   Copyright (C) 2008

     Christophe Troestler <chris_77@users.sourceforge.net>
     WWW: http://math.umh.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.1 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)


open Rubik

let () =
  let mul1 = Phase1.initialize_mul() in
  let prun1 = Phase1.initialize_pruning mul1 in
  let mul2 = Phase2.initialize_mul() in
  let prun2 = Phase2.initialize_pruning mul2 in

  Gc.full_major();
  Gc.print_stat stdout;
  flush stdout;
  Unix.sleep 10

