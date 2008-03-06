(* File: ida.ml

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

(** Iterative deepening A* algorithm. *)


module Make(C :
  sig
    include Rubik.Coordinate
    val max_moves : int
    val in_goal : t -> bool
  end) =
struct

  let mul = C.initialize_mul()
  let pruning = C.initialize_pruning mul

  (* Return a list of possible paths (added to the already existing
     solutions [sols]). *)
  let rec add_solution sols cost_max  perm last_move path depth =
    if C.in_goal perm then path :: sols else begin
      let depth = depth + 1 in
      List.fold_left begin fun sols m ->
        if C.Move.have_same_gen m last_move then sols (* skip the move *)
        else
          let perm = mul perm m in
          let cost = depth + pruning perm in
          if cost > cost_max then sols
          else add_solution sols cost_max  perm last_move path depth
      end sols C.Move.all
    end

  let search_seq_to_goal init cost_max =
    if C.in_goal init then [[]]           (* [] : no move needed *)
    else List.fold_left begin fun sols m ->
      add_solution sols cost_max (mul init m) m [m] 1
    end [] C.Move.all
end



(* Local Variables: *)
(* compile-command: "make -k ida.cmo" *)
(* End: *)
