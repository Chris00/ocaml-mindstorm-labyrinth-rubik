(* File: solver.ml

   Copyright (C) 2008

     Julie de Pril <julie_87@users.sourceforge.net>

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


(** Functor using a [Phase] module to develop some strategies to find a
    sequence of moves to get into a goal state. *)
module Make(P:
  sig
    type t
    val compare : t -> t -> int
    val max_moves : int
    val initialize : ?file:string -> unit -> (t -> Rubik.Move.t -> t) * (t -> int)
  end) =
struct

  open Rubik

  module PQ = Priority_queue

  let (mul,prun) = P.initialize()

  (** [search_seq_to_goal p in_goal] returns a list of moves that lead
      to the a permutation [q] (such that [in_goal q] is true) from
      the permutation [p]. *)
  let search_seq_to_goal first is_in_goal =
    (* No sequence of moves found, so exit with error *)
    let no_seq() = exit 2 in
    (* [get_children p] returns the "children" of the permutation [p] (ie all
       the permutations we can reach by applying a single move to [p]) with
       the associated move. *)
    let get_children p = List.map (fun move -> (mul p move, move)) Move.all in
    (* [aStar] recursively searches a sequence of moves to get into the goal
       state. (A* algorithm) *)
    let rec aStar opened =
      (* [opened] is the set of all pending candidates. *)
      if PQ.is_empty opened (* No more candidates. *) then no_seq()
      else
        let (p,seq,pcost) = PQ.take opened in
        if is_in_goal p (* Get into the goal state! *) then seq
        else begin
          let children = get_children p in
          (* We update the set [opened] with the children of [p]. *)
          let opened =
            List.fold_left begin fun opened (child,m) ->
              let fchild = pcost+1 + prun child in
              if fchild <= P.max_moves then (* The path is not too long. *)
                let c = (child, m::seq, pcost+1) in
                PQ.add fchild c opened;
                opened
              else opened
            end
              opened children
          in
          aStar opened
        end
    in
    let start = (first,[],0) in
    let opened = PQ.make (P.max_moves+1) in
    PQ.add (prun first) start opened;
    aStar opened

end

module Solver1 = Make(Rubik.Phase1)
open Rubik
let seq = Solver1.search_seq_to_goal
  (Solver1.mul (Phase1.of_cube Cubie.id) (Move.make(F,3))) Phase1.in_G1


