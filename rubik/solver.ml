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

  module PermAndSeq =
  struct
    type t = P.t * Move.t list * int * int * int
        (* (permutation, list of moves leading to this permutation,
           path-cost, lower bound of the distance-to-the-goal)

           Note that the distance-plus-cost fonction is the sum of the
           path-cost function and the distance-to-the-goal function.
        *)

    let compare (p,_,pentry,gp,hp) (q,_,qentry,gq,hq) =
      if p = q then 0
      else
        let fp = gp + hp in
        let fq = gq + hq in
        if fp < fq then -1
        else if fp = fq then
          (if pentry < qentry then -1 else if pentry = qentry then 0 else 1)
        else 1
          (* We order the elements according to their value for the
             distance-plus-cost function, and then according to the moment of
             their arrival in the set. *)
  end

  module PQ = Priority_queue
  module S = Set.Make(P)

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
    let rec aStar opened closed =
      (* [x] is the new candidate to consider, [opened] is the set of
         all pending candidates, [closed] is the set of all
         permuations already considered.  *)
      if PQ.is_empty opened (* No more candidates. *) then no_seq()
      else
        let (p, seq, gp, _) = PQ.take opened in
        if is_in_goal p (* Get into the goal state! *) then seq
        else begin
          let closed = S.add p closed in
          let children = get_children p in
          (* We update the sets opened and closed with the children of [p]. *)
          let (newopened, newclosed) =
            List.fold_left begin fun (opened,closed) (child,m) ->
              if gp+1 <= P.max_moves then ( (* The path is not too long. *)
                (* We look for [c] in opened and closed. *)
                let in_opened = Q.filter (fun (q,_,_,_,_) -> q = child) opened in
                let in_closed = Q.filter (fun (q,_,_,_,_) -> q = child) closed in
                (* g(child) = g(x)+1 and h(child) = prun child. *)
                let c = (child, m::seq, gp+1, prun child) in
                let (opened,closed) =
                  if not(Q.is_empty in_closed) then
                    (* [c] already is in [closed]. *)
                    let elt_in_closed = Q.choose in_closed in
                    let (_,_,_,ge,_) = elt_in_closed in
                    if gp+1 <= ge then
                      (* We re-open [c] because the path-cost of [c] is less
                         than the path-cost of [elt_in_closed]. *)
                      (Q.add c opened, Q.remove elt_in_closed closed)
                    else (opened,closed)
                  else (opened,closed)
                in
                if not(Q.is_empty in_opened) then
                  (* [c] already is in [opened]. *)
                  let elt_in_opened = Q.choose in_opened in
                  let (_,_,_,ge,_) = elt_in_opened in
                  if gp+1 <= ge then
                    (* We replace [elt_in_opened] by [c] in [opened]
                       because the path-cost of [c] is less than the
                       path-cost of [elt_in_closed]. *)
                    (Q.add c ( Q.remove elt_in_opened opened),closed,n+1)
                  else acc (* [opened] and [closed] don't change. *)
                else if Q.is_empty in_closed then
                  (* [c] is not in [closed] or in [opened], so we add it to
                     [opened]. *)
                  (Q.add c opened,closed,n+1)
                else acc) (* [opened] and [closed] don't change. *)
              else acc (* [opened] and [closed] don't change. *)
            end
              (opened,closed) children
          in
          (* We continue the recursion: the new candidate is the element in
             [opened] which has the lowest value for the distance-plus-cost
             function. *)
          aStar (Q.min_elt newopened) newopened newclosed
        end
    in
    let first = (first,[],0,prun first) in
    let opened = Q.make P.max_moves in
    Q.add first opened;
    aStar first opened Q.empty 1

end

module Solver1 = Make(Rubik.Phase1)
open Rubik
let seq = Solver1.search_seq_to_goal
  (Solver1.mul (Phase1.of_cube Cubie.id) (Move.make(F,3))) Phase1.in_G1


