(* File: solver.ml *)

(** Functor using a [Phase] module to develop some strategies to find a
    sequence of moves to get into a goal state. *)
module Make(P: sig
  type t
  type move = Rubik.Move.t
  val max_moves : int
  val initialize : ?file:string -> unit -> (t -> move -> t) * (t -> int)
end) =
struct

  open Rubik

  module PermAndSeq =
  struct
    type t = P.t * Move.t list * int * int * int
        (* (permutation,list of moves saying which moves we have done to get
           into this permutation,entry number,value of the path-cost function,
           value of the distance-to-the-goal function)

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

  module Q = Set.Make(PermAndSeq)

  let (mul,prun) = P.initialize()

  (** [search_seq_to_goal p in_goal] returns a list of moves that lead to the
      a permutation [q] (such that [in_goal q] is true) from the permutation
      [p]. *)
  let search_seq_to_goal first is_in_goal =
    (* No sequence of moves found, so exit with error *)
    let no_seq() = exit 2 in
    (* [get_children p] returns the "children" of the permutation [p] (ie all
       the permutations we can reach by applying a single move to [p]) with
       the associated move. *)
    let get_children p =
      let children = ref [] in
      let moves = Move.get() in
      for i = 0 to (Array.length moves)-1 do
        children := (mul p moves.(i),moves.(i))::!children
      done; !children in
    (* [aStar] recursively searches a sequence of moves to get into the goal
       state. (A* algorithm) *)
    let rec aStar x opened closed nentry  =
      (* x is the new candidate to consider, opened is the set of all
      possible candidates, closed is the set of all permuations already
      considered, nentry is the number of elements added to opened. *)
      let (p,seq,_,gp,_) = x in
      if Q.is_empty !opened (* No more candidates. *) then no_seq()
      else if is_in_goal p (* Get into the goal state! *) then seq
      else begin
        opened := Q.remove x !opened;(* We now consider x. *)
        closed := Q.add x !closed;
        let children = get_children p in
        (* We update the sets opened and closed with the children of [p]. *)
        let (newopened,newclosed,newentry) = List.fold_left
          begin fun acc (child,m) ->
            let (opened,closed,n) = acc in
            if gp+1 <= P.max_moves then (* The path is not too long. *)
              (* We look for [c] in opened and closed. *)
              (let in_opened = Q.filter (fun (q,_,_,_,_) -> q = child) opened in
              let in_closed = Q.filter (fun (q,_,_,_,_) -> q = child) closed in
              (* g(child) = g(x)+1 and h(child) = prun child. *)
              let c = (child,m::seq,n+1,gp+1,prun child) in
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
                  (* We replace [elt_in_opened] by [c] in [opened] because the
                     path-cost of [c] is less than the path-cost of
                     [elt_in_closed]. *)
                  (Q.add c ( Q.remove elt_in_opened opened),closed,n+1)
                else acc (* [opened] and [closed] don't change. *)
              else if Q.is_empty in_closed then
                (* [c] is not in [closed] or in [opened], so we add it to
                   [opened]. *)
                (Q.add c opened,closed,n+1)
              else acc) (* [opened] and [closed] don't change. *)
            else acc (* [opened] and [closed] don't change. *)
          end
          (!opened,!closed,nentry) children
        in
        (* We continue the recursion: the new candidate is the element in
           [opened] which has the lowest value for the distance-plus-cost
           function. *)
        aStar (Q.min_elt newopened) (ref newopened) (ref newclosed) newentry
      end
    in let first = (first,[],1,0,prun first) in
       aStar first (ref(Q.singleton first)) (ref(Q.empty)) 1

end

module Solver1 = Make(Rubik.Phase1)
open Rubik
let seq = Solver1.search_seq_to_goal
  (Solver1.mul (Phase1.of_cube Cubie.id) (Move.make(F,3))) Phase1.in_G1


