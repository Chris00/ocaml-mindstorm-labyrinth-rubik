(* File: solver.ml *)

open Rubik

let (mul,prun) = Phase1.initialize()

module PermAndSeq =
  struct
    type t = Phase1.t * Move.t list * int * int
        (* (permutation,sequence of moves,entry number,depth of the permutation
        in the tree of all possibilities) *)

    let compare (p,_,pentry,pdepth) (q,_,qentry,qdepth) =
      let fp = pdepth + (prun p) in
      let fq = qdepth + (prun q) in
      if fp < fq then -1
      else
        if fp = fq then
          if pentry < qentry then -1 else if pentry = qentry then 0 else 1
          else 1
  end

module S = Set.Make(PermAndSeq)

let search_seq_to_goal first is_in_goal =
  let no_seq() = exit 2 in
  let get_children p =
    let children = ref [] in
    let moves = Move.get() in
    for i = 0 to (Array.length moves)-1 do
      children := (mul p moves.(i),moves.(i))::!children
    done; !children in
  let rec aStar x opened closed nentry depth =
    let (p,seq,_,_) = x in
    if S.is_empty !opened then no_seq()
    else if is_in_goal p then seq
    else begin
      opened := S.remove x !opened;
      closed := S.add x !closed;
      let children = get_children p in
      let (newopened,newentry) = List.fold_left
        begin fun (opened,n) (child,m) ->
          (S.add(child,m::seq,n+1,depth+1) opened,n+1) end
        (!opened,nentry) children
      in
      aStar (S.min_elt newopened) (ref newopened) closed newentry (depth+1)
    end
  in let first = (first,[],1,1) in
     aStar first (ref(S.singleton first)) (ref(S.empty)) 1 1

let seq = search_seq_to_goal
  (mul (Phase1.of_cube Cubie.id) (Move.make(F,3))) Phase1.in_G1


