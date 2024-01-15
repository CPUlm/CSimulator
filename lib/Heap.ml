(*
 * Heap -- binomial heaps
 * Copyright (C) 2011  Batteries Included Development Team
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(** The result of {!Make} *)
module type S = sig
  (** Type of elements of the heap *)
  type elm

  (** Type of the heap *)
  type t

  val empty : t
  (** The empty heap. *)

  val size : t -> int
  (** Number of elements in the heap. O(1) *)

  val add : elm -> t -> t
  (** [add x h] is the same as [insert h x]. This function is intended
    to be used with [fold_right]. O(log m) *)

  val find_min : t -> elm
  (** Find the minimal element of the heap. O(1)
    @raise Invalid_argument ["find_min"] if the heap is empty *)

  val del_min : t -> t
  (** Delete the minimal element of the heap. O(log n)
    @raise Invalid_argument ["del_min"] if the heap is empty *)

  val of_seq : elm Seq.t -> t
  (** Build a heap from a given sequence. O(n log n) *)
end

module Make (Ord : Map.OrderedType) = struct
  module Set = Set.Make (Ord)

  type elm = Ord.t

  let ord_min x y = if Ord.compare x y <= 0 then x else y

  type bt = {rank: int; root: Ord.t; kids: bt list}

  type t = {size: int; data: bt list; mind: Ord.t option}

  let empty = {size= 0; data= []; mind= None}

  let size bh = bh.size

  let link bt1 bt2 =
    assert (bt1.rank = bt2.rank) ;
    let rank = bt1.rank + 1 in
    let leq = Ord.compare bt1.root bt2.root <= 0 in
    let root = if leq then bt1.root else bt2.root in
    let kids = if leq then bt2 :: bt1.kids else bt1 :: bt2.kids in
    {rank; root; kids}

  let rec add_tree t = function
    | [] ->
        [t]
    | ut :: uts as ts ->
        assert (t.rank <= ut.rank) ;
        if t.rank < ut.rank then t :: ts else add_tree (link t ut) uts

  let insert bh x =
    let data = add_tree {rank= 0; root= x; kids= []} bh.data in
    let mind =
      match bh.mind with None -> Some x | Some mind -> Some (ord_min x mind)
    in
    {size= bh.size + 1; data; mind}

  let add x bh = insert bh x

  let rec merge_data ts1 ts2 =
    match (ts1, ts2) with
    | _, [] ->
        ts1
    | [], _ ->
        ts2
    | t1 :: tss1, t2 :: tss2 ->
        if t1.rank < t2.rank then t1 :: merge_data tss1 ts2
        else if t1.rank > t2.rank then t2 :: merge_data ts1 tss2
        else add_tree (link t1 t2) (merge_data tss1 tss2)

  let find_min bh =
    match bh.mind with None -> invalid_arg "find_min" | Some d -> d

  let rec find_min_tree ts ~kfail ~ksuccess =
    match ts with
    | [] ->
        kfail ()
    | [t] ->
        ksuccess t
    | t :: ts ->
        find_min_tree ts ~kfail ~ksuccess:(fun u ->
            if Ord.compare t.root u.root <= 0 then ksuccess t else ksuccess u )

  let rec del_min_tree bts ~kfail ~ksuccess =
    match bts with
    | [] ->
        kfail ()
    | [t] ->
        ksuccess t []
    | t :: ts ->
        del_min_tree ts ~kfail ~ksuccess:(fun u uts ->
            if Ord.compare t.root u.root <= 0 then ksuccess t ts
            else ksuccess u (t :: uts) )

  let del_min bh =
    let kfail () = invalid_arg "del_min" in
    del_min_tree bh.data ~kfail ~ksuccess:(fun bt data ->
        let size = bh.size - 1 in
        let data = merge_data (List.rev bt.kids) data in
        let mind =
          if size = 0 then None
          else Some (find_min_tree data ~kfail ~ksuccess:(fun t -> t)).root
        in
        {size; data; mind} )

  let of_seq l = Seq.fold_left insert empty l
end
