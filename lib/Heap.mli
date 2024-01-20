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

(** Functorized heaps over arbitrary orderings. *)
module Make (Ord : Map.OrderedType) : S with type elm = Ord.t
