type t

val fresh : unit -> t

module Map : Map.S with type key = t

module Set : Set.S with type elt = t

type 'a map = 'a Map.t

type set = Set.t

val pp : Format.formatter -> t -> unit

val compare : t -> t -> int
