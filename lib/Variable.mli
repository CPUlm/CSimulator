type t

val fresh : string -> int -> t

val size : t -> int

val exists : string -> t option

module Map : Map.S with type key = t

module Set : Set.S with type elt = t

type 'a map = 'a Map.t

type set = Set.t

val pp : Format.formatter -> t -> unit

val compare : t -> t -> int
