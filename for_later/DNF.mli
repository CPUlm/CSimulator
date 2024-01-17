module type S = sig
  type var

  type literal = True of var | False of var

  type t

  val top : t

  val bot : t

  val ( &&& ) : literal -> t -> t

  val ( ||| ) : t -> t -> t

  val of_literal : literal -> t

  val is_bot : t -> bool

  val is_top : t -> bool

  val compare : t -> t -> int

  val is_satified_by : t -> t -> bool

  val pp : (Format.formatter -> var -> unit) -> Format.formatter -> t -> unit
end

module Make (M : Set.OrderedType) : S with type var = M.t
