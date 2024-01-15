module type S = sig
  type var

  type literal = True of var | False of var

  type dnf

  val top : dnf

  val bot : dnf

  val ( &&& ) : literal -> dnf -> dnf

  val ( ||| ) : dnf -> dnf -> dnf

  val of_literal : literal -> dnf

  val is_bot : dnf -> bool

  val is_top : dnf -> bool

  val pp : (Format.formatter -> var -> unit) -> Format.formatter -> dnf -> unit
end

module Make (M : Set.OrderedType) : S with type var = M.t
