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

module Make (Var : Set.OrderedType) = struct
  type var = Var.t

  type literal = True of Var.t | False of Var.t

  module Conjunction = Set.Make (struct
    type t = literal

    let compare x y =
      match (x, y) with
      | True x, True y | False x, False y ->
          Var.compare x y
      | True _, False _ ->
          1
      | False _, True _ ->
          -1
  end)

  module DNF = Set.Make (Conjunction)

  type t = DNF.t

  (** top *)
  let top = DNF.singleton Conjunction.empty

  (** bot *)
  let bot = DNF.empty

  let of_literal l = DNF.singleton (Conjunction.singleton l)

  let is_top v =
    if DNF.cardinal v = 1 then
      let x = DNF.choose v in
      Conjunction.is_empty x
    else false

  let is_bot = DNF.is_empty

  (** Conjunction of a formula with a litteral *)
  let ( &&& ) lit f =
    let opp_lit = match lit with True v -> False v | False v -> True v in
    DNF.fold
      (fun c conjs ->
        if Conjunction.mem opp_lit c then conjs
        else DNF.add (Conjunction.add lit c) conjs )
      f DNF.empty

  (** Disjunctions of two formulas *)
  let ( ||| ) f1 f2 =
    let rec simplify acc = function
      | [] ->
          acc
      | hd :: tl ->
          if DNF.exists (fun s -> Conjunction.subset s hd) acc then
            simplify acc tl
          else if List.exists (fun s -> Conjunction.subset s hd) tl then
            simplify acc tl
          else simplify DNF.(add hd acc) tl
    in
    simplify DNF.empty (DNF.to_list f1 @ DNF.to_list f2)

  let compare = DNF.compare

  let is_satified_by a b =
    DNF.for_all
      (fun b_conj ->
        DNF.exists (fun a_conj -> Conjunction.subset b_conj a_conj) a )
      b

  let pp_conf pp ppf conj =
    if Conjunction.is_empty conj then Format.pp_print_string ppf "⊤"
    else
      Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.fprintf ppf " ∧ ")
        (fun ppf -> function
          | True v ->
              Format.fprintf ppf "True(%a)" pp v
          | False v ->
              Format.fprintf ppf "False(%a)" pp v )
        ppf (Conjunction.to_list conj)

  let pp p ppf f =
    if DNF.is_empty f then Format.pp_print_string ppf "⊥"
    else
      Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.fprintf ppf " ∨ ")
        (fun ppf -> Format.fprintf ppf "(%a)" (pp_conf p))
        ppf (DNF.to_list f)
end
