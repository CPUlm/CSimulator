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

  type dnf = Conjunction.t list

  (** top *)
  let top = [Conjunction.empty]

  (** bot *)
  let bot = []

  let of_literal l = [Conjunction.singleton l]

  let is_top v =
    match v with [x] when Conjunction.is_empty x -> true | _ -> false

  let is_bot v = match v with [] -> true | _ -> false

  (** Conjunction of a formula with a litteral *)
  let ( &&& ) lit f =
    let opp_lit = match lit with True v -> False v | False v -> True v in
    List.fold_left
      (fun conjs c ->
        if Conjunction.mem opp_lit c then conjs
        else Conjunction.add lit c :: conjs )
      [] f

  (** Disjunctions of two formulas *)
  let ( ||| ) f1 f2 =
    let rec simplify acc = function
      | [] ->
          acc
      | hd :: tl ->
          if List.exists (fun s -> Conjunction.subset s hd) acc then
            simplify acc tl
          else if List.exists (fun s -> Conjunction.subset s hd) tl then
            simplify acc tl
          else simplify (hd :: acc) tl
    in
    simplify [] (f1 @ f2)

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
    if f = [] then Format.pp_print_string ppf "⊥"
    else
      match f with
      | [x] when Conjunction.is_empty x ->
          Format.pp_print_string ppf "⊤"
      | _ ->
          Format.pp_print_list
            ~pp_sep:(fun ppf () -> Format.fprintf ppf " ∨ ")
            (fun ppf -> Format.fprintf ppf "(%a)" (pp_conf p))
            ppf f
end
