type t = int

let cpt = ref 0

let fresh () = incr cpt ; !cpt

let compare = Int.compare

module Map = Map.Make (Int)
module Set = Set.Make (Int)

type 'a map = 'a Map.t

type set = Set.t

let pp ppf = Format.fprintf ppf "Block%i"
