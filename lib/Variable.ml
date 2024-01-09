type t = string

let var_size = Hashtbl.create 17

let fresh n typ =
  if Hashtbl.mem var_size n then failwith "This var is already defined"
  else Hashtbl.add var_size n typ ;
  n

let size n = Hashtbl.find var_size n

let exists n = if Hashtbl.mem var_size n then Some n else None

module Map = Map.Make (String)
module Set = Set.Make (String)

type 'a map = 'a Map.t

type set = Set.t

let pp ppf = Format.pp_print_string ppf

let compare = String.compare
