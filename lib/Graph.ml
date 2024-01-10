module Make (Ord : Map.OrderedType) = struct
  module Map = Map.Make (Ord)
  module Set = Set.Make (Ord)

  exception Cycle

  type mark = NotVisited | InProgress | Visited

  type node =
    {mutable n_mark: mark; mutable n_childrens: Set.t; mutable n_parents: Set.t}

  type t = node Map.t

  let empty = Map.empty

  let add_node g x =
    if Map.mem x g then failwith "Multiple def of a node"
    else
      let n =
        {n_mark= NotVisited; n_childrens= Set.empty; n_parents= Set.empty}
      in
      Map.add x n g

  let node_of_label g x =
    match Map.find_opt x g with
    | Some n ->
        (n, g)
    | None ->
        failwith "Unknown node"

  let add_edge g id1 id2 =
    let n1, g = node_of_label g id1 in
    let n2, g = node_of_label g id2 in
    if Set.mem id2 n1.n_childrens then ()
    else n1.n_childrens <- Set.add id2 n1.n_childrens ;
    if Set.mem id1 n2.n_parents then ()
    else n2.n_parents <- Set.add id1 n2.n_parents ;
    g

  let clear_marks g = Map.iter (fun _ n -> n.n_mark <- NotVisited) g

  let cpt = ref 0

  let topological g =
    let ordering = Hashtbl.create 17 in
    let rec dfs g node =
      let node_struct = Map.find node g in
      if node_struct.n_mark = InProgress then raise Cycle
      else if node_struct.n_mark = Visited then ()
      else (
        node_struct.n_mark <- InProgress ;
        Set.iter (dfs g) node_struct.n_childrens ;
        node_struct.n_mark <- Visited ;
        incr cpt ;
        Hashtbl.add ordering node !cpt )
    in
    clear_marks g ;
    cpt := 0 ;
    Map.iter (fun n _ -> dfs g n) g ;
    ordering

  let children g n =
    match Map.find_opt n g with None -> Set.empty | Some n -> n.n_childrens

  let parents g n =
    match Map.find_opt n g with None -> Set.empty | Some n -> n.n_parents

  let iter f = Map.iter (fun n ns -> f n ns.n_parents ns.n_childrens)
end
