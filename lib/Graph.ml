module Make (Ord : Map.OrderedType) = struct
  module Map = Map.Make (Ord)
  module Set = Set.Make (Ord)

  exception Cycle

  type mark = NotVisited | InProgress | Visited

  type mut_node =
    { mut_childrens: (Ord.t, unit) Hashtbl.t
    ; mut_parents: (Ord.t, unit) Hashtbl.t }

  type mut = (Ord.t, mut_node) Hashtbl.t

  type node = {n_parents: Set.t; n_childrens: Set.t}

  type t = node Map.t

  let empty () = Hashtbl.create 17

  let add_node g x =
    if Hashtbl.mem g x then failwith "Multiple def of a node"
    else
      let n =
        {mut_childrens= Hashtbl.create 7; mut_parents= Hashtbl.create 7}
      in
      Hashtbl.add g x n

  let add_edge g id1 id2 =
    let n1 = Hashtbl.find g id1 in
    let n2 = Hashtbl.find g id2 in
    Hashtbl.replace n1.mut_childrens id2 () ;
    Hashtbl.replace n2.mut_parents id1 ()

  let cpt = ref 0

  let topological g =
    let ordering = Hashtbl.create 17 in
    let marks = Hashtbl.create 17 in
    let rec dfs node =
      match Hashtbl.find_opt marks node with
      | Some InProgress ->
          raise Cycle
      | Some Visited ->
          ()
      | _ ->
          Hashtbl.add marks node InProgress ;
          let children = (Map.find node g).n_childrens in
          Set.iter dfs children ;
          Hashtbl.add marks node Visited ;
          incr cpt ;
          Hashtbl.add ordering node !cpt
    in
    cpt := 0 ;
    Map.iter (fun n _ -> dfs n) g ;
    ordering

  let hasttbl_to_set h =
    Hashtbl.fold (fun elm () set -> Set.add elm set) h Set.empty

  let freeze mut_g =
    Hashtbl.fold
      (fun n mut_n map ->
        let n_parents = hasttbl_to_set mut_n.mut_parents in
        let n_childrens = hasttbl_to_set mut_n.mut_childrens in
        Map.add n {n_parents; n_childrens} map )
      mut_g Map.empty

  let children g n =
    match Map.find_opt n g with None -> Set.empty | Some n -> n.n_childrens

  let parents g n =
    match Map.find_opt n g with None -> Set.empty | Some n -> n.n_parents

  let iter f = Map.iter (fun n ns -> f n ns.n_parents ns.n_childrens)
end
