exception Cycle

type mark = NotVisited | InProgress | Visited

type 'a graph = {mutable g_nodes: 'a node list}

and 'a node =
  { n_label: 'a
  ; mutable n_mark: mark
  ; mutable n_link_to: 'a node list
  ; mutable n_linked_by: 'a node list }

let mk_graph () = {g_nodes= []}

let add_node g x =
  let n = {n_label= x; n_mark= NotVisited; n_link_to= []; n_linked_by= []} in
  g.g_nodes <- n :: g.g_nodes

let node_of_label g x = List.find (fun n -> n.n_label = x) g.g_nodes

let add_edge g id1 id2 =
  try
    let n1 = node_of_label g id1 in
    let n2 = node_of_label g id2 in
    n1.n_link_to <- n2 :: n1.n_link_to ;
    n2.n_linked_by <- n1 :: n2.n_linked_by
  with Not_found ->
    Format.eprintf "Tried to add an edge between non-existing nodes" ;
    raise Not_found

let clear_marks g = List.iter (fun n -> n.n_mark <- NotVisited) g.g_nodes

let find_roots g = List.filter (fun n -> n.n_linked_by = []) g.g_nodes

let has_cycle g =
  clear_marks g ;
  let rec dfs node =
    if node.n_mark = InProgress then true
    else if node.n_mark = Visited then false
    else (
      node.n_mark <- InProgress ;
      let result =
        List.fold_left
          (fun result neighbor -> result || dfs neighbor)
          false node.n_link_to
      in
      node.n_mark <- Visited ;
      result )
  in
  List.fold_left (fun result node -> result || dfs node) false g.g_nodes

let topological g =
  clear_marks g ;
  let topo = ref [] in
  let rec dfs node =
    if node.n_mark = InProgress then raise Cycle
    else if node.n_mark = Visited then ()
    else (
      node.n_mark <- InProgress ;
      List.iter (fun neighbor -> dfs neighbor) node.n_link_to ;
      node.n_mark <- Visited ;
      topo := node.n_label :: !topo )
  in
  List.iter dfs g.g_nodes ; !topo
