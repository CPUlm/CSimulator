exception Cycle

type mark = NotVisited | InProgress | Visited

type 'a node =
  { n_label: 'a
  ; mutable n_mark: mark
  ; mutable n_childrens: ('a, 'a node) Hashtbl.t
  ; mutable n_parents: ('a, 'a node) Hashtbl.t }

type 'a graph =
  {mutable g_nodes: ('a, 'a node) Hashtbl.t; g_index: ('a, int) Hashtbl.t}

let empty_graph () = {g_nodes= Hashtbl.create 17; g_index= Hashtbl.create 17}

let add_node g x =
  if Hashtbl.mem g.g_nodes x then failwith "Multiple def of a node"
  else
    let n =
      { n_label= x
      ; n_mark= NotVisited
      ; n_childrens= Hashtbl.create 17
      ; n_parents= Hashtbl.create 17 }
    in
    Hashtbl.add g.g_nodes x n ; n

let node_of_label g x =
  match Hashtbl.find_opt g.g_nodes x with Some n -> n | None -> add_node g x

(** [add_edge g n1 n2] adds an edge from [n1] to [n2]. *)
let add_edge g id1 id2 =
  let n1 = node_of_label g id1 in
  let n2 = node_of_label g id2 in
  if Hashtbl.mem n1.n_childrens id2 then failwith "Already a Child"
  else Hashtbl.add n1.n_childrens id2 n2 ;
  if Hashtbl.mem n2.n_parents id1 then failwith "Already a Parent"
  else Hashtbl.add n1.n_childrens id2 n2

let clear_marks g = Hashtbl.iter (fun _ n -> n.n_mark <- NotVisited) g.g_nodes

let cpt = ref 0

let rec dfs g node =
  if node.n_mark = InProgress then raise Cycle
  else if node.n_mark = Visited then ()
  else (
    node.n_mark <- InProgress ;
    Hashtbl.iter (fun _ -> dfs g) node.n_childrens ;
    node.n_mark <- Visited ;
    incr cpt ;
    Hashtbl.add g.g_index node.n_label !cpt )

let topological g =
  clear_marks g ;
  cpt := 0 ;
  Hashtbl.iter (fun _ -> dfs g) g.g_nodes ;
  g.g_index
