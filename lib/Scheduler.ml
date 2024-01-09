open Ast
module VarGraph = Graph.Make (Variable)

let make_graph p =
  let add_edge g n = function
    | Aconst _ ->
        g
    | Avar v ->
        VarGraph.add_edge g v n
  in
  Hashtbl.fold
    (fun n eq g ->
      match eq with
      | Ereg _ ->
          (* Does NOT add a dependancy *)
          g
      | Emux (a, b, c) ->
          let g = add_edge g n a in
          let g = add_edge g n b in
          let g = add_edge g n c in
          g
      | Eram ram ->
          let g = add_edge g n ram.read_addr in
          g
      | Erom rom ->
          (* Write is performed in at the end of the cycle. *)
          let g = add_edge g n rom.read_addr in
          g
      | Ebinop (_, a, b) ->
          let g = add_edge g n a in
          let g = add_edge g n b in
          g
      | Econcat (a, b) ->
          let g = add_edge g n a in
          let g = add_edge g n b in
          g
      | Eslice s ->
          let g = add_edge g n s.arg in
          g
      | Enot x | Earg x | Eselect (_, x) ->
          let g = add_edge g n x in
          g )
    p.p_eqs VarGraph.empty

let variable_ordering p =
  let g = make_graph p in
  let top = VarGraph.topological g in
  (top, g)

let pp_set pp ppf ht =
  Format.fprintf ppf "@[" ;
  VarGraph.Set.iter (fun a -> Format.fprintf ppf "%a@ " pp a) ht ;
  Format.fprintf ppf "@]"

let pp_graph pp ppf g =
  VarGraph.iter
    (fun id par chi ->
      Format.fprintf ppf "Node %a:@.@[<hv 2>Childrens: %a@;Parents: %a@]@." pp
        id (pp_set pp) chi (pp_set pp) par )
    g

type block = {repr: Variable.t; members: (Variable.t, unit) Hashtbl.t}

type color = Color of int

let center ?(filler = ' ') size pp i =
  let text = Format.asprintf "%a" pp i in
  let left_padding, right_padding =
    let text_length = String.length text in
    let left_size = (size - text_length) / 2 in
    ( String.make left_size filler
    , String.make (size - left_size - text_length) filler )
  in
  left_padding ^ text ^ right_padding

let pp_color ppf (colors, top, p) =
  let l =
    Hashtbl.to_seq top |> List.of_seq
    |> List.sort (fun (_, i) (_, j) -> Int.compare i j)
  in
  let col_width = 20 in
  let sep = String.make (5 * col_width) '-' in
  Format.fprintf ppf "%s|%s|%s|%s|%s@.%s@."
    (center col_width Format.pp_print_string "Variable")
    (center col_width Format.pp_print_string "Ordering")
    (center col_width Format.pp_print_string "Color")
    (center col_width Format.pp_print_string "Input")
    (center col_width Format.pp_print_string "Output")
    sep ;
  List.iter
    (fun (v, i) ->
      let c =
        match Hashtbl.find_opt colors v with
        | Some (Color i) ->
            string_of_int i
        | None ->
            "None"
      in
      Format.fprintf ppf "%s|%s|%s|%s|%s@."
        (center col_width Variable.pp v)
        (center col_width Format.pp_print_int i)
        (center col_width Format.pp_print_string c)
        (center col_width Format.pp_print_string
           (if Hashtbl.mem p.p_inputs v then "x" else "") )
        (center col_width Format.pp_print_string
           (if Hashtbl.mem p.p_outputs v then "x" else "") ) )
    l ;
  Format.fprintf ppf "%s@." sep

let fresh_color =
  let cpt = ref 0 in
  fun () -> incr cpt ; Color !cpt

exception NotTrue

let split_in_block p =
  let order, g = variable_ordering p in
  let comp v1 v2 =
    Int.compare (Hashtbl.find order v1) (Hashtbl.find order v2)
  in
  let module VarHeap = Heap.Make (struct
    type t = Variable.t

    let compare = comp
  end) in
  let colors = Hashtbl.create 17 in
  let blocks = Hashtbl.create 17 in
  (* [color_bloc] color the variable [var] in [col] and all its precedessor
     if they can be colored. *)
  let color_block var fresh_col =
    let node_colored = Hashtbl.create 17 in
    Hashtbl.add colors var fresh_col ;
    Hashtbl.add node_colored var () ;
    let to_process =
      let var_parents = VarGraph.parents g var in
      VarGraph.Set.fold (fun v h -> VarHeap.add v h) var_parents VarHeap.empty
    in
    let rec loop to_process =
      (* Empty Heap, nothing to do *)
      if VarHeap.size to_process = 0 then ()
      else
        (* We extract the min node (respecting the topological order) *)
        let node = VarHeap.find_min to_process in
        let to_process = VarHeap.del_min to_process in
        (* We retrieve its parents & children *)
        let childrens = VarGraph.children g node in
        let parents = VarGraph.parents g node in
        (* If :
           - this node is not colored
           - all its children are colored with the current fresh color *)
        if
          (not (Hashtbl.mem colors node))
          && VarGraph.Set.for_all
               (fun v ->
                 match Hashtbl.find_opt colors v with
                 | Some col ->
                     col = fresh_col
                 | None ->
                     false )
               childrens
        then (
          (* We color it with the fresh color *)
          Hashtbl.add colors node fresh_col ;
          Hashtbl.add node_colored node () ;
          (* And add its parents to the node to be processed. *)
          let to_process =
            VarGraph.Set.fold (fun v h -> VarHeap.add v h) parents to_process
          in
          (* And we process them *)
          loop to_process )
        else (* Nothing can be done. *)
          loop to_process
    in
    loop to_process ; node_colored
  in
  let rec loop to_process =
    if VarHeap.size to_process = 0 then ()
    else
      let repr = VarHeap.find_min to_process in
      let to_process = VarHeap.del_min to_process in
      ( match Hashtbl.find_opt colors repr with
      | Some _ ->
          ()
      | None ->
          let new_col = fresh_color () in
          let members = color_block repr new_col in
          Hashtbl.add blocks {repr; members} () ) ;
      loop to_process
  in
  let to_process = VarHeap.of_seq (Hashtbl.to_seq_keys p.p_eqs) in
  loop to_process ; (colors, blocks)
