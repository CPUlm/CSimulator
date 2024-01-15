open Ast
module VarGraph = Graph.Make (Variable)

let make_graph p =
  let g = VarGraph.empty () in
  Hashtbl.iter (fun v () -> VarGraph.add_node g v) p.p_vars ;
  let add_edge g n = function
    | Constant _ ->
        ()
    | Variable v ->
        VarGraph.add_edge g v n
  in
  Hashtbl.iter
    (fun n eq ->
      match eq with
      | Reg _ ->
          (* Does NOT add a dependancy *)
          ()
      | Mux md ->
          add_edge g n md.cond ;
          add_edge g n md.true_b ;
          add_edge g n md.false_b
      | Ram ram ->
          add_edge g n ram.read_addr
      | Rom rom ->
          (* Write is performed in at the end of the cycle. *)
          add_edge g n rom.read_addr
      | Binop (_, a, b) ->
          add_edge g n a ; add_edge g n b
      | Concat (a, b) ->
          add_edge g n a ; add_edge g n b
      | Slice s ->
          add_edge g n s.arg
      | Not x | Arg x | Select (_, x) ->
          add_edge g n x )
    p.p_eqs ;
  VarGraph.freeze g

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

type mut_block =
  {m_repr: Variable.t; m_members: Variable.t list; m_deps: Variable.t list}

type color = Color of int

module ColorSet = Set.Make (struct
  type t = color

  let compare (Color i) (Color j) = Int.compare i j
end)

type bloc = {repr: Variable.t; members: Variable.set; deps: ColorSet.t}

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

let split p =
  let order, g = variable_ordering p in
  let comp v1 v2 =
    Int.compare (Hashtbl.find order v1) (Hashtbl.find order v2)
  in
  let module VarHeap = Heap.Make (struct
    type t = Variable.t

    let compare = comp
  end) in
  let colors = Hashtbl.create 17 in
  (* [color_bloc] color the variable [var] in [col] and all its precedessor
     if they can be colored. *)
  let color_block var fresh_col =
    let node_colored = ref [] in
    let deps = ref [] in
    Hashtbl.add colors var fresh_col ;
    node_colored := var :: !node_colored ;
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
          node_colored := node :: !node_colored ;
          (* And add its parents to the node to be processed. *)
          let to_process =
            VarGraph.Set.fold (fun v h -> VarHeap.add v h) parents to_process
          in
          (* And we process them *)
          loop to_process )
        else (
          (* Nothing can be done. *)
          deps := node :: !deps ;
          loop to_process )
    in
    loop to_process ; (!node_colored, !deps)
  in
  let rec loop acc to_process =
    if VarHeap.size to_process = 0 then acc
    else
      let m_repr = VarHeap.find_min to_process in
      let to_process = VarHeap.del_min to_process in
      let acc =
        match Hashtbl.find_opt colors m_repr with
        | Some _ ->
            acc
        | None ->
            let new_col = fresh_color () in
            let m_members, m_deps = color_block m_repr new_col in
            {m_repr; m_members; m_deps} :: acc
      in
      loop acc to_process
  in
  let to_process = VarHeap.of_seq (Hashtbl.to_seq_keys p.p_eqs) in
  let blocks = loop [] to_process in
  let blocks =
    List.fold_left
      (fun bset mb ->
        let repr = mb.m_repr in
        let members = Variable.Set.of_list mb.m_members in
        let deps =
          List.map (Hashtbl.find colors) mb.m_deps |> ColorSet.of_list
        in
        Variable.Map.add repr {repr; members; deps} bset )
      Variable.Map.empty blocks
  in
  (g, colors, blocks)
