open Ast

type mut_block =
  {m_repr: Variable.t; m_members: Variable.t list; m_deps: Variable.t list}

type color = Color of int

module ColorSet = Set.Make (struct
  type t = color

  let compare (Color i) (Color j) = Int.compare i j
end)

type block = {repr: Variable.t; members: Variable.set; deps: Variable.set}

let fresh_color =
  let cpt = ref 0 in
  fun () -> incr cpt ; Color !cpt

let split program =
  let module VarHeap = Heap.Make (struct
    type t = Variable.t

    let compare v1 v2 =
      Int.compare
        (Hashtbl.find program.order v1)
        (Hashtbl.find program.order v2)
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
      let var_parents = VarGraph.parents program.deps_graph var in
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
        let childrens = VarGraph.children program.deps_graph node in
        let parents = VarGraph.parents program.deps_graph node in
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
  let to_process =
    VarHeap.of_seq
      Variable.Set.(
        union
          (union program.axioms.we_vars program.axioms.out_vars)
          program.axioms.reg_vars
        |> to_seq )
  in
  let blocks = loop [] to_process in
  let to_process = VarHeap.of_seq (Hashtbl.to_seq_keys program.eqs) in
  let blocks = loop blocks to_process in
  let blocks =
    List.fold_left
      (fun bset mb ->
        let repr = mb.m_repr in
        let members = Variable.Set.of_list mb.m_members in
        let deps = mb.m_deps |> Variable.Set.of_list in
        Variable.Map.add repr {repr; members; deps} bset )
      Variable.Map.empty blocks
  in
  (colors, blocks)
