open Ast

let axioms filtered p =
  let out_vars =
    Hashtbl.fold
      (fun v () set ->
        assert (not (filtered v)) ;
        Variable.Set.add v set )
      p.p_outputs Variable.Set.empty
  in
  let reg_vars =
    Hashtbl.fold
      (fun _ eq set ->
        match eq with
        | Reg v ->
            assert (not (filtered v)) ;
            Variable.Set.add v set
        | _ ->
            set )
      p.p_eqs Variable.Set.empty
  in
  let we_vars =
    Hashtbl.fold
      (fun _ eq set ->
        match eq with
        | Ram v -> (
          match v.write_enable with
          | Variable v ->
              assert (not (filtered v)) ;
              Variable.Set.add v set
          | Constant _ ->
              set )
        | _ ->
            set )
      p.p_eqs Variable.Set.empty
  in
  {reg_vars; out_vars; we_vars}

(** Compute the set of variable needed to compute the value of an expression. *)
let var_needed eq =
  let add_arg set = function
    | Variable v ->
        Variable.Set.add v set
    | Constant _ ->
        set
  in
  match eq with
  | Arg v | Not v | Select (_, v) ->
      add_arg Variable.Set.empty v
  | Reg _ ->
      Variable.Set.empty
  | Binop (_, lhs, rhs) | Concat (lhs, rhs) ->
      let set = add_arg Variable.Set.empty lhs in
      add_arg set rhs
  | Mux md ->
      let set = add_arg Variable.Set.empty md.cond in
      let set = add_arg set md.true_b in
      add_arg set md.false_b
  | Rom rod ->
      add_arg Variable.Set.empty rod.read_addr
  | Ram rad ->
      add_arg Variable.Set.empty rad.read_addr
  | Slice sd ->
      add_arg Variable.Set.empty sd.arg

let reachable_vars p =
  let reachable = Hashtbl.create 17 in
  let rec loop set =
    if Variable.Set.is_empty set then ()
    else
      let v = Variable.Set.choose set in
      let set = Variable.Set.remove v set in
      if Hashtbl.mem reachable v then loop set
      else (
        Hashtbl.add reachable v () ;
        let to_process =
          match Hashtbl.find_opt p.p_eqs v with
          | Some eq ->
              Variable.Set.union (var_needed eq) set
          | None ->
              set
        in
        loop to_process )
  in
  let to_process = Hashtbl.to_seq_keys p.p_outputs |> Variable.Set.of_seq in
  let to_process =
    Hashtbl.fold
      (fun _ eq to_process ->
        let add_v to_process = function
          | Variable v ->
              Variable.Set.add v to_process
          | Constant _ ->
              to_process
        in
        match eq with
        | Reg v ->
            Variable.Set.add v to_process
        | Ram rd ->
            let to_process = add_v to_process rd.write_enable in
            let to_process = add_v to_process rd.write_addr in
            let to_process = add_v to_process rd.write_data in
            to_process
        | _ ->
            to_process )
      p.p_eqs to_process
  in
  let () = loop to_process in
  reachable

let make_graph filtered p =
  let do_if_not_filtered fct v = if filtered v then () else fct v in
  let g = VarGraph.empty () in
  Hashtbl.iter (fun v () -> do_if_not_filtered (VarGraph.add_node g) v) p.p_vars ;
  let add_edge g n = function
    | Constant _ ->
        ()
    | Variable v ->
        do_if_not_filtered (fun v -> VarGraph.add_edge g v n) v
  in
  Hashtbl.iter
    (fun n eq ->
      if filtered n then ()
      else
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

let to_graph ?(clean = true) p =
  let filtered, nb_useless =
    if clean then
      let r = reachable_vars p in
      let nb_useless =
        Hashtbl.fold
          (fun v () acc -> if Hashtbl.mem r v then acc else acc + 1)
          p.p_vars 0
      in
      ((fun v -> not (Hashtbl.mem r v)), nb_useless)
    else ((fun _ -> false), 0)
  in
  let input_vars =
    Hashtbl.fold
      (fun v () set -> if filtered v then set else Variable.Set.add v set)
      p.p_inputs Variable.Set.empty
  in
  let output_vars =
    Hashtbl.fold
      (fun v () set ->
        assert (not (filtered v)) ;
        Variable.Set.add v set )
      p.p_outputs Variable.Set.empty
  in
  let vars =
    Hashtbl.fold
      (fun v () set -> if filtered v then set else Variable.Set.add v set)
      p.p_vars Variable.Set.empty
  in
  let eqs = Hashtbl.copy p.p_eqs in
  let () =
    Hashtbl.filter_map_inplace
      (fun v eq -> if filtered v then None else Some eq)
      eqs
  in
  let axioms = axioms filtered p in
  let deps_graph = make_graph filtered p in
  let order = VarGraph.topological deps_graph in
  ({input_vars; output_vars; vars; deps_graph; order; axioms; eqs}, nb_useless)
