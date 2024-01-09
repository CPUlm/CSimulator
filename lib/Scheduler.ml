open Ast

let make_graph p =
  let g = Graph.empty_graph () in
  let add_edge n = function
    | Aconst _ ->
        ()
    | Avar v ->
        if Variable.Set.mem v p.p_inputs then () else Graph.add_edge g n v
  in
  Variable.Map.iter
    (fun n -> function
      | Ereg _ ->
          (* Does NOT add a dependancy *)
          ()
      | Emux (a, b, c) ->
          add_edge n a ; add_edge n b ; add_edge n c
      | Eram ram ->
          add_edge n ram.read_addr
      | Erom rom ->
          (* Write is performed in at the end of the cycle. *)
          add_edge n rom.read_addr
      | Ebinop (_, a, b) ->
          add_edge n a ; add_edge n b
      | Econcat (a, b) ->
          add_edge n a ; add_edge n b
      | Eslice s ->
          add_edge n s.arg
      | Enot x | Earg x | Eselect (_, x) ->
          add_edge n x )
    p.p_eqs ;
  g

let schedule p =
  let g = make_graph p in
  let top = Graph.topological g in
  let n = Hashtbl.length top in
  (top, n)
