open Ast
module VarPos = DNF.Make (Variable)

type axiom =
  {reg_vars: Variable.set; out_vars: Variable.set; we_vars: Variable.set}

let axioms p =
  let out_vars = Variable.Set.of_seq (Hashtbl.to_seq_keys p.p_outputs) in
  let reg_vars =
    Hashtbl.fold
      (fun _ eq set ->
        match eq with Reg v -> Variable.Set.add v set | _ -> set )
      p.p_eqs Variable.Set.empty
  in
  let we_vars =
    Hashtbl.fold
      (fun _ eq set ->
        match eq with
        | Ram v -> (
          match v.write_enable with
          | Variable v ->
              Variable.Set.add v set
          | Constant _ ->
              set )
        | _ ->
            set )
      p.p_eqs Variable.Set.empty
  in
  {reg_vars; out_vars; we_vars}

let compute_pos p order =
  let comp v1 v2 =
    Int.compare (Hashtbl.find order v1) (Hashtbl.find order v2)
  in
  let module VarHeap = Heap.Make (struct
    type t = Variable.t

    let compare = comp
  end) in
  let p_axioms = axioms p in
  let var_pos =
    let var_pos = Hashtbl.create 17 in
    let () =
      Hashtbl.iter (fun v () -> Hashtbl.add var_pos v VarPos.bot) p.p_vars
    in
    var_pos
  in
  let process var heap =
    let vpos = Hashtbl.find var_pos var in
    let add_var_var heap pos = function
      | Constant _ ->
          heap
      | Variable var ->
          let heap = VarHeap.add var heap in
          let () =
            let s = Hashtbl.find var_pos var in
            Hashtbl.replace var_pos var VarPos.(s ||| pos)
          in
          heap
    in
    match Hashtbl.find_opt p.p_eqs var with
    | None | Some (Reg _) ->
        heap
    | Some (Arg v | Not v | Select (_, v)) ->
        add_var_var heap vpos v
    | Some (Binop (_, v1, v2) | Concat (v1, v2)) ->
        let heap = add_var_var heap vpos v1 in
        add_var_var heap vpos v2
    | Some (Rom rd) ->
        add_var_var heap vpos rd.read_addr
    | Some (Slice sd) ->
        add_var_var heap vpos sd.arg
    | Some (Ram rd) ->
        let ram_pos =
          match rd.write_enable with
          | Variable v ->
              VarPos.of_literal (True v)
          | Constant _ ->
              vpos
        in
        let heap = add_var_var heap vpos rd.read_addr in
        let heap = add_var_var heap ram_pos rd.write_addr in
        add_var_var heap ram_pos rd.write_data
    | Some (Mux md) -> (
      match md.cond with
      | Variable v ->
          let heap = add_var_var heap vpos md.cond in
          let heap = add_var_var heap VarPos.(True v &&& vpos) md.true_b in
          add_var_var heap VarPos.(False v &&& vpos) md.false_b
      | Constant _ ->
          let heap = add_var_var heap vpos md.cond in
          let heap = add_var_var heap vpos md.true_b in
          add_var_var heap vpos md.false_b )
  in
  let processed = Hashtbl.create 17 in
  let rec loop heap =
    if VarHeap.size heap = 0 then ()
    else
      let v = VarHeap.find_min heap in
      let heap = VarHeap.del_min heap in
      if Hashtbl.mem processed v then loop heap
      else
        let heap = process v heap in
        let () = Hashtbl.add processed v () in
        loop heap
  in
  let heap =
    Variable.Set.fold
      (fun v heap ->
        Hashtbl.replace var_pos v VarPos.top ;
        VarHeap.add v heap )
      p_axioms.reg_vars VarHeap.empty
  in
  let heap =
    Variable.Set.fold
      (fun v heap ->
        Hashtbl.replace var_pos v VarPos.top ;
        VarHeap.add v heap )
      p_axioms.out_vars heap
  in
  let heap =
    Variable.Set.fold
      (fun v heap ->
        Hashtbl.replace var_pos v VarPos.top ;
        VarHeap.add v heap )
      p_axioms.we_vars heap
  in
  let () = loop heap in
  (var_pos, p_axioms)

let split_nodes vpos =
  Hashtbl.fold
    (fun v vp (useless, always) ->
      if VarPos.is_bot vp then (Variable.Set.add v useless, always)
      else if VarPos.is_top vp then (useless, Variable.Set.add v always)
      else (useless, always) )
    vpos
    Variable.Set.(empty, empty)

let regroup_functions () = assert false

let center ?(filler = ' ') size pp i =
  let text = Format.asprintf "%a" pp i in
  let left_padding, right_padding =
    let text_length = String.length text in
    let left_size = (size - text_length) / 2 in
    ( String.make left_size filler
    , String.make (size - left_size - text_length) filler )
  in
  left_padding ^ text ^ right_padding

let pp_pos ppf (var_pos, top, p) =
  let l =
    Hashtbl.to_seq top |> List.of_seq
    |> List.sort (fun (_, i) (_, j) -> Int.compare i j)
  in
  let col_width = 20 in
  let sep = String.make (5 * col_width) '-' in
  Format.fprintf ppf "%s|%s|%s|%s|%s@.%s@."
    (center col_width Format.pp_print_string "Variable")
    (center col_width Format.pp_print_string "Ordering")
    (center col_width Format.pp_print_string "Input")
    (center col_width Format.pp_print_string "Output")
    (center col_width Format.pp_print_string "Position")
    sep ;
  List.iter
    (fun (v, i) ->
      let c ppf () =
        let vpos = Hashtbl.find var_pos v in
        VarPos.pp Variable.pp ppf vpos
      in
      Format.fprintf ppf "%s|%s|%s|%s|  %a@."
        (center col_width Variable.pp v)
        (center col_width Format.pp_print_int i)
        (center col_width Format.pp_print_string
           (if Hashtbl.mem p.p_inputs v then "x" else "") )
        (center col_width Format.pp_print_string
           (if Hashtbl.mem p.p_outputs v then "x" else "") )
        c () )
    l ;
  Format.fprintf ppf "%s@." sep
