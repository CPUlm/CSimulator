open Ast

let compute_pos program =
  let module VarHeap = Heap.Make (struct
    type t = Variable.t

    let compare v1 v2 =
      Int.compare
        (Hashtbl.find program.order v1)
        (Hashtbl.find program.order v2)
  end) in
  let var_pos =
    let var_pos = Hashtbl.create 17 in
    let () =
      Variable.Set.iter (fun v -> Hashtbl.add var_pos v VarPos.bot) program.vars
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
    match Hashtbl.find_opt program.eqs var with
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
      program.axioms.reg_vars VarHeap.empty
  in
  let heap =
    Variable.Set.fold
      (fun v heap ->
        Hashtbl.replace var_pos v VarPos.top ;
        VarHeap.add v heap )
      program.axioms.out_vars heap
  in
  let heap =
    Variable.Set.fold
      (fun v heap ->
        Hashtbl.replace var_pos v VarPos.top ;
        VarHeap.add v heap )
      program.axioms.we_vars heap
  in
  let () = loop heap in
  var_pos

let split_nodes vpos =
  Hashtbl.fold
    (fun v vp (useless, always) ->
      if VarPos.is_bot vp then (Variable.Set.add v useless, always)
      else if VarPos.is_top vp then (useless, Variable.Set.add v always)
      else (useless, always) )
    vpos
    Variable.Set.(empty, empty)

module VPosSet = Set.Make (VarPos)

let pp_pos ppf (var_pos, program) =
  let l =
    Hashtbl.to_seq program.order
    |> List.of_seq
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
           (if Variable.Set.mem v program.input_vars then "x" else "") )
        (center col_width Format.pp_print_string
           (if Variable.Set.mem v program.output_vars then "x" else "") )
        c () )
    l ;
  Format.fprintf ppf "%s@." sep
