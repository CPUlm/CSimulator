open Ast
open Format
open BlockSplitter

let phi = (1. +. sqrt 5.) /. 2.

let luminance (r, g, b) =
  let norm x =
    if x <= 0.03928 then x /. 12.92 else Float.pow ((x +. 0.055) /. 1.055) 2.4
  in
  let r, g, b = (norm r, norm g, norm b) in
  (0.2126 *. r) +. (0.7152 *. g) +. (0.0722 *. b)

let color_of_text rgb =
  let black = (0.0, 0.0, 0.0) in
  let white = (0.0, 0.0, 1.0) in
  let cr_ratio = (luminance rgb +. 0.05) /. (luminance black +. 0.05) in
  if cr_ratio < 5. then white else black

let hsv_to_rgb (h, s, v) =
  let h = h *. 360. in
  let ti = int_of_float (h /. 60.) mod 6 in
  let f = (h /. 60.) -. float_of_int ti in
  let l = v *. (1. -. s) in
  let m = v *. (1. -. (f *. s)) in
  let n = v *. (1. -. ((1. -. f) *. s)) in
  match ti with
  | 0 ->
      (v, n, l)
  | 1 ->
      (m, v, l)
  | 2 ->
      (l, v, n)
  | 3 ->
      (l, m, v)
  | 4 ->
      (n, l, v)
  | 5 ->
      (v, l, m)
  | _ ->
      assert false

let pp_inputs ppf p =
  Variable.Set.iter
    (fun v ->
      fprintf ppf "\t%a  [label=<<b>%a</b><br/><i>size</i>: %i>" Variable.pp v
        Variable.pp v (Variable.size v) ;
      if Variable.Set.mem v p.output_vars then fprintf ppf ", shape=rect]@."
      else fprintf ppf "]\n@." )
    p.input_vars

let pp_arg ppf = function
  | Variable v ->
      Variable.pp ppf v
  | Constant c ->
      fprintf ppf "%#x" c.value

let pp_binop ppf = function
  | And ->
      pp_print_string ppf "And"
  | Or ->
      pp_print_string ppf "Or"
  | Nand ->
      pp_print_string ppf "Nand"
  | Xor ->
      pp_print_string ppf "Xor"

let pp_eq ppf = function
  | Arg a ->
      pp_arg ppf a
  | Reg v ->
      Variable.pp ppf v
  | Not a ->
      pp_arg ppf a
  | Binop (binop, lhs, rhs) ->
      fprintf ppf "%a %a %a" pp_binop binop pp_arg lhs pp_arg rhs
  | Mux md ->
      fprintf ppf "MUX %a %a %a" pp_arg md.cond pp_arg md.true_b pp_arg
        md.false_b
  | Rom rd ->
      fprintf ppf "ROM %a %i %i" pp_arg rd.read_addr rd.addr_size rd.word_size
  | Ram rd ->
      fprintf ppf "RAM %a %i %i %a %a %a" pp_arg rd.read_addr rd.addr_size
        rd.word_size pp_arg rd.write_enable pp_arg rd.write_addr pp_arg
        rd.write_data
  | Concat (lhs, rhs) ->
      fprintf ppf "CONCAT %a %a" pp_arg lhs pp_arg rhs
  | Slice s ->
      fprintf ppf "SLICE %i %i %a" s.min s.max pp_arg s.arg
  | Select (index, arg) ->
      fprintf ppf "SELECT %i %a" index pp_arg arg

let pp_arg_link ppf ?(weak = false) (src, dest) =
  match dest with
  | Variable v ->
      if weak then
        fprintf ppf "\t%a -> %a [style=dashed]@." Variable.pp v Variable.pp src
      else fprintf ppf "\t%a -> %a@." Variable.pp v Variable.pp src
  | Constant _ ->
      ()

let pp_links ppf (src, eq) =
  match eq with
  | Arg a ->
      pp_arg_link ppf (src, a)
  | Reg v ->
      pp_arg_link ppf ~weak:true (src, Variable v)
  | Not a ->
      pp_arg_link ppf (src, a)
  | Binop (_, lhs, rhs) ->
      pp_arg_link ppf (src, lhs) ;
      pp_arg_link ppf (src, rhs)
  | Mux md ->
      pp_arg_link ppf (src, md.cond) ;
      pp_arg_link ppf (src, md.true_b) ;
      pp_arg_link ppf (src, md.false_b)
  | Rom d ->
      pp_arg_link ppf (src, d.read_addr)
  | Ram d ->
      pp_arg_link ppf (src, d.read_addr) ;
      pp_arg_link ppf ~weak:true (src, d.write_enable) ;
      pp_arg_link ppf ~weak:true (src, d.write_addr) ;
      pp_arg_link ppf ~weak:true (src, d.write_data)
  | Concat (lhs, rhs) ->
      pp_arg_link ppf (src, lhs) ;
      pp_arg_link ppf (src, rhs)
  | Slice a ->
      pp_arg_link ppf (src, a.arg)
  | Select (_, a) ->
      pp_arg_link ppf (src, a)

let pp_col ppf (h, s, v) = fprintf ppf "\"%f %f %f\"" h s v

let pp_node_col ppf (color_map, v) =
  match color_map with
  | None ->
      ()
  | Some col ->
      let back_col =
        let h =
          let (Color col) = Hashtbl.find col v in
          let col = float_of_int col in
          (col *. phi) -. floor (col *. phi)
        in
        (fst (Float.modf h), 1.0, 1.0)
      in
      let text_col = color_of_text (hsv_to_rgb back_col) in
      fprintf ppf "style=\"filled\",fillcolor=%a,fontcolor=%a," pp_col back_col
        pp_col text_col

let pp_eqs ppf (p, color_map) =
  Hashtbl.iter
    (fun v exp ->
      fprintf ppf "\t%a  [%alabel=<%a<br/><i>size</i>: %i<br/><i>eq</i>: %a>"
        Variable.pp v pp_node_col (color_map, v) Variable.pp v (Variable.size v)
        pp_eq exp ;
      if Variable.Set.mem v p.output_vars then fprintf ppf ", shape=rect]@."
      else fprintf ppf "]@." ;
      fprintf ppf "%a@." pp_links (v, exp) )
    p.eqs

let pp_graph ppf (p, colors) =
  fprintf ppf "digraph {%a@.%a}@." pp_inputs p pp_eqs (p, colors)
