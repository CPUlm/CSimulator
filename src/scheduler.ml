open Netlist_ast
open Graph

exception Combinational_cycle

let read_exp (ident,exp) =
	let lst_from_arg = function
		| Avar i -> [i]
		| Aconst _ -> []
	in
	match exp with
		| Earg (a) -> lst_from_arg a
		| Ereg _ -> []
		| Enot a -> lst_from_arg a
		| Ebinop (_,a,b) -> (lst_from_arg a)@(lst_from_arg b)   (* size of the lists <= 1 *)
		| Emux (a,b,c) -> (lst_from_arg a)@(lst_from_arg b)@(lst_from_arg c)
		| Erom (_,_,a) -> lst_from_arg a
		| Eram (_,_,a,_,_,_) -> (lst_from_arg a) (* writing to the ram will take place at the end of the iteration and will therefore not cause a cycle *)
		| Econcat (a,b) -> (lst_from_arg a)@(lst_from_arg b)
		| Eslice (_,_,a) -> lst_from_arg a
		| Eselect (_,a) -> lst_from_arg a


let schedule p =
	let g = mk_graph() in
	List.iter (fun (ident,exp) -> add_node g ident) p.p_eqs;
	
	let rec is_not_input v = function
		| [] -> true
		| i::_ when i=v -> false
		| _::lstInput -> is_not_input v lstInput
	in
	let rec addLst nodeA = function
		| [] -> ()
		| nodeB::queue -> (if is_not_input nodeB p.p_inputs then add_edge g nodeB nodeA; addLst nodeA queue)
	in
		
	List.iter 
		(fun (ident,exp) -> addLst ident (read_exp (ident,exp)))
		p.p_eqs;
	
	try begin
		let item_order = topological g in
		
		let rec find_equa item = function
			| [] -> failwith "not found"
			| e::_ when fst e = item -> e
			| _::q -> find_equa item q
		in
		let rec build_p_eqs = function
			| [] -> []
			| item::lst -> (find_equa item p.p_eqs)::(build_p_eqs lst)
		in
		{p_eqs = build_p_eqs item_order; p_inputs = p.p_inputs; p_outputs = p.p_outputs; p_vars = p.p_vars}
	end with
		| Cycle -> raise Combinational_cycle

	

