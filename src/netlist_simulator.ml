open Netlist_ast

let print_only = ref false
let number_steps = ref (-1)
let quiet = ref false

let compile filename =
	try
		let p = Netlist.read_file filename in
		begin try
				let p = Scheduler.schedule p in
				if !print_only then Netlist_printer.print_program stdout p
				else begin
					let c_prog = Netlist_to_c.to_c p !number_steps in
					(*let c_file = open_out "prog.cpp" in
					Printf.fprintf c_file "%s\n" c_prog;
					close_out c_file*)
					Printf.printf "%s" c_prog
				end
			with
				| Scheduler.Combinational_cycle ->
						Format.eprintf "The netlist has a combinatory cycle.@.";
		end;
	with
		| Netlist.Parse_error s -> Format.eprintf "An error accurred: %s@." s; exit 2
		

let main () =
	Arg.parse
		[("-n", Arg.Set_int number_steps, "Number of steps to simulate");
		 ("--print", Arg.Set print_only, "print the sorted net-list, without simulating it");
		 ("-q", Arg.Set quiet, "De not show the questions (eg a=... is not printed)")]
		compile
		""
;;

main ()
