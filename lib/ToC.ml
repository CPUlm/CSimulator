let export_file file text =
  let file = open_out file in
  output_string file text ; close_out file

let files =
  [ ("main.c", LibC.Main.content)
  ; ("commons.h", LibC.Commons.header)
  ; ("memory.h", LibC.Memory.header)
  ; ("memory.c", LibC.Memory.content)
  ; ("screen.h", LibC.Screen.header)
  ; ("screen.c", LibC.Screen.content) ]

let generated_file = "logic.c"

let export_into out_dir genv =
  if Sys.file_exists out_dir then (
    if Sys.is_directory out_dir then
      Format.printf
        "Warning: @[<v>The directory %s is not empty!@,\
         Some of its content could be erased.@]@." out_dir )
  else Sys.mkdir out_dir 0o740 ;
  let () =
    List.iter
      (fun (filename, text) ->
        let file = Filename.concat out_dir filename in
        export_file file text )
      files
  in
  let ch = open_out (Filename.concat out_dir generated_file) in
  let fmt = Format.formatter_of_out_channel ch in
  let () = WriteLogic.pp_prog fmt genv in
  let () = Format.pp_print_flush fmt () in
  let () = close_out ch in
  ()
