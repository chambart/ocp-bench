open BenchCore

let rundir = ref None

let batch_files = ref []
let bench_files = ref []
let output_file = ref None

let spec =
  [
    "-b", Arg.String (fun s -> batch_files := s :: !batch_files), "batch file";
    "-l", Arg.String (fun s -> bench_files := s :: !bench_files), "bench file";
    "-d", Arg.String (fun s -> rundir := Some s), "default binary directory";
    "-o", Arg.String (fun s -> output_file := Some s), "result file";
  ]

let usage = "launch bencher"

let () = Arg.parse spec (fun _ -> ()) usage

let command_line = {
  rundir = !rundir;
}

let batch_files, bench_files =
  match !batch_files, !bench_files with
  | [], _
  | _, [] ->
     Arg.usage spec usage;
     exit 1
  | batch, bench -> batch, bench

let batch file =
  let l = Lexing.from_channel (open_in file) in
  let s = Yojson.init_lexer ~fname:file () in
  BenchFiles_j.read_batch s l

let bench_list file =
  let l = Lexing.from_channel (open_in file) in
  let s = Yojson.init_lexer ~fname:file () in
  BenchFiles_j.read_bench_list s l

let id =
  let c = ref (-1) in
  fun () -> incr c; !c
let batchs = List.map batch batch_files
let benchs = List.map (fun b -> id (),b)
                      (List.flatten (List.map bench_list bench_files))

let () = List.iter (fun ba -> Printf.printf "%s\n%!" (BenchFiles_j.string_of_batch ba)) batchs
let () = Printf.printf "%s\n%!" (BenchFiles_j.string_of_bench_list (List.map snd benchs))

let rec filter_map f = function
  | [] -> []
  | t::q -> match f t with
           | None -> filter_map f q
           | Some v -> v :: (filter_map f q)

let filter_bench = function
  | (id,Runner ({ kind = Bench _ } as b)) -> Some (id,b)
  | _ -> None

let bench_runs batch bench_list =
  lwt l = Lwt_list.map_s
    (fun (id,bench) -> load_run ~command_line batch bench >|= List.map (fun r -> id,r))
    bench_list in
  Lwt.return (filter_map filter_bench (List.flatten l))

open BenchFiles_t

let output result =
  match !output_file with
  | None -> Lwt.return ()
  | Some output_file ->
     Lwt_io.with_file ~mode:Lwt_io.output output_file
       (fun oc ->
        let s = BenchFiles_j.string_of_batch_results result in
        Lwt_io.write oc s)

let main () =
  let aux_runner (id,runner) =
    lwt state = loop_launch runner in
    let result = result_of_bench_state runner state in
    let () = Printf.printf "%s\n%!" (BenchFiles_j.string_of_bench_result result) in
    Lwt.return (id,result)
  in
  let aux_batch (runners,batch) =
    lwt r = Lwt_list.map_s aux_runner runners in
    Lwt.return { batch; results = r }
  in
  lwt batch_runners =
    Lwt_list.map_s (fun batch ->
        lwt runners = bench_runs batch benchs in
        Lwt.return (runners,batch))
      batchs
  in
  lwt results = Lwt_list.map_s aux_batch batch_runners in
  let result = { results_benchs = benchs; results_batchs = results; } in
  output result

let () = Lwt_unix.run (main ())
