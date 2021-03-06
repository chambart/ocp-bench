let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let section = Lwt_log.Section.make "benchmark"

type command = string * string array
type file = string

type test_expected = file

type test
type bench

type measure_conditions = {
  warm_up_time : float;
  min_measures : int;
  max_confidence : float;
  max_duration : float;
}

type perf_options = {
  events : string list;
  perf_output : file;
}

type gc_output =
  | Fixed_file of file
  | Env_var of string

type gc_options = {
  gc_output : gc_output;
}

type time_info =
  | Real_time
  | User_time
  | Sys_time

type measure =
  | Perf_event of string
  | Time_info of time_info
  | Gc_info of string

type bench_option = {
  measure_conditions : measure_conditions;
  perf_options : perf_options option;
  gc_options : gc_options option;
  criterion : measure;
}

type _ run_kind =
  | Test : test_expected -> test run_kind
  | Bench : bench_option -> bench run_kind

type 'a run = {
  name : string;
  command : command;
  env : (string * string) list option;
  stdin : file option;
  stdout : file option;
  stderr : file option;
  kind : 'a run_kind;
}

let (>=?) f = function
  | None -> None
  | Some x -> let r = f x in Some r

let (>>?) f = function
  | None -> ()
  | Some x -> f x

let in_flags, in_mode =
  [Unix.O_RDONLY], 0o0
let out_flags, out_mode =
  [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC], 0o666

let force_close fd =
  try Unix.close fd with
  | Unix.Unix_error _ -> ()

let with_files_in_out_out file_in file_out1 file_out2 f =
  let openfile flags mode filename =
    Unix.openfile filename flags mode in
  let fd_in = (openfile in_flags in_mode) >=? file_in in
  try_lwt
    let fd_out1 = (openfile out_flags out_mode) >=? file_out1 in
    try_lwt
      let fd_out2 = (openfile out_flags out_mode) >=? file_out2 in
      try_lwt
        f fd_in fd_out1 fd_out2
      finally Lwt.return (force_close >>? fd_out2)
    finally Lwt.return (force_close >>? fd_out1)
  finally Lwt.return (force_close >>? fd_in)

let redir_opt = function
  | None -> `Dev_null
  | Some fd -> `FD_move fd

let make_env = function
  | None -> None
  | Some l ->
     let l' = List.map (fun (name,value) -> name ^ "=" ^ value) l in
    Some (Array.of_list l')

let print_command (executable, options) =
  let components = match Array.to_list options with
    | [] -> [executable]
    | t::q -> executable::q in
  String.concat " " components

let run_process ?src_stdin ?dest_stdout ?dest_stderr ?env command =
  lwt () = Lwt_log.info_f ~section "run command %s" (print_command command) in
  with_files_in_out_out src_stdin dest_stdout dest_stderr
    (fun fd_stdin fd_stdout fd_stderr ->
     Lwt_process.with_process_none
       ?env:(make_env env)
       ~stdin:(redir_opt fd_stdin)
       ~stdout:(redir_opt fd_stdout)
       ~stderr:(redir_opt fd_stderr)
       command
       (fun process ->
        lwt status = process#status in
        lwt rusage = process#rusage in
        Lwt.return (status, rusage)))

type 'a bench_v = (measure * 'a) list

type bench_r = float bench_v

type runner = Runner : 'a run -> runner

type error =
  | Launching_command
  | Return_code of int
  | Different_content
  | Killed of int
  | Stopped of int
  | No_output_redirection
  | Termination_criterion_not_recorded of measure
  | File_not_present of file
  | Uncaught_exception of exn

exception Err of runner * error

let print_error (runner,error) =
  match error with
  | Launching_command -> "error launching command"
  | Return_code i -> Printf.sprintf "return code: %i" i
  | Different_content -> "different content"
  | Killed i -> Printf.sprintf "killed with signal: %i" i
  | Stopped i -> Printf.sprintf "stopped with signal: %i" i
  | No_output_redirection -> "no output redirection"
  | Termination_criterion_not_recorded _ -> "termination criterion not recorded"
  | File_not_present file -> Printf.sprintf "file not present: %s" file
  | Uncaught_exception exn -> Printf.sprintf "uncaught exception: %s" (Printexc.to_string exn)

let run runner =
  lwt status, rusage = run_process
    ?src_stdin:runner.stdin
    ?dest_stdout:runner.stdout
    ?dest_stderr:runner.stderr
    ?env:runner.env
    runner.command in
  match status with
  | Lwt_unix.WEXITED 0 ->
     Lwt.return rusage
  | Lwt_unix.WEXITED n ->
     if n = 127
     then raise_lwt (Err (Runner runner, Launching_command))
     else raise_lwt (Err (Runner runner, Return_code n))
  | Lwt_unix.WSIGNALED n ->
     raise_lwt (Err (Runner runner, Killed n))
  | Lwt_unix.WSTOPPED n ->
     raise_lwt (Err (Runner runner, Stopped n))

type _ run_result =
  | Test_result : test run_result
  | Bench_result : bench_r -> bench run_result

let diff_io runner file1 file2 =
  let count = 4096 in
  let rec aux () =
    lwt content1 = Lwt_io.read ~count file1 in
    lwt content2 = Lwt_io.read ~count file2 in
    match content1, content2 with
    | "", "" -> Lwt.return ()
    | "", _
    | _, "" -> raise_lwt (Err (Runner runner, Different_content))
    | _, _ ->
       if content1 = content2
       then aux ()
       else raise_lwt (Err (Runner runner, Different_content))
    in
    aux ()

let diff runner file1 file2 =
  Lwt_io.with_file ~mode:Lwt_io.input file1 (fun ic1 ->
    Lwt_io.with_file ~mode:Lwt_io.input file2 (fun ic2 ->
      diff_io runner ic1 ic2))

let launch_test : test run -> test run_result Lwt.t = fun runner ->
     let Test expected = runner.kind in
     lwt _ = run runner in
     match runner.stdout with
     | None ->
        Lwt_log.info ~section "no output redirection" >>
        raise_lwt (Err (Runner runner, No_output_redirection))
     | Some output ->
        lwt () = diff runner output expected in
        Lwt.return (Test_result)

let rec read_lines f acc ic =
  lwt line = Lwt_io.read_line_opt ic in
  match line with
  | None -> Lwt.return acc
  | Some line ->
     let acc =
       try (f line) :: acc with
       | Scanf.Scan_failure _
       | End_of_file -> acc
     in
     read_lines f acc ic

let parse_perf runner file =
  let parse_perf_line l = Scanf.sscanf l "%f , %s" (fun v s -> s,v) in
  try_lwt
    Lwt_io.with_file ~mode:Lwt_io.input file (read_lines parse_perf_line [])
  with
  | Unix.Unix_error(Unix.ENOENT, "open", f) ->
     raise_lwt (Err (Runner runner, File_not_present f))

let parse_gc runner file =
  let parse_gc_line l = Scanf.sscanf l "%s@: %f" (fun s v -> s,v) in
  try_lwt
    Lwt_io.with_file ~mode:Lwt_io.input file (read_lines parse_gc_line [])
  with
  | Unix.Unix_error(Unix.ENOENT, "open", f) ->
     raise_lwt (Err (Runner runner, File_not_present f))

let gc_out_file : unit -> file = fun _ -> "run.gc_stat"

let launch_bench : bench run -> bench run_result Lwt.t = fun runner ->
  let Bench options = runner.kind in
  let t1 = Unix.gettimeofday () in
  lwt result = run runner in
  let t2 = Unix.gettimeofday () in
  let res = [ Time_info Real_time, t2 -. t1;
              Time_info User_time, result.Lwt_unix.ru_utime;
              Time_info Sys_time, result.Lwt_unix.ru_stime; ] in
  lwt res = match options.perf_options with
    | None -> Lwt.return res
    | Some { perf_output } ->
       lwt perf_result = parse_perf runner perf_output in
       Lwt.return (( List.map (fun (n,v) -> Perf_event n, v) perf_result ) @ res)
  in
  lwt res = match options.gc_options with
    | None -> Lwt.return res
    | Some { gc_output } ->
      let out_file = match gc_output with
        | Fixed_file f -> f
        | Env_var _ -> gc_out_file () in
      lwt gc_result = parse_gc runner out_file in
      Lwt.return (( List.map (fun (n,v) -> Gc_info n, v) gc_result ) @ res)
  in
  Lwt.return (Bench_result res)

let launch : type kind . kind run -> kind run_result Lwt.t = fun runner ->
  match runner.kind with
  | Test _ -> launch_test runner
  | Bench _ -> launch_bench runner


(****)

open BenchCommon

let mean_with_confidence a =
  let (m, v) = mean_variance a in
  let l = Array.length a in
  (m, sqrt v /. sqrt (float l) *. tinv99 (l - 1))

let need_more c l =
  let a = Array.of_list l in
  let (m, i) = mean_with_confidence a in
  let n = Array.length a in
  Lwt_log.ign_info_f ~section "==> %f +/- %f / %f %d" m i (i /. m) n;
  n < c.min_measures || (i /. m > c.max_confidence /. 2.)

(****)

type float_acc = float list

type bench_s = float_acc bench_v

type _ state =
  | Test_state : test state
  | Bench_state : bench_s -> bench state

type 'a loop_state = {
  count : int;
  state : 'a state;
}

let init_bench_state r = List.map (fun (n,v) -> (n,[v])) r

let add_bench_state res st =
  List.map2 (fun (n1,r) (n2,l) ->
             assert (n1 = n2);
             n1,r::l) res st

let init_loop_state : type kind . kind run_kind ->
  kind run_result -> kind loop_state = fun kind result ->
  let init state =
    { count = 1;
      state = state } in
  match result with
    | Test_result -> init Test_state
    | Bench_result res ->
       init (Bench_state (init_bench_state res))

let update_state : type kind . kind loop_state ->
  kind run_result -> kind loop_state = fun state result ->
  { count = state.count + 1;
    state = match state.state with
      | Test_state -> assert false
      | Bench_state st ->
         let Bench_result res = result in
         Bench_state (add_bench_state res st) }

let continue_loop : type kind . kind run ->
  kind loop_state -> bool = fun runner state ->
  match state.state with
  | Test_state -> false
  | Bench_state bench_state ->
     let Bench { measure_conditions =
                   { min_measures;
                     max_confidence;
                     max_duration; } as c;
                 criterion; } = runner.kind in
     let crit =
       try
         List.assoc criterion bench_state
       with
       | Not_found ->
          raise (Err (Runner runner,
            Termination_criterion_not_recorded criterion))
     in
     min_measures >= state.count || need_more c crit

type 'a result =
  | Error of runner * error
  | Success of 'a

let warm_up : type kind. kind run -> unit Lwt.t = fun runner ->
  match runner.kind with
  | Test _ -> Lwt.return ()
  | Bench { measure_conditions = { warm_up_time } } ->
     lwt () = Lwt_log.info_f ~section "warm_up %f" warm_up_time in
     let init_time = Unix.gettimeofday () in
     let i = ref 0 in
     let rec aux () =
       let current_time = Unix.gettimeofday () in
       if current_time -. init_time >= warm_up_time
       then Lwt.return ()
       else
         lwt _ = run runner in
         incr i;
         aux ()
     in
     lwt () = aux () in
     let current_time = Unix.gettimeofday () in
     Lwt_log.info_f ~section "warm_up done: %i times in %f seconds" !i (current_time -. init_time)

let loop_launch' : type kind . kind run -> kind loop_state Lwt.t = fun runner ->
  lwt () = warm_up runner in
  lwt first_result = launch runner in
  let init = init_loop_state runner.kind first_result in
  let rec loop state =
    if continue_loop runner state
    then
      lwt result = launch runner in
      let state = update_state state result in
      loop state
    else
      lwt () = Lwt_log.info_f ~section "loop end" in
      Lwt.return state
  in
  loop init

let loop_launch runner =
  try_lwt
    lwt res = loop_launch' runner in
    Lwt.return (Success res)
  with
  | Err (r,e) ->
     Lwt.return (Error (r,e))
  | exn ->
     Lwt.return (Error (Runner runner,Uncaught_exception exn))

(****************)

let res_mean l =
  List.map (fun (n,v) -> (n,mean (Array.of_list v))) l

let res_mean_variance l =
  List.map (fun (n,v) -> (n,mean_variance (Array.of_list v))) l

(*******************)
(* Simple launcher *)
(*******************)

let measure_string = function
  | Perf_event s -> s
  | Time_info v ->
     begin match v with
     | Real_time -> "real time"
     | User_time -> "user time"
     | Sys_time -> "sys time" end
  | Gc_info s -> s

let print_res { state = Bench_state res } =
  let res = res_mean_variance res in
  let l = List.map (fun (n,(m,v)) ->
    Printf.sprintf "%s: %f [%f]\n" (measure_string n) m v) res in
  Lwt_list.iter_s Lwt_io.print l

let launch_one f v =
  try_lwt
    match_lwt loop_launch v with
    | Success x -> f x
    | Error (r,e) ->
       Lwt_log.error_f "%s\n" (print_error (r,e))
  with
  | Err (r,e) as exn ->
     Lwt_log.error_f ~exn "%s\n" (print_error (r,e))
  | exn ->
     Lwt_log.error_f ~exn "uncaught exception\n"

(********************)

let section = Lwt_log.Section.make "load"

type load_error =
  | No_opam_alias of string
  | File_not_found of string
  | Not_regular_file of string

exception Load_err of load_error

let print_load_error error =
  match error with
  | No_opam_alias alias -> Printf.sprintf "OPAM alias unknown: %s" alias
  | File_not_found path -> Printf.sprintf "File not found: %s" path
  | Not_regular_file path -> Printf.sprintf "%s is not a regular file" path

type load_result =
  | Load_success of runner list
  | Load_error of load_error

(********************)
(* OPAM integration *)
(********************)

let opam_root = OpamPath.default ()
let opam_aliases = OpamFile.Aliases.safe_read (OpamPath.aliases opam_root)
let path_table = Hashtbl.create 1
let opam_alias_bin_path alias_name =
  let aux alias_name =
    let alias = OpamSwitch.of_string alias_name in
    if OpamSwitch.Map.mem alias opam_aliases
    then
      let path = OpamPath.Switch.bin opam_root alias in
      Some (OpamFilename.Dir.to_string path)
    else
      None in
  try Hashtbl.find path_table alias_name
  with Not_found ->
    let r = aux alias_name in
    Hashtbl.add path_table alias_name r;
    r

let exec_path ?rundir alias subpath =
  let path =
    if not (Filename.is_relative subpath)
    then
      subpath
    else
      let base_path = match alias with
        | Some alias_name ->
           begin
             match opam_alias_bin_path alias_name with
             | None -> raise (Load_err (No_opam_alias alias_name))
             | Some path -> path
           end
        | None ->
           match rundir with
           | None ->
              Unix.getcwd ()
           | Some d ->
              if Filename.is_relative d
              then Filename.concat (Unix.getcwd ()) d
              else d
      in
      Filename.concat base_path subpath in
  try
    let stat = Unix.stat path in
    (match stat.Unix.st_kind with
     | Unix.S_REG -> ()
     | _ -> raise (Load_err (Not_regular_file path)));
    path
  with Unix.Unix_error (Unix.ENOENT, "stat", _) ->
    raise (Load_err (File_not_found path))

(****************)

let normal = {
  warm_up_time = 1.;
  min_measures = 10;
  max_confidence = 0.03;
  max_duration = 1200.;
}

let fast = {
  warm_up_time = 1.;
  min_measures = 5;
  max_confidence = 0.15;
  max_duration = 30.;
}

let ffast = {
  warm_up_time = 1.;
  min_measures = 2;
  max_confidence = 42.;
  max_duration = 30.;
}

let gc_opt = {
  warm_up_time = 0.;
  min_measures = 2;
  max_confidence = 0.1;
  max_duration = 30.;
}

open BenchFiles_t

let perf_available () = true

let perf_options bench batch =
  if perf_available ()
  then
    match batch.batch_perf_events with
    | [] -> None
    | _ ->
       Some { perf_output = batch.batch_name ^ "_" ^ bench.bench_name ^ ".perf_result";
              events = batch.batch_perf_events }
  else
    None

let criterion_of_string = function
  | "real_time" -> Time_info Real_time
  | "user_time" -> Time_info User_time
  | "sys_time" -> Time_info Sys_time
  | s -> Perf_event s

let load_gc_options bench =
  match bench.bench_gc_output, bench.bench_gc_env_file with
  | _, Some e ->
    Some { gc_output = Env_var e }
  | Some f, None ->
    Some { gc_output = Fixed_file f }
  | None, None ->
    None

let load_bench_option batch bench =
  match batch.batch_criterion with
  | None -> None
  | Some criterion ->
     Some
       { measure_conditions = fast;
         perf_options = perf_options bench batch;
         gc_options = load_gc_options bench;
         criterion = criterion_of_string criterion }

let load_test_options batch bench = None

let perf_command options (exec_path, command) =
  let command = exec_path :: (List.tl (Array.to_list command)) in (* command does not contains the full path *)
  let command = String.concat " " command in
  let events = List.flatten (List.map (fun s -> ["-e";s]) options.events) in
  "/usr/bin/perf",
  Array.of_list
    ([ "perf"; "stat"; "-x"; ","; "-o"; options.perf_output ] @
     events @
     [ "--"; command ])

type command_line = {
  rundir : string option;
}

let default_command_line = {
  rundir = None;
}

let split_env_line l = Scanf.sscanf l "%s@=%s" (fun s v -> s,v)
let environment = Array.to_list (Array.map split_env_line (Unix.environment ()))
let add_env (key,v) env =
  (key,v) :: (List.remove_assoc key env)
let add_if_not ((key,_) as v) = function
  | None -> Some [v]
  | Some env ->
     Some (if List.mem_assoc key env
           then env
           else v :: env)

let load_run' ?(command_line=default_command_line) batch bench =
  let test_options = load_test_options batch bench in
  let bench_options = load_bench_option batch bench in
  let make ?(env=batch.batch_env) command kind =
    { name = bench.bench_name;
      command = command;
      env;
    (* TODO: populate *)
      stdin = None;
      stdout = None;
      stderr = None;
      kind = kind }
  in
  let exec = exec_path ?rundir:command_line.rundir batch.batch_opam_alias bench.bench_executable in
  let basic_command = exec,
                      Array.of_list (bench.bench_executable :: bench.bench_args) in
  let test = match test_options with
    | None -> []
    | Some opt ->
       [Runner (make basic_command (Test opt))] in
  let bench = match bench_options with
    | None -> []
    | Some opt ->
      let env = match opt with
        | { gc_options = Some { gc_output = Env_var var } } ->
          let gc_out_var = var,gc_out_file () in
          begin match batch.batch_env with
          | None -> Some [gc_out_var]
          | Some l -> Some (gc_out_var :: l) end
        | _ -> batch.batch_env in
      let command, env =
        match opt.perf_options with
        | None -> basic_command, env
        | Some perf_options ->
           let env = add_if_not ("PATH", Sys.getenv "PATH") env in
           perf_command perf_options basic_command, env in
      [Runner (make ~env command (Bench opt))] in
  test @ bench

let load_run ?command_line batch bench =
  try_lwt
    Lwt.return (load_run' ?command_line batch bench)
  with Load_err error ->
    Lwt_log.error ~section (print_load_error error) >>
    Lwt.return []

(*******)

let measure_res (measure,measure_results) =
  let measure_mean, measure_variance = mean_variance (Array.of_list measure_results) in
  { measure_name = measure_string measure;
    measure_mean;
    measure_variance;
    measure_standard_deviation = sqrt measure_variance;
    measure_results }

let result_of_bench_state run state : bench_result =
  match state with
  | Error (r,e) -> `Failure (print_error (r,e))
  | Success { count; state = Bench_state res } ->
     `Success
      { result_name = run.name;
        result_count = count;
        result_measures = List.map measure_res res }
