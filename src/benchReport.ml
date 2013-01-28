let result_files = ref []
let reference_batch : string option ref = ref None
let report_measure = ref "user time"
let display_list = ref false

let spec =
  [
    "-r", Arg.String (fun s -> reference_batch := Some s), "reference";
    "-m", Arg.String (fun s -> report_measure := s), "displayed measure";
    "-l", Arg.Set display_list, "list recorded measures";
  ]

let usage = "show report"

let annon_fun s = result_files := s :: !result_files

let () = Arg.parse spec annon_fun usage

open Options_t

module MyMap(O:Map.OrderedType) = struct
  include Map.Make(O)
  let of_list l = List.fold_left (fun m (id,v) -> add id v m) empty l
  let to_list = bindings
end

type id = int
module Id = struct
  type t = id
  let compare = (-)
end
module IdMap = MyMap(Id)

module Batch : sig
  type t = private int
  val compare : t -> t -> int
  val create : unit -> t
end = struct
  type t = int
  let compare = (-)
  let create =
    let c = ref (-1) in
    fun () -> incr c; !c
end
module BatchMap = MyMap(Batch)

module StringSet = Set.Make(String)

type res = {
  benchs : (id * bench) list;
  batch_map : batch BatchMap.t;
  res_map : (bench_result IdMap.t) BatchMap.t
}

let prepare_batch batch_results =
  let l =
    List.map (fun results_batch ->
              Batch.create (), results_batch.batch, IdMap.of_list results_batch.results)
             batch_results.results_batchs in
  let batch_map = BatchMap.of_list (List.map (fun (id,batch,_) -> (id,batch)) l) in
  let res_map = BatchMap.of_list (List.map (fun (id,_,r) -> (id,r)) l) in
  { benchs = batch_results.results_benchs; batch_map; res_map }

let bench_results res id name =
  let aux key map acc =
    try
      match IdMap.find id map with
      | `Success r ->
         begin
           let measure = List.find (fun { measure_name } -> measure_name = name)
                                   r.result_measures in
           (key,`Success measure) :: acc
         end
      | `Failure r -> (key, `Failure r) :: acc
    with Not_found ->
      acc
  in
  BatchMap.of_list (BatchMap.fold aux res.res_map [])

let normalize reference measures =
  let orderBatch (a,_) (b,_) =
    if a = b
    then 0
    else
      if a = reference
      then -1
      else
        if b = reference
        then 1
        else compare a b in
  let l = BatchMap.fold
            (fun k v acc ->
             match v with
             | `Success m -> (k,m)::acc
             | `Failure _ -> acc)
            measures [] in
  let l = List.sort orderBatch l in
  match l with
  | [] -> BatchMap.empty
  | (_,t)::q ->
     let aux = function
       | `Success { measure_mean; measure_standard_deviation } ->
          `Success (measure_mean /. t.measure_mean,
                    measure_standard_deviation /. t.measure_mean)
       | `Failure _ as r -> r in
     BatchMap.map aux measures

let invert measure_name res =
  let aux (bench_id, _) = bench_id, bench_results res bench_id measure_name in
  IdMap.of_list (List.map aux res.benchs)

let load_result file =
  let l = Lexing.from_channel (open_in file) in
  let s = Yojson.init_lexer ~fname:file () in
  Options_j.read_batch_results s l

let get_reference map =
  let min_bind = fst (BatchMap.min_binding map) in
  match !reference_batch with
  | None -> min_bind
  | Some name ->
     let v = BatchMap.fold (fun k v acc ->
       if v.batch_name = name
       then Some k
       else acc) map None in
     match v with
     | None -> min_bind
     | Some v -> v

let (<|) f g x = f (g x)

let equal_name_length l =
  let l = List.map (fun (id, x) -> id, x.bench_name) l in
  let len = List.map (String.length <| snd) l in
  let max_len = List.fold_left max 0 len in
  List.map (fun (id, s) -> id, s ^ (String.make (max_len - (String.length s)) ' ')) l

let measure_names results =
  let set = ref StringSet.empty in
  List.iter (fun batch_res ->
    List.iter (fun (_, bench_result) ->
      match bench_result with
      | `Failure _ -> ()
      | `Success { result_measures } ->
         List.iter ( fun { measure_name } ->
           if not (StringSet.mem measure_name !set)
           then set := StringSet.add measure_name !set ) result_measures)
              batch_res.results)
            results.results_batchs;
  StringSet.elements !set

let main () =
  let results =
    match !result_files with
    | [r] -> load_result r
    | _ ->
       Arg.usage spec usage;
       exit 1 in
  if !display_list
  then begin
    List.iter print_endline (measure_names results)
    end
  else begin
    let res = prepare_batch results in
    let map = invert !report_measure res in
    let reference_batch = get_reference res.batch_map in
    let norm = IdMap.map (normalize reference_batch) map in
    BatchMap.iter (fun k batch -> Printf.printf "%s: %i " batch.batch_name (k:Batch.t:>int)) res.batch_map;
    print_newline ();
    let print (k:Batch.t) = function
      | `Success (m,et) -> Printf.printf "%i: %.3f (%.3f) " (k:>int) m et
      | `Failure _ -> Printf.printf "%i: ____ ____ " (k:>int)
    in
    let bench_names = equal_name_length res.benchs in
    IdMap.iter (fun bench_id v ->
                let bench_name = List.assoc bench_id bench_names in
                Printf.printf "%s: " bench_name;
                BatchMap.iter print v;
                print_newline ()) norm
    end

let () = main ()
