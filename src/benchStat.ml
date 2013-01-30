open BenchCommon
open BenchFiles_t

let epanechnikov x =
  let x2 = x *. x in
  if x2 >= 1.
  then 0.
  else 0.75 *. (1. -. x *. x)

let pi = 4. *. atan 1.
let gaussian_cst = 1. /. sqrt (2. *. pi)

let gaussian x =
  gaussian_cst *. exp (-. 0.5 *. x *. x )

let estimage_gaussian_bandwidth a =
  let std_dev = standard_deviation a in
  let n = float (Array.length a) in
  ((4. /. 3.) *. (std_dev ** 5.) /. n) ** (1. /. 5.)

let kde kernel a h x =
  let sum = ref 0. in
  let len = Array.length a in
  assert(len > 0);
  for i = 0 to len - 1 do
    sum := !sum +. kernel ( (x -. a.(i)) /. h );
  done;
  !sum /. ( h *. float len )

let highest_point a =
  let h = estimage_gaussian_bandwidth a in
  Array.fold_left (fun acc p -> max (kde gaussian a h p) acc) 0. a

let sampling_points n min_range max_range a =
  let d = (max_range -. min_range) /. (float (n-1)) in
  let other = Array.init n (fun i -> min_range +. d *. (float i)) in
  let array = Array.concat [other;a] in
  Array.sort compare array;
  array

let plot vp ?(kernel=epanechnikov) ?(h=1.) a =
  let f x = kde kernel a h x in
  Archimedes.fx vp f 0. 10.

let add_result vp y_range min_range max_range a =
  let h = estimage_gaussian_bandwidth a in
  Printf.printf "plot [%f,%f] h: %f\n" min_range max_range h;
  let f x =
    let v = kde gaussian a h x in
    v
  in
  let x_array = sampling_points 1000 min_range max_range a in
  let y_array = Array.map f x_array in
(*
  Archimedes.fx vp f ~n:1000
    ~cost:Archimedes.Sampler.cost_angle_dist
    min_range max_range
*)
  Archimedes.Array.xy ~style:`Lines vp x_array y_array;
  let y_array' = Array.map (fun _ -> y_range *. 0.025) a in
  Archimedes.Array.xy ~style:`Impulses vp a y_array'


let bound f b l = List.fold_left (fun acc a -> Array.fold_left f acc a) b l

let show_plot vp label results =
  let min_val = bound min max_float results in
  let max_val = bound max min_float results in
  Printf.printf "bounds [%f,%f]\n%!" min_val max_val;
  let min_range =
    min_val -. ((max_val -. min_val) *. 0.5) in
  let max_range =
    max_val +. ((max_val -. min_val) *. 0.5) in
  let max_y_val = List.fold_left
      (fun acc a -> max acc (highest_point a)) 0. results in
  let max_y_range = max_y_val *. 1.1 in
  Archimedes.xrange vp min_range max_range;
  Archimedes.yrange vp 0. max_y_range;
  Archimedes.Viewport.xlabel vp label;
  Archimedes.Viewport.ylabel vp "Probability";
  Archimedes.Axes.cross vp;
  List.iter (add_result vp max_y_range min_range max_range) results;
  Archimedes.close vp

let get_table res batch_name bench_name measure =
  let batch_id =
    try StringMap.find batch_name res.batch_id
    with Not_found ->
      Printf.eprintf "Batch %s not found\n%!" batch_name;
      exit 1 in
  let bench_id =
    try StringMap.find bench_name res.bench_id
    with Not_found ->
      Printf.eprintf "Bench %s not found\n%!" bench_name;
      exit 1 in
  let batch_result = BatchMap.find batch_id res.res_map in
  let bench_result =
    try IdMap.find bench_id batch_result
    with Not_found ->
      Printf.eprintf "Bench %s not found for batch %s\n%!" bench_name batch_name;
      exit 1 in
  match bench_result with
  | `Failure error ->
    Printf.eprintf "Bench %s for batch %s failed with error %s\n%!"
      bench_name batch_name error;
    exit 1
  | `Success res ->
    assert(res.result_name = bench_name);
    if res.result_count <= 2
    then begin
      Printf.eprintf "Too few results (%i) for bench %s in batch %s\n%!"
        res.result_count bench_name batch_name;
      exit 1
    end;
    let measure = try
      List.find (fun m -> m.measure_name = measure) res.result_measures
    with Not_found ->
      Printf.eprintf "No measure %s recorded for bench %s in batch %s\n%!"
        measure bench_name batch_name;
      exit 1
    in
    Array.of_list measure.measure_results

let run_plot res measure choice =
  let tables = List.map (fun (bench_name,batch_name) ->
      get_table res batch_name bench_name measure) choice in
  let vp = Archimedes.init [] in
  show_plot vp measure tables
