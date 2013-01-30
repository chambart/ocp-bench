open BenchFiles_t

let mean a =
  let s = ref 0. in
  for i = 0 to Array.length a - 1 do
    s := !s +. a.(i)
  done;
  !s /. float (Array.length a)

let mean_variance a =
  let m = mean a in
  let s = ref 0. in
  for i = 0 to Array.length a - 1 do
    let d = a.(i) -. m in
    s := !s +. d *. d
  done;
  (m, !s /. float (Array.length a - 1))

let standard_deviation a =
  let m = mean a in
  let s = ref 0. in
  for i = 0 to Array.length a - 1 do
    let d = a.(i) -. m in
    s := !s +. d *. d
  done;
  sqrt (!s /. float (Array.length a))

          (*90%    95%    98%    99%    99.5%  99.8%  99.9%*)
let tinv_table =
  [|(1,   [|6.314; 12.71; 31.82; 63.66; 127.3; 318.3; 636.6|]);
    (2,   [|2.920; 4.303; 6.965; 9.925; 14.09; 22.33; 31.60|]);
    (3,   [|2.353; 3.182; 4.541; 5.841; 7.453; 10.21; 12.92|]);
    (4,   [|2.132; 2.776; 3.747; 4.604; 5.598; 7.173; 8.610|]);
    (5,   [|2.015; 2.571; 3.365; 4.032; 4.773; 5.893; 6.869|]);
    (6,   [|1.943; 2.447; 3.143; 3.707; 4.317; 5.208; 5.959|]);
    (7,   [|1.895; 2.365; 2.998; 3.499; 4.029; 4.785; 5.408|]);
    (8,   [|1.860; 2.306; 2.896; 3.355; 3.833; 4.501; 5.041|]);
    (9,   [|1.833; 2.262; 2.821; 3.250; 3.690; 4.297; 4.781|]);
    (10,  [|1.812; 2.228; 2.764; 3.169; 3.581; 4.144; 4.587|]);
    (11,  [|1.796; 2.201; 2.718; 3.106; 3.497; 4.025; 4.437|]);
    (12,  [|1.782; 2.179; 2.681; 3.055; 3.428; 3.930; 4.318|]);
    (13,  [|1.771; 2.160; 2.650; 3.012; 3.372; 3.852; 4.221|]);
    (14,  [|1.761; 2.145; 2.624; 2.977; 3.326; 3.787; 4.140|]);
    (15,  [|1.753; 2.131; 2.602; 2.947; 3.286; 3.733; 4.073|]);
    (16,  [|1.746; 2.120; 2.583; 2.921; 3.252; 3.686; 4.015|]);
    (17,  [|1.740; 2.110; 2.567; 2.898; 3.222; 3.646; 3.965|]);
    (18,  [|1.734; 2.101; 2.552; 2.878; 3.197; 3.610; 3.922|]);
    (19,  [|1.729; 2.093; 2.539; 2.861; 3.174; 3.579; 3.883|]);
    (20,  [|1.725; 2.086; 2.528; 2.845; 3.153; 3.552; 3.850|]);
    (21,  [|1.721; 2.080; 2.518; 2.831; 3.135; 3.527; 3.819|]);
    (22,  [|1.717; 2.074; 2.508; 2.819; 3.119; 3.505; 3.792|]);
    (23,  [|1.714; 2.069; 2.500; 2.807; 3.104; 3.485; 3.767|]);
    (24,  [|1.711; 2.064; 2.492; 2.797; 3.091; 3.467; 3.745|]);
    (25,  [|1.708; 2.060; 2.485; 2.787; 3.078; 3.450; 3.725|]);
    (26,  [|1.706; 2.056; 2.479; 2.779; 3.067; 3.435; 3.707|]);
    (27,  [|1.703; 2.052; 2.473; 2.771; 3.057; 3.421; 3.690|]);
    (28,  [|1.701; 2.048; 2.467; 2.763; 3.047; 3.408; 3.674|]);
    (29,  [|1.699; 2.045; 2.462; 2.756; 3.038; 3.396; 3.659|]);
    (30,  [|1.697; 2.042; 2.457; 2.750; 3.030; 3.385; 3.646|]);
    (40,  [|1.684; 2.021; 2.423; 2.704; 2.971; 3.307; 3.551|]);
    (50,  [|1.676; 2.009; 2.403; 2.678; 2.937; 3.261; 3.496|]);
    (60,  [|1.671; 2.000; 2.390; 2.660; 2.915; 3.232; 3.460|]);
    (80,  [|1.664; 1.990; 2.374; 2.639; 2.887; 3.195; 3.416|]);
    (100, [|1.660; 1.984; 2.364; 2.626; 2.871; 3.174; 3.390|]);
    (120, [|1.658; 1.980; 2.358; 2.617; 2.860; 3.160; 3.373|])|]

let tinv_row n =
  let i = ref 1 in
  let l = Array.length tinv_table in
  while !i < l && fst tinv_table.(!i) <= n do incr i done;
  snd tinv_table.(!i - 1)

let tinv95 n = (tinv_row n).(1)
let tinv99 n = (tinv_row n).(3)


(*****************)

module MyMap(O:Map.OrderedType) = struct
  include Map.Make(O)
  let of_list l = List.fold_left (fun m (id,v) -> add id v m) empty l
  let to_list = bindings
end

type id = int
let new_id =
  let r = ref 0 in
  (fun () -> incr r; !r)

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
module StringMap = MyMap(String)

type res = {
  benchs : (id * bench) list;
  bench_id : id StringMap.t;
  batch_map : batch BatchMap.t;
  batch_id : Batch.t StringMap.t;
  res_map : (bench_result IdMap.t) BatchMap.t;
}

let add_benchs map l =
  let aux map (id,bench) =
    if StringMap.mem bench.bench_name map
    then map
    else
      let id' = new_id () in
      StringMap.add bench.bench_name (id',bench) map
  in
  List.fold_left aux map l

let renumber_map map l =
  let aux renum_map (id,bench) =
    let (id',_) = StringMap.find bench.bench_name map in
    IdMap.add id id' renum_map in
  List.fold_left aux IdMap.empty l

let renumber_batch_results renum_map batch =
  let aux (id,result) = IdMap.find id renum_map ,result in
  List.map (fun r -> { r with results = List.map aux r.results } )
    batch.results_batchs

let fuse_results results =
  let bench_map =
    List.fold_left (fun map r -> add_benchs map r.results_benchs)
      StringMap.empty results in
  let batchs =
    List.map (fun r ->
      let renum_map = renumber_map bench_map r.results_benchs in
      renumber_batch_results renum_map r)
      results in
  let benchs =
    StringMap.fold (fun _ (id,bench) l -> (id,bench)::l) bench_map [] in
  { results_benchs = benchs;
    results_batchs = List.concat batchs; }
