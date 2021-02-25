open Lib

(* Print stack trace *)
let _ = Printexc.record_backtrace true


(**************************************************
 Utilities *)

let rec all_combinations ?( acc = [] ) a = function
	| hd :: xs -> all_combinations ~acc:( ( List.map ( fun x -> ( hd, x ) ) a ) @ acc ) a xs
	| [] -> acc

let shuffle ls =
	let rb = List.map ( fun c -> ( Random.bits (), c ) ) ls in
	let sorted = List.sort compare rb in
	List.map snd sorted

let round d x =
	if x -. ( Float.round x ) = 0. then x else
	let m = 10. ** ( float d ) in
	( Float.floor ( ( x *. m ) +. 0.5 ) ) /. m

let mean_of_ints len ls = ( List.fold_left (+) 0 ls |> float ) /. ( float len ) |> round 4
let mean_of_floats len ls = ( List.fold_left (+.) 0. ls ) /. ( float len ) |> round 4

let percentile q ls = (* ls is sorted *)
    let n = List.length ls |> float_of_int in
    let i = if q = 100. then int_of_float n - 1 else q /. 100. *. n |> int_of_float in
    List.nth ls i

let random_from ls = List.length ls |> Random.int |> List.nth ls

let random_subset n ls = shuffle ls |> BatList.take n


(**************************************************
 Variables *)

let min_gini, attempts = 0.4, 10
let testset_rows_amount = 50 (* n *)
let permutations_amount = 10
let random_searches_amount = 50

let test_m = List.init ( testset_rows_amount ) ( fun i -> i + 1 )
let test_b = List.init ( testset_rows_amount ) ( fun i -> i + 1 )

(* These are the parameters agains which we'll test the trees *)
let test_parameters = all_combinations test_m test_b


(**************************************************
 Random search *)

let random_search ds csv =

	let feature_index = List.nth csv 0 |> List.length |> Random.int in

	let feature_map = List.nth ds#feature_maps feature_index in

	random_from csv |> feature_map |> ds#search feature_index


(**************************************************
 Build arguments *)

let build_arguments table_size feature_amount =

	let feature_maps = List.init feature_amount ( fun i -> ( fun x -> List.nth x i ) ) in

	let hash_family_size = 100 in

	let hash_maps = List.init hash_family_size ( fun _ -> let n = Random.int 100 in ( fun x -> ( Hashtbl.seeded_hash n x ) mod table_size ) ) in

	feature_maps, hash_maps


(**************************************************
 Boxplot *)

let print_boxplot_data _ data =
    
    let stats = List.map ( fun ls -> List.nth ls 2 |> float_of_string ) data |> List.sort compare in

    let q1 = percentile 25. stats |> round 4 in
    let q2 = percentile 50. stats |> round 4 in
    let q3 = percentile 75. stats |> round 4 in

    Printf.printf {|
\addplot+[boxplot prepared={
    lower whisker=%f, lower quartile=%f,
    median=%f,
    upper quartile=%f, upper whisker=%f
}] coordinates {};
|} q1 q1 q2 q3 q3

(**************************************************
 Multifeature search *)

let run_multifeature_search_test source_file destination_dir =

	let testset = Csv.load source_file |> BatList.take 500 in

	let features_amount = List.nth testset 0 |> List.length in
	let features_indexes = List.init features_amount ( fun i -> i ) in

    for search_features_amount = 1 to features_amount do

        let destination = Printf.sprintf "%s/%d.out" destination_dir search_features_amount in        

        let data = List.map ( fun ( m, b ) ->

            let feature_maps, hash_maps = build_arguments b features_amount in
            let tree = new HHtree.hashTree m feature_maps hash_maps b min_gini attempts in
            let _ = List.map tree#insert testset in

            for _ = 0 to random_searches_amount do

                let element = random_from testset in
                let indexes = random_subset ( search_features_amount + 1 ) features_indexes in
                let values = List.map ( fun i -> element |> List.nth feature_maps i ) indexes in
                List.combine indexes values |> tree#multi_feature_search

            done;

            (* [ string_of_int m; string_of_int b;
              mean_of_ints random_searches_amount tree#search_costs |> string_of_float
            ] *)

            [ mean_of_ints random_searches_amount tree#search_costs |> string_of_float ]


        ) test_parameters in

        (* let _ = print_boxplot_data search_features_amount data in *)

        Csv.save destination ( ["data"] :: data ) (* [ "m"; "b"; "avg_access" ] destination data *)

    done


(**************************************************
 Main *)

let () =
    
    let _ = run_multifeature_search_test
		"../data/source/magic04.data"
		"../data/tree/multifeature/magic04" in

	(* let _ = run_multifeature_search_test
		"../data/source/cloud.data"
		"../data/tree/multifeature/cloud" in

    let _ = run_multifeature_search_test
		"../data/source/shelterdogs.data"
		"../data/tree/multifeature/shelterdogs" in *)

    ()
