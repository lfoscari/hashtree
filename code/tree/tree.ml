open Lib

(* Is this file the adaptive and strict
 * hh-tree structure will be tested *)

(* Print stack trace *)
let _ = Printexc.record_backtrace true


(**************************************************
 Utilities *)

exception Invalid_input

let rec compare_lists a b =
	match a, b with
	| xa :: xsa, xb :: xsb when xa = xb -> compare_lists xsa xsb
	| xa :: _, xb :: _ when xa > xb -> 1
	| xa :: _, xb :: _ when xa < xb -> -1
	| [], [] -> 0
	| _ -> raise Invalid_input

let rec all_combinations ?( acc = [] ) a = function
	| hd :: xs -> all_combinations ~acc:( ( List.map ( fun x -> ( hd, x ) ) a ) @ acc ) a xs
	| [] -> acc

let round d x =
	if x -. ( Float.round x ) = 0. then x else
	let m = 10. ** ( float d ) in
	( Float.floor ( ( x *. m ) +. 0.5 ) ) /. m

let mean_of_ints len ls = ( List.fold_left (+) 0 ls |> float ) /. ( float len ) |> round 4
let mean_of_floats len ls = ( List.fold_left (+.) 0. ls ) /. ( float len ) |> round 4

let std_of_floats len mean ls =
	let len_f = float len in
	( List.fold_left ( fun var x -> var +. ( x -. mean ) ** 2. ) 0. ls )
		/. len_f |> sqrt |> round 4


(**************************************************
 Testing parameters *)

let testset_rows_amount = 50 (* n *)
let permutations_amount = 10
let random_searches_amount = 50

(* strict only *)
let min_gini, attempts = 0.4, 10

(* Avoid the obvious values b = m = 1 and b = m = n *)
let test_m = List.init ( testset_rows_amount ) ( fun i -> i + 1 )
let test_b = List.init ( testset_rows_amount ) ( fun i -> i + 1 )

(* These are the parameters agains which we'll test the trees *)
let test_parameters = all_combinations test_m test_b

let load_csv dataset_filename map =

	let testset_csv = Csv.load dataset_filename in

	let testset_gen =
		BatList.take testset_rows_amount testset_csv
		|> List.map map
		|> Permutations.stream_of_permutations in

	testset_gen, Csv.columns testset_csv


(**************************************************
 Build HH-trees *)

let build_arguments table_size feature_amount =
	(* Needed arguments to build both trees *)

	let feature_maps = List.init feature_amount ( fun i -> ( fun x -> List.nth x i ) ) in

	let hash_family_size = 100 in

	let hash_maps = List.init hash_family_size ( fun _ -> let n = Random.int 100 in ( fun x -> ( Hashtbl.seeded_hash n x ) mod table_size ) ) in

	feature_maps, hash_maps

let build_tree_adaptive bucket_size feature_maps hash_maps table_size test_set =

	let tree = new HHtreeAdaptive.hashTree bucket_size feature_maps hash_maps table_size in
	let _ = List.map tree#insert test_set in

	tree

let build_tree_strict bucket_size feature_maps hash_maps table_size test_set =

  let tree = new HHtreeStrict.hashTree bucket_size feature_maps hash_maps table_size min_gini attempts in
  let _ = List.map tree#insert test_set in

  tree


(**************************************************
 Random search *)

let random_search tree csv =

	let feature_index = List.nth csv 0 |> List.length |> Random.int in

	let feature_map = List.nth tree#feature_maps feature_index in

	let element = List.length csv |> Random.int |> List.nth csv in

	feature_map element |> tree#search feature_index


(**************************************************
 Run tests *)

let save_to_csv header filename test_results =

	let _ = print_string "Test results saved in "; print_endline filename in

	header :: test_results |> Csv.save filename

let get_metrics trees csvs =

	let depth_ls = List.map ( fun tree -> tree#depth |> float ) trees in
	let usage_ls = List.map ( fun tree -> tree#usage ) trees in
	let access_ls = List.mapi ( fun index tree ->
		List.init random_searches_amount ( fun _ -> List.nth csvs index |> random_search tree )
		|> mean_of_ints random_searches_amount
	) trees in
	let insertion_ls = List.map ( fun tree -> mean_of_ints testset_rows_amount tree#insertion_costs ) trees in

	let avg_depth = mean_of_floats permutations_amount depth_ls in
	let avg_usage = mean_of_floats permutations_amount usage_ls in

	let std_depth = std_of_floats permutations_amount avg_depth depth_ls in
	let std_usage = std_of_floats permutations_amount avg_usage usage_ls in

	let avg_access = mean_of_floats permutations_amount access_ls in
	let avg_insertion = mean_of_floats permutations_amount insertion_ls in

	[
		string_of_float avg_depth; string_of_float avg_usage;
		string_of_float std_depth; string_of_float std_usage;
		string_of_float avg_access;
		string_of_float avg_insertion
	]

let assemble_data m b testset_gen features_amount =

	let parameters = [ string_of_int m; string_of_int b ] in
	let csvs = Stream.npeek permutations_amount testset_gen in
	let feature_maps, hash_maps = build_arguments b features_amount in

	let adaptive = List.map ( build_tree_adaptive m feature_maps hash_maps b ) csvs in
	let strict = List.map ( build_tree_strict m feature_maps hash_maps b ) csvs in

	(* Test baseline *)
	let _ = new Linear.linear feature_maps in
	let linear_results = 1. in (* the cost of insertion is 1 *)

	(* let hashgroup = new Hashgroup.hashgroup feature_maps hash_maps b in *)
	let hashgroup_results =
		List.map ( fun csv ->
			let hg = new Hashgroup.hashgroup feature_maps hash_maps b in
			let _ = List.iter hg#insert csv in
			mean_of_ints testset_rows_amount hg#insertion_costs ) csvs
		|> mean_of_floats permutations_amount in

	( parameters @ get_metrics adaptive csvs, parameters @ get_metrics strict csvs ),
	( [ string_of_float linear_results ], [ string_of_float hashgroup_results ] )

let run_permutations_test source_file dest_adaptive dest_strict dest_linear dest_hashgroup map =

	let testset_gen, features_amount = load_csv source_file map in

	let trees_data, naive_data =
		List.map ( fun ( m, b ) -> assemble_data m b testset_gen features_amount ) test_parameters
		|> List.split in

	let adaptive_test_data, strict_test_data 		= List.split trees_data in
	let linear_test_data, 	hashgroup_test_data = List.split naive_data in

	let trees_header = [ "m"; "b"; "avg_depth"; "avg_usage"; "std_depth"; "std_usage"; "avg_access"; "avg_insertion" ] in
	let naive_header = [ "avg_insertion" ] in

	let _ = save_to_csv trees_header dest_adaptive 	adaptive_test_data in
	let _ = save_to_csv trees_header dest_strict 		strict_test_data in
	let _ = save_to_csv naive_header dest_hashgroup hashgroup_test_data in
	let _ = save_to_csv naive_header dest_linear 		linear_test_data in

	()


(**************************************************
 Exec *)

let () =

	let _ = print_endline "Running tests..." in

	let _ = run_permutations_test
		"../data/source/magic04.data"
		"../data/adaptive/magic04.out"
		"../data/strict/magic04.out"
		"../data/linear/magic04.out"
		"../data/hashgroup/magic04.out"
		( fun x -> x ) in

	let _ = run_permutations_test
		"../data/source/cloud.data"
		"../data/adaptive/cloud.out"
		"../data/strict/cloud.out"
		"../data/linear/cloud.out"
		"../data/hashgroup/cloud.out"
		( fun x -> List.map float_of_string x ) in

	let _ = run_permutations_test
		"../data/source/shelterdogs.data"
		"../data/adaptive/shelterdogs.out"
		"../data/strict/shelterdogs.out"
		"../data/linear/shelterdogs.out"
		"../data/hashgroup/shelterdogs.out"
		( fun x -> x ) in

	(* let _ = run_log_test
		"../data/source/cloud.data"
		"../data/strict/cloud.out"
		( fun x -> List.map float_of_string x ) in *)

	()