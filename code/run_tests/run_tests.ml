open Lib

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

let rec split3 ?( acc = ( [], [], [] ) ) ls =
	let a, b, c = acc in
	match ls with
	| ( d, e, f ) :: xs -> split3 ~acc:( a @ [d], b @ [e], c @ [f] ) xs
	| [] -> acc


(**************************************************
 Testing parameters *)

let testset_rows_amount = 50 (* n *)
let permutations_amount = 10
let random_searches_amount = 50

(* tree only *)
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


(**************************************************
 Random search *)

let random_search ds csv =

	let feature_index = List.nth csv 0 |> List.length |> Random.int in

	let feature_map = List.nth ds#feature_maps feature_index in

	let element = List.length csv |> Random.int |> List.nth csv in

	feature_map element |> ds#search feature_index


(**************************************************
 Run tests *)

let save_to_csv header filename test_results =

	let _ = print_string "Test results saved in "; print_endline filename in

	header :: test_results |> Csv.save filename

let common_metrics dss =

	let insertion_ls = List.map ( fun ds -> mean_of_ints testset_rows_amount ds#insertion_costs ) dss in
	let size_ls = List.map ( fun ds -> ds#size ) dss in
	let access_ls = List.map ( fun ds -> mean_of_ints testset_rows_amount ds#search_costs ) dss in

	let avg_insertion = mean_of_floats permutations_amount insertion_ls in
	let avg_size = mean_of_ints permutations_amount size_ls in
	let avg_access = mean_of_floats permutations_amount access_ls in

	[ string_of_float avg_insertion;
		string_of_float avg_size;
		string_of_float avg_access;
	]

let tree_metrics trees =

	let depth_ls = List.map ( fun tree -> tree#depth ) trees in
	let usage_ls = List.map ( fun tree -> tree#usage ) trees in

	let avg_depth = mean_of_floats permutations_amount depth_ls in
	let avg_usage = mean_of_floats permutations_amount usage_ls in

	let std_depth = std_of_floats permutations_amount avg_depth depth_ls in
	let std_usage = std_of_floats permutations_amount avg_usage usage_ls in

	[ string_of_float avg_depth; string_of_float avg_usage;
		string_of_float std_depth; string_of_float std_usage;
	] @ common_metrics trees


let assemble_data testset_gen features_amount ( bucket_size, table_size ) =

	let m, b = string_of_int bucket_size, string_of_int table_size in
	let csvs = Stream.npeek permutations_amount testset_gen in
	let feature_maps, hash_maps = build_arguments table_size features_amount in

	let trees = List.map ( fun test_set ->
		let tree = new HHtree.hashTree bucket_size feature_maps hash_maps table_size min_gini attempts in
		let _ = List.iter tree#insert test_set in
		for _ = 0 to random_searches_amount do random_search tree test_set done; tree
	) csvs in
	let trees_results = tree_metrics trees in

	let hashgroups = List.map ( fun test_set ->
		let hg = new Hashgroup.hashgroup feature_maps hash_maps table_size in
		let _ = List.iter hg#insert test_set in
		let _ = List.iter hg#search test_set in hg
	) csvs in
	let hashgroup_results = common_metrics hashgroups in

	let linears = List.map ( fun test_set ->
		let ln = new Linear.linear feature_maps in
		let _ = List.iter ln#insert test_set in
		for _ = 0 to random_searches_amount do random_search ln test_set done; ln
	) csvs in
	let linear_results = common_metrics linears in

	[m; b] @ trees_results, [m] @ hashgroup_results, linear_results

let run_permutations_test source_file dest_tree dest_linear dest_hashgroup map =

	let testset_gen, features_amount = load_csv source_file map in

	let build_structures = assemble_data testset_gen features_amount in
	let trees_test_data, hashgroup_test_data, linear_test_data = List.map build_structures test_parameters |> split3 in

	let common_header 	 = [ "avg_insertion"; "avg_size"; "avg_access" ] in
	let hashgroup_header = [ "b" ] @ common_header in
	let linear_header 	 = [] @ common_header in
	let trees_header 		 = [ "m"; "b"; "avg_depth"; "avg_usage"; "std_depth"; "std_usage" ] @ common_header in

	let _ = save_to_csv trees_header dest_tree trees_test_data in
	let _ = save_to_csv hashgroup_header dest_hashgroup hashgroup_test_data in
	let _ = save_to_csv linear_header dest_linear linear_test_data in

	()


(**************************************************
 Exec *)

let () =

	let _ = print_endline "Running tests..." in

	(* Numeric *)
	let _ = run_permutations_test
		"../data/source/magic04.data"
		"../data/tree/magic04.out"
		"../data/linear/magic04.out"
		"../data/hashgroup/magic04.out"
		( fun x -> x ) in

	(* Numeric *)
	let _ = run_permutations_test
		"../data/source/cloud.data"
		"../data/tree/cloud.out"
		"../data/linear/cloud.out"
		"../data/hashgroup/cloud.out"
		( fun x -> List.map float_of_string x ) in

	(* Categorical *)
	let _ = run_permutations_test
		"../data/source/shelterdogs.data"
		"../data/tree/shelterdogs.out"
		"../data/linear/shelterdogs.out"
		"../data/hashgroup/shelterdogs.out"
		( fun x -> x ) in

	let _ = print_endline "Done." in

	()