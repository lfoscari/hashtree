open Lib
open Hhtree

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


(**************************************************
 Testing parameters *)

let testset_rows_amount = 50 (* n *)
let permutations_amount = 10
let random_searches_amount = 50

let test_m = List.init testset_rows_amount ( fun i -> i + 1 )
let test_b = List.init testset_rows_amount ( fun i -> i + 1 )

(* These are the parameters agains which we'll test the tree *)
let test_parameters = all_combinations test_m test_b

let load_csv dataset_filename map = 

    let testset_csv = Csv.load dataset_filename in (* "resources/magic04.data" *)
    let testset_gen = BatList.take testset_rows_amount testset_csv |> List.map map |> Permutations.stream_of_permutations in

    testset_gen, Csv.columns testset_csv (*, Csv.lines testset_csv *)


(**************************************************
 Build HH-tree *)

(* Remove incrementing bucket size? *)
let build_tree bucket_size table_size feature_amount test_set =

    let feature_maps = List.init feature_amount ( fun i -> ( fun x -> List.nth x i ) ) in

    let hash_family_size = 100 in
    (* Sommo la lista prima di fare hashing perchè Hashtbl quando fa hashing considera solo i primi 10 elementi *)
    let hash_maps = List.init hash_family_size ( fun _ -> let n = Random.int 100 in ( fun x -> ( Hashtbl.seeded_hash n x ) mod table_size ) ) in

    let tree = new hashTree bucket_size feature_maps hash_maps table_size in
    let _ = List.iter tree#insert test_set in

    tree


(**************************************************
 Random search *)

let random_search ( tree: ( 'a, 'b ) hashTree ) csv =

    let feature_index = List.nth csv 0 |> List.length |> Random.int in

    let feature_map = List.nth tree#feature_maps feature_index in

    let element = List.length csv |> Random.int |> List.nth csv in

    feature_map element |> tree#counting_search feature_index |> float_of_int
    

(**************************************************
 Run tests *)

let run_test source_file destination_file map =

    let testset_gen, features_amount = load_csv source_file map in

    let test_results = List.map ( fun ( m, b ) ->

        let csvs = Stream.npeek permutations_amount testset_gen in
        let trees = List.map ( build_tree m b features_amount ) csvs in

        let depth_ls = List.map ( fun tree -> tree#depth tree#root |> float_of_int ) trees in
        let usage_ls = List.map ( fun tree -> tree#usage tree#root ) trees in
        let access_count = List.mapi ( fun index tree ->
            ( List.init random_searches_amount ( fun _ -> List.nth csvs index |> random_search tree )
            |> List.fold_left (+.) 0. ) /. ( float_of_int random_searches_amount )
        ) trees in

        let len = float_of_int permutations_amount in

        let avg_depth = List.fold_left (+.) 0. depth_ls /. len |> round 4 in
        let avg_usage = List.fold_left (+.) 0. usage_ls /. len |> round 4 in

        let var_depth = List.fold_left ( fun var x -> var +. ( x -. avg_depth ) ** 2. ) 0. depth_ls /. len |> round 4 in
        let var_usage = List.fold_left ( fun var x -> var +. ( x -. avg_usage ) ** 2. ) 0. usage_ls /. len |> round 4 in

        let avg_access = List.fold_left (+.) 0. access_count /. len |> round 4 in

        [
            string_of_int m; string_of_int b;
            string_of_float avg_depth; string_of_float avg_usage;
            string_of_float var_depth; string_of_float var_usage;
            string_of_float avg_access
        ]

    ) test_parameters in

    [ "m"; "b"; "avg_depth"; "avg_usage"; "var_depth"; "var_usage"; "avg_access" ] :: test_results
    |> Csv.save destination_file


(**************************************************
 Exec *)

let () =

    let _ = run_test
        "../resources/magic/magic04.data"
        "../resources/magic/magic04.out"
        ( fun x -> x ) in

    let _ = run_test
        "../resources/cloud/cloud.data"
        "../resources/cloud/cloud.out"
        ( fun x -> List.map float_of_string x ) in

    ()