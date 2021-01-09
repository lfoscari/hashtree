open Lib
open Hhtree

(* Print stack trace *)
let _ = Printexc.record_backtrace true

(*
 * TODO:
 * Modularizzare il codice.
 * Scegliere un altro dataset e fare due test.
 *)

(* The test are organized like follows.
 * The dataset is fixed (MAGIC Gamma Telescope Dataset), from
 * such the first 'testset_rows_amount' or 'n' rows are selected and 
 * 'permutations_amount' of the total number of permutations
 * of those lines are generated.
 * 
 * A single test is parametric over two values m and b. Fixed
 * these for each permutation a tree in created, the values are
 * inserted and the metric are evaluated.
 * 
 * The values of m and b we want to test are
 *      m: 1 .. infinity
 *      b: 1 .. infinity
 * The following consideration are related to the
 * implementation in which m is constant in each node.
 * Obviously these need to be reduced, like follows
 *      When m = 1 then each bucket is long at most 1,
 *      ergo the usage is 100% and the depth is at most n.
 *      When m = n the usage is 100% and the depth is 1.
 *      As m increases the usage drops, but the depth is
 *      the same. It's same to assume that an m greater 
 *      than n is uselesss.
 * 
 *      When b = 1 the depth is at most n, the usage is
 *      ( 1 - ( n % m ) / m ) * 100. when b >= n
 *      and the hashing is universal the expected value
 *      of the depth is 1, the usage is 1 / m. Like b, m
 *      is not to be tested beyond n.
 *
 *  for i = 1 ... permutations_amount
 *      for j = 1 ... len(test_parameters)
 *          Create tree with parameters j-th test_parameters
 *          Insert i-th testset into tree
 *      Save metrics evaluations
 *)

(* If m is increased in each node, so that a leaf at depth d
 * has is long at most d + m, then the following happens
 *      When b = 1 the usage is 100%, because there's a
 *      split at every insertion past the m-th, and a split
 *      occurs when the usage is is a leaf; the depth is n - m + 1.
 *      When b = n there is not much to say, the expected
 *      value of depth is 1 (universal hashing) and of usage
 *      is n / m.
 *
 *      m is a mistery to me at the moment.
 *)

 (* Is the best option for both m and b, log2(n)? *)


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