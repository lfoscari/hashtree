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


(**************************************************
 Testing parameters *)

let testset_rows_amount = 50 (* n *)
let permutations_amount = 10
let random_searches_amount = 50

(* strict only *)
let min_gini, attempts = 0.4, 10

let test_m = List.init testset_rows_amount ( fun i -> i + 1 )
let test_b = List.init testset_rows_amount ( fun i -> i + 1 )

(* These are the parameters agains which we'll test the trees *)
let test_parameters = all_combinations test_m test_b

let load_csv dataset_filename map =

    let testset_csv = Csv.load dataset_filename in
    let testset_gen = BatList.take testset_rows_amount testset_csv |> List.map map |> Permutations.stream_of_permutations in

    testset_gen, Csv.columns testset_csv


(**************************************************
 Testing metrics *)

let metrics = [
  ( "avg_depth", ( fun csv struc -> 1 ) );
  ( "avg_usage", ( fun csv struc -> 1 ) );
  ( "std_depth", ( fun csv struc -> 1 ) );
  ( "std_usage", ( fun csv struc -> 1 ) );
  ( "avg_access", ( fun csv struc -> 1 ) );
  ( "avg_insertion", ( fun csv struc -> 1 ) );
]

let structures = [
  ( fun ibs fm hf ts -> HHtreeAdaptive.hashTree ibs fm hf ts );
  ( fun ibs fm hf ts -> HHtreeStrict.hashTree ibs fm hf ts min_gini attempts );
  ( fun _ fm _ _ -> Linear.linear fm );
  ( fun _ fm hf ts -> Hashgroup.hashgroup fm hf ts );
]

let test_dataset filename map =

  let csv_stream, csv_width = load_csv filename map in



  ()