(* Le metriche in comune tra le 3 strutture dati sono:
 *   - insertion
 *   - size
 *   - access
 * Qui vogliamo calcolare la differenza tra tree e hashgroup e tra tree e linear
 * per ogni valore di m e b
 *)


(**************************************************
 Utilities *)

exception Invalid_input

let to_keep = [ "avg_insertion"; "avg_size"; "avg_access" ]
let header = [ "m"; "b" ] @ to_keep

let sample = Csv.load "../data/tree/magic04.out"
let parameters = List.tl sample |> Csv.transpose |> BatList.take 2 |> Csv.transpose

let load_data_from_csv filename =
    Csv.load filename
    |> Csv.transpose
    |> List.filter ( fun ls -> List.mem ( List.hd ls ) to_keep )
    |> Csv.transpose
    |> List.tl
    |> List.map ( List.map float_of_string )

let save_to_csv header filename data =
    let csv_data = List.map ( List.map string_of_float ) data in
    let _ = header :: ( List.map2 (@) parameters csv_data ) |> Csv.save filename in
	print_string "Test results saved in "; print_endline filename


(**************************************************
 Delta *)

let compute_delta a b = List.map2 ( List.map2 (-.) ) a b

let delta_of dataset_name =

    let dataset_filename = dataset_name ^ ".out" in
    let destination_folder = "../data/deltas/" ^ dataset_name in

    let tree = load_data_from_csv ( "../data/tree/" ^ dataset_filename ) in
    let hashgroup = load_data_from_csv ( "../data/hashgroup/" ^ dataset_filename ) in
    let linear = load_data_from_csv ( "../data/linear/" ^ dataset_filename ) in

    let tree_hashground_delta = compute_delta tree hashgroup in
    let tree_linear_delta = compute_delta tree linear in

    let _ = save_to_csv header ( destination_folder ^ "/tree-hashgroup.out" ) tree_hashground_delta in
    let _ = save_to_csv header ( destination_folder ^ "/tree-linear.out" ) tree_linear_delta in

    ()


(**************************************************
 Main *)

let () =

    let _ = delta_of "magic04" in
    let _ = delta_of "cloud" in
    let _ = delta_of "shelterdogs" in

    ()

