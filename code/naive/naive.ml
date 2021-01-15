open Lib

(* Print stack trace *)
let _ = Printexc.record_backtrace true

let feature_amount = 3
let feature_maps = List.init feature_amount ( fun i -> ( fun x -> List.nth x i ) )
let test_set = List.init 50 ( fun _ -> List.init feature_amount ( fun _ -> Random.int 100 ) )

let test_hashgorups =

  let hash_tables_size, hash_family_size = 10, 100 in

  let hash_maps = List.init hash_family_size ( fun _ -> let n = Random.int 100 in ( fun x -> ( Hashtbl.seeded_hash n x ) mod hash_tables_size ) ) in

  let hg = new Naive.hashgroup feature_maps hash_maps hash_tables_size in

  let _ = List.iter hg#insert test_set in

  let check = List.for_all ( fun t ->
    let random_feature_index = Random.int feature_amount in
    let random_feature = List.nth feature_maps random_feature_index in
    random_feature t |> hg#search random_feature_index |> List.mem t
  ) test_set in
  
  let _ = print_endline hg#to_string in

  let _ = Printf.printf "%b\n" check in

  ()


let test_linear =

  let linear = new Naive.linear feature_maps in

  let _ = List.iter linear#insert test_set in

  let check = List.for_all ( fun t ->
    let random_feature_index = Random.int feature_amount in
    let random_feature = List.nth feature_maps random_feature_index in
    random_feature t |> linear#search random_feature_index |> List.mem t
  ) test_set in
  
  let _ = print_string linear#to_string in

  let _ = Printf.printf "%b\n\n" check in

  ()

let () =

  let _ = test_hashgorups in

  let _ = test_linear in

  ()