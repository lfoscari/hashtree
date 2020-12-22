open HashTree

(* TODO: fare test estensivi su initial_bucket_size e table_sizes
 * differenti per trovare la configurazione ottimale e scrivere
 * un algoritmo che dato un dataset di esempio trova i valori
 * migliori (che avvicinano usage a 50%), fissate le feature
 * (che sono inalterabili perchè dipeso da utilità e dall'universo)
 *)

let initial_bucket_size = 100 (* Random.int 20 + 3 *)
let feature_maps: ( data -> int ) list = List.init feature_amount ( fun i -> ( fun x -> List.nth x i ) )
let table_sizes = [15; 15; 15]  (* List.map ( fun _ -> 2 + Random.int 10 ) feature_maps *)
let hash_maps: ( int -> int ) list = List.map ( fun size -> ( fun x -> Hashtbl.hash x mod size ) ) table_sizes

let root = new hashTree initial_bucket_size feature_maps hash_maps table_sizes
let _ = List.iter ( fun value -> root#insert value ) test_data

let no_loss = List.sort compare_lists root#visit = List.sort compare_lists test_data
let depth = root#depth root#root
let usage = root#usage root#root
