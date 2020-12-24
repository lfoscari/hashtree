open HashTree
open Printf

(* TODO: fare test estensivi su initial_bucket_size e table_sizes
 * differenti per trovare la configurazione ottimale e scrivere
 * un algoritmo che dato un dataset di esempio trova i valori
 * migliori (che avvicinano usage a 50%), fissate le feature
 * (che sono inalterabili perchè dipeso da utilità e dall'universo)
 *)

let rec compare_lists a b =
    match a, b with
    | xa :: xsa, xb :: xsb when xa = xb -> compare_lists xsa xsb
    | xa :: _, xb :: _ when xa > xb -> 1
    | xa :: _, xb :: _ when xa < xb -> -1
    | [], [] -> 0
    | _ -> raise Invalid_input

let initial_bucket_size = 100 (* Random.int 20 + 3 *)
let feature_maps = List.init feature_amount ( fun i -> ( fun x -> List.nth x i ) )
let table_size = 15

let hash_family_size = 10 (* Non è così importante per ora *)
let hash_maps = List.init hash_family_size ( fun _ -> ( fun x -> Hashtbl.seeded_hash ( Random.int 100 ) x mod table_size ) )
(* Facciamo finta che sia una famiglia universale *)

let root = new hashTree initial_bucket_size feature_maps hash_maps table_size
let _ = List.iter ( fun value -> root#insert value ) test_data

let no_loss = List.sort compare_lists root#visit = List.sort compare_lists test_data
let depth = root#depth root#root
let usage = root#usage root#root
