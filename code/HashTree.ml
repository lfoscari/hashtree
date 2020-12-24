module HashTree = struct
    (* Utilities *)
    let random_from ls = List.length ls |> Random.int |> List.nth ls
    let count a = List.fold_left ( fun acc v -> if v = a then acc + 1 else acc ) 0
    let max_index ls = List.fold_left ( fun ( ( i, max ), cur ) v ->
            if v > max then ( ( cur, v ), cur + 1 ) else ( ( i, max ), cur + 1 )
        ) ( ( 0, List.nth ls 0 ), 0 ) ls (* -> ( max value position, max value ), list length *)

    type 'a tree =
        | Leaf of 'a list * int
        | Node of 'a tree array * int * ( 'a -> int ) * ( int -> int )

    class ['a] hashTree
        ( initial_bucket_size: int )
        ( feature_maps: ( 'a -> int ) list )
        ( hash_family: ( int -> int ) list )
        ( table_size: int )
    = object (self)

        val mutable root: 'a tree = Leaf ( [], initial_bucket_size )
        method root = root

        method featured_gini bucket feat =
            let featured = List.map feat bucket in
            let len = List.length bucket |> float_of_int in (* = max_bucket_size *)
            let unique = List.sort_uniq compare featured in
            let len_unique = List.length unique |> float_of_int in
            if len_unique = 1. then 0. else
                ( 1. -. ( List.fold_left ( fun sum unq -> sum +. ( ( count unq featured |> float_of_int ) /. len ) ** 2. ) 0. unique ) ) *.
                    ( len_unique /. ( len_unique -. 1. ) )

        (* Ottimizzazione: utilizzo già i valori di f(x) calcolati per trovare la feature migliore *)
        method private pick_feature bucket =
            let ( max_i, _ ), _ = List.map ( self#featured_gini bucket ) feature_maps |> max_index in
            max_i, ( List.nth feature_maps max_i ), ( random_from hash_family )

        method private create_node bucket size =
            let feature_index, feature_map, hash_map = self#pick_feature bucket in
            Node ( Array.make table_size ( Leaf ( [], size ) ), feature_index, feature_map, hash_map )

        method insert el =
            let rec aux dest el =
                match dest with
                | Leaf ( bucket, max ) when List.length bucket < max -> Leaf ( el :: bucket, max )
                | Leaf ( bucket, max ) -> List.fold_left aux ( self#create_node bucket ( max + 1 ) ) ( el :: bucket )
                | Node ( children, index, feat, hash ) ->
                    let dest_i = feat el |> hash in
                    let _ = aux ( Array.get children dest_i ) el |> Array.set children dest_i in
                    Node ( children, index, feat, hash )
            in root <- aux root el

        method visit = self#search ( -1 ) 0

        method search ?( node = root ) feat_index partial_el =
            match node with
            | Leaf ( bucket, _ ) -> bucket
            | Node ( children, index, _, hash ) when index = feat_index ->
                self#search ~node:( hash partial_el |> Array.get children ) feat_index partial_el
            | Node ( children, _, _, _ ) ->
                Array.fold_left ( fun res node -> ( self#search ~node:node feat_index partial_el ) @ res ) [] children

        method depth ( node: 'a tree ) =
            match node with
            | Leaf ( _, _ ) -> 1
            | Node ( children, _, _, _ ) -> 1 + Array.fold_left ( fun max child ->
                let d = self#depth child in
                if d > max then d else max
            ) 0 children

        method usage ( node: 'a tree ) =
            match node with
            | Leaf ( bucket, max ) -> ( List.length bucket |> float_of_int ) /. ( float_of_int max )
            | Node ( children, _, _, _ ) ->
                ( Array.fold_left ( fun mean child -> mean +. self#usage child ) 0. children ) /. ( Array.length children |> float_of_int )
    end
end

open HashTree

(* Test *)

type data = int list
let feature_amount = 3
let test_size = 1000

(* Fisso il caso di test *)
let test_data = List.init test_size ( fun _ -> List.init feature_amount ( fun _ -> Random.int 100 ) )
