(**************************************************
 Utilities *)

let random_from ls = List.length ls |> Random.int |> List.nth ls
let count a = List.fold_left ( fun acc v -> if v = a then acc + 1 else acc ) 0
let max_index ls = List.fold_left ( fun ( ( i, max ), cur ) v ->
        if compare v max = 1 then ( ( cur, v ), cur + 1 ) else ( ( i, max ), cur + 1 )
    ) ( ( 0, List.nth ls 0 ), 0 ) ls (* -> ( max value position, max value ), list length *)


(**************************************************
 Type *)

type ( 'a, 'b ) tree =
    | Leaf of 'a list * int
    | Node of ( 'a, 'b ) tree array * int * ( 'a -> 'b ) * ( 'b -> int )


(**************************************************
 HH-tree adaptive *)

class ['a, 'b] hashTree
    ( initial_bucket_size: int )
    ( feature_maps: ( 'a -> 'b ) list )
    ( hash_family: ( 'b -> int ) list )
    ( table_size: int )
= object (self)

    val mutable root: ( 'a, 'b ) tree = Leaf ( [], initial_bucket_size )
    method root = root

    val mutable insertion_costs = []
    method insertion_costs = insertion_costs

    method feature_maps = feature_maps

    (* Ottimizzazione: utilizzo già i valori di f(x) calcolati per trovare la feature migliore *)
    method private pick_feature bucket hash_map =
        let ( max_i, _ ), _ = List.map ( self#featured_gini bucket hash_map ) feature_maps |> max_index in
        max_i, List.nth feature_maps max_i

    method private create_node bucket size =
        let hash_map = random_from hash_family in
        let feature_index, feature_map = self#pick_feature bucket hash_map in
        Node ( Array.make table_size ( Leaf ( [], size ) ), feature_index, feature_map, hash_map )

    method insert el =
        let count = ref 0 in
        let rec aux dest el =
            let _ = count := !count + 1 in
            match dest with
            | Leaf ( bucket, max ) when List.length bucket <= max ->
                Leaf ( el :: bucket, max )
            | Leaf ( bucket, max ) ->
                List.fold_left aux ( self#create_node bucket ( max + 1 ) ) ( el :: bucket )
            | Node ( children, index, feature, hash ) ->
                let dest = feature el |> hash in
                let _ = children.(dest) <- aux children.(dest) el in
                Node ( children, index, feature, hash ) in
        let _ = root <- aux root el in
        let _ = insertion_costs <- !count :: insertion_costs in
        ()

    (* DA QUI IN POI LE STRUTTURE DATI AD ALBERO SONO IDENTICHE *)

    (* Da ripulire *)
    method featured_gini bucket hash_map feature_map =
        let featured = List.map ( fun v -> feature_map v |> hash_map ) bucket in
        let len = List.length bucket |> float_of_int in (* = max_bucket_size *)
        let unique = List.sort_uniq compare featured in
        let len_unique = List.length unique |> float_of_int in
        if len_unique = 1. then 0. else
            ( 1. -. ( List.fold_left ( fun sum unq -> sum +. ( ( count unq featured |> float_of_int ) /. len ) ** 2. ) 0. unique ) ) *.
                ( len_unique /. ( len_unique -. 1. ) )

    (* method visit ( node: ( 'a, 'b ) tree ) =
        match node with
        | Leaf ( bucket, _ ) -> bucket
        | Node ( children, _, _, _ ) -> Array.fold_left ( fun res child -> ( self#visit child ) @ res ) [] children *)

    (* TODO: ricerca su più feature_index e feature_value *)
    method search feature_index feature_value =
        let count = ref 0 in
        let rec aux node =
            let _ = count := !count + 1 in
            match node with
            | Leaf ( bucket, _ ) -> bucket
            | Node ( children, index, _, hash ) when index = feature_index ->
                hash feature_value |> Array.get children |> aux
            | Node ( children, _, _, _ ) ->
                Array.fold_left ( fun res node -> ( aux node ) @ res ) [] children
        in aux root |> ignore; !count

    (* Like search, but counts the number of accesses *)
    (* method counting_search feature_index feature_value =
        let rec aux count node =
            let new_count = count + 1 in
            match node with
            | Leaf _ -> new_count
            | Node ( children, index, _, hash ) when index = feature_index ->
                hash feature_value |> Array.get children |> aux new_count
            | Node ( children, _, _, _ ) ->
                Array.fold_left aux new_count children
        in aux 0 root *)

    method depth =
        let rec aux = function
            | Leaf _ -> 1
            | Node ( children, _, _, _ ) -> 1 + Array.fold_left ( fun max child ->
                    match aux child with
                    | d when d > max -> d
                    | _ -> max
                ) 0 children
        in aux root

    method usage =
        let rec aux = function
            | Leaf ( bucket, max ) -> ( List.length bucket |> float_of_int ) /. ( float_of_int max )
            | Node ( children, _, _, _ ) ->
                ( Array.fold_left ( fun mean child -> mean +. aux child ) 0. children ) /. ( Array.length children |> float_of_int )
        in aux root

    method size =
        let rec aux = function
            | Leaf ( bucket, _ ) -> 1 + List.length bucket (* Need to consider the slot in the father's table *)
            | Node ( children, _, _, _ ) -> Array.fold_left ( fun sum node -> sum + aux node ) 0 children
        in aux root


end
