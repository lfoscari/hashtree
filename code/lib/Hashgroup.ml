(**************************************************
 Utilities *)

let random_from ls = List.length ls |> Random.int |> List.nth ls
let count a = List.fold_left ( fun acc v -> if v = a then acc + 1 else acc ) 0
let max_index ls = List.fold_left ( fun ( ( i, max ), cur ) v ->
        if compare v max = 1 then ( ( cur, v ), cur + 1 ) else ( ( i, max ), cur + 1 )
    ) ( ( 0, List.nth ls 0 ), 0 ) ls (* -> ( max value position, max value ), list length *)


(**************************************************
 Hashgroup *)

class ['a, 'b] hashgroup
    ( feature_maps: ( 'a -> 'b ) list )
    ( hash_family: ( 'b -> int ) list )
    ( hash_table_initial_size: int )
= object(self)

  val mutable size = hash_table_initial_size
  val mutable amount = 0 (* Number of elements in the data structure *)

  val mutable archive = Array.make hash_table_initial_size []
  method archive = archive

  val mutable insertion_costs = []
  method insertion_costs = insertion_costs

  method size = Array.length archive |> float_of_int

  val mutable search_costs = []
  method search_costs = search_costs

  val mutable feature = List.length feature_maps |> Random.int |> List.nth feature_maps
  val mutable hash = List.length hash_family |> Random.int |> List.nth hash_family

  method featured_gini bucket feature_map =
    let featured = List.map feature_map bucket in
    let len = List.length bucket |> float_of_int in
    let unique = List.sort_uniq compare featured in
    let len_unique = List.length unique |> float_of_int in
    if len_unique = 1. then 0. else
        ( 1. -. ( List.fold_left ( fun sum unq -> sum +. ( ( count unq featured |> float_of_int ) /. len ) ** 2. ) 0. unique ) ) *.
            ( len_unique /. ( len_unique -. 1. ) )

  method private pick_feature bucket =
    let ( max_i, _ ), _ = List.map ( self#featured_gini bucket ) feature_maps |> max_index in
    List.nth feature_maps max_i

  method rehash_archive =
    let old = Array.fold_left ( fun acc ls -> if ls != [] then ls :: acc else acc ) [] archive in
    hash <- List.length hash_family |> Random.int |> List.nth hash_family;
    feature <- self#pick_feature old;
    size <- size * 2;
    archive <- Array.make size [];
    List.map self#inner_insert old |> ignore

  (* Soluzione interna alle collisioni *)
  method inner_insert el =
      let count = ref 0 in
      let rec aux index =
        count := !count + 1;
        match archive.(index) with
        | [] -> archive.(index) <- el
        | _ -> aux ( ( index + 1 ) mod size )
      in
        if amount >= size then self#rehash_archive;
        amount <- amount + 1;
        aux ( ( feature el |> hash ) mod size );
        !count

  method insert el = insertion_costs <- ( self#inner_insert el ) :: insertion_costs

  method search element =
    let count = ref 0 in (* numero di accessi *)
    let _ = count := !count + 1 in
    let feature_value = feature element in
    let rec aux index =
      match archive.(index) with
      | t when t = [] || feature t = feature_value && t = element ->
        search_costs <- !count :: search_costs
      | _ -> aux ( ( index + 1 ) mod size )
    in aux ( ( hash feature_value ) mod size )

    method feature_maps = feature_maps
    method hash_family = hash_family
    method table_size = size

end