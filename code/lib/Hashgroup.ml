(**************************************************
 Utilities *)

let random_from ls = List.length ls |> Random.int |> List.nth ls
let count a = List.fold_left ( fun acc v -> if v = a then acc + 1 else acc ) 0
let max_index ls = List.fold_left ( fun ( ( i, max ), cur ) v ->
        if compare v max = 1 then ( ( cur, v ), cur + 1 ) else ( ( i, max ), cur + 1 )
    ) ( ( 0, List.nth ls 0 ), 0 ) ls (* -> ( max value position, max value ), list length *)

(* class ['a, 'b] hashgroup
    ( feature_maps: ( 'a -> 'b ) list )
    ( hash_family: ( 'b -> int ) list )
    ( hash_tables_size: int )
= object (_)

  val mutable archive = []
  method archive = archive

  val mutable hash_tables = List.init ( List.length feature_maps ) ( fun i ->
    List.nth feature_maps i, List.nth hash_family i , Array.make hash_tables_size []
  )
  method hash_tables = hash_tables

  method insert el =
    let _ = archive <- archive @ [el] in
    let pos = List.length archive - 1 in
    hash_tables <- List.map ( fun ( feature_map, hash_map, table ) ->
      let index = feature_map el |> hash_map in
      let _ = table.(index) <- pos :: table.(index) in
      feature_map, hash_map, table
    ) hash_tables

  method counting_insert el =
    let _ = archive <- archive @ [el] in
    let count = ref 1 in
    let pos = List.length archive - 1 in
    let _ = hash_tables <- List.map ( fun ( feature_map, hash_map, table ) ->
      let _ = count := !count + 1 in (* 2? *)
      let index = feature_map el |> hash_map in
      let _ = table.(index) <- pos :: table.(index) in
      feature_map, hash_map, table
    ) hash_tables in !count

  (* method private archive_to_string =
    let _, str = ( List.fold_left ( fun ( index, acc ) a ->
      index + 1, acc ^ ( string_of_int index ) ^ " [" ^ ( List.fold_left ( fun y b -> y ^ " " ^ ( string_of_int b ) ) "" a ) ^ " ]\n"
    ) ( 0, "" ) archive ) in str

  method private tables_to_string =
    List.fold_left ( fun a ( _, _, table ) ->
      a ^ ( Array.fold_left ( fun b x ->
        b ^ "[ " ^ ( List.fold_left ( fun c y -> c ^ ( string_of_int y ) ^ ", " ) "" x ) ^ "]\n"
      ) "" table ) ^ "\n"
    ) "" hash_tables

  method to_string = self#archive_to_string ^ "\n" ^ self#tables_to_string *)

  method search feature_index feature_value = (* ( feature_value: 'b ) *)
    let _, hash_map, hm = List.nth hash_tables feature_index in
    let indexes = hash_map feature_value |> Array.get hm in
    let _, v =
      ( List.mapi ( fun i v -> i, v ) archive
      |> List.filter ( fun ( i, _ ) -> List.mem i indexes )
      |> List.split ) in
    v

end *)

class ['a, 'b] hashgroup
    ( feature_maps: ( 'a -> 'b ) list )
    ( hash_family: ( 'b -> int ) list )
    ( hash_table_initial_size: int )
= object(self)

  val mutable size = hash_table_initial_size
  val mutable amount = 0 (* Number of elements in the data structure *)

  val mutable archive = Array.make hash_table_initial_size []
  method archive = archive

  val mutable feature = List.length feature_maps |> Random.int |> List.nth feature_maps
  val mutable hash = List.length hash_family |> Random.int |> List.nth hash_family

  method featured_gini bucket feature_map =
    let featured = List.map ( fun v -> feature_map v |> hash ) bucket in
    let len = List.length bucket |> float_of_int in (* = max_bucket_size *)
    let unique = List.sort_uniq compare featured in
    let len_unique = List.length unique |> float_of_int in
    if len_unique = 1. then 0. else
        ( 1. -. ( List.fold_left ( fun sum unq -> sum +. ( ( count unq featured |> float_of_int ) /. len ) ** 2. ) 0. unique ) ) *.
            ( len_unique /. ( len_unique -. 1. ) )

  method private pick_feature bucket =
    let ( max_i, _ ), _ = List.map ( self#featured_gini bucket ) feature_maps |> max_index in
    List.nth feature_maps max_i

  method rehash_archive =
    let _ = hash <- List.length hash_family |> Random.int |> List.nth hash_family in
    let _ = feature <- Array.to_list archive |> self#pick_feature in
    let _ = size <- size * 2 in
    let old = Array.copy archive in
    let _ = archive <- Array.make size [] in
    let _ = Array.iter self#insert old in
    ()

  (* Soluzione interna alle collisioni *)
  method insert el =
      let rec aux featured =
        let index = ( hash featured ) mod size in
        match archive.(index) with
        | [] -> archive.(index) <- el
        | _ -> aux index
      in
        let _ = amount <- amount + 1 in
        aux ( feature el )

  method counting_insert el = self#insert el; 1

  (* method search feature_index feature_value = () *)

end