class ['a, 'b] hashgroup
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

end