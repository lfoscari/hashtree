class ['a, 'b] linear
    ( feature_maps: ( 'a -> 'b ) list )
= object (self)
    val mutable archive = []

    method insert el = archive <- el :: archive
    method counting_insert el = let _ = self#insert el in 1 (* O(1) is for boys, 1 is for men *)

    method search feature_index feature_value =
      let feature_map = List.nth feature_maps feature_index in
      List.filter ( fun v -> feature_map v = feature_value ) archive

    (* method to_string =
      List.fold_left ( fun a v ->
        a ^ "[" ^ ( List.fold_left ( fun b t -> b ^ " " ^ ( string_of_int t ) ) "" v ) ^ " ]\n"
      ) "" archive *)

end