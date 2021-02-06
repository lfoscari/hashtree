class ['a, 'b] linear
    ( feature_maps: ( 'a -> 'b ) list )
= object (_)
    val mutable archive = []

    val mutable insertion_costs = []
    method insertion_costs = insertion_costs

    method insert el =
      archive <- el :: archive;
      insertion_costs <- 1 :: insertion_costs (* Numero di accessi *)

    method search feature_index feature_value =
      (* let count = List.length archive in (* Numero di accessi *) *)
      let feature_map = List.nth feature_maps feature_index in
      List.filter ( fun v -> feature_map v = feature_value ) archive

    (* method to_string =
      List.fold_left ( fun a v ->
        a ^ "[" ^ ( List.fold_left ( fun b t -> b ^ " " ^ ( string_of_int t ) ) "" v ) ^ " ]\n"
      ) "" archive *)

end