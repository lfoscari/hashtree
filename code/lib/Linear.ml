class ['a, 'b] linear
    ( feature_maps: ( 'a -> 'b ) list )
= object (self)

  val mutable archive = []

  val mutable insertion_costs = []
  method insertion_costs = insertion_costs

  method size = List.length archive

  val mutable search_costs = []
  method search_costs = search_costs

  method insert el =
    archive <- el :: archive;
    insertion_costs <- 1 :: insertion_costs (* Numero di accessi *)

  method search feature_index feature_value =
    search_costs <- self#size :: search_costs;
    let feature_map = List.nth feature_maps feature_index in
    List.filter ( fun v -> feature_map v = feature_value ) archive |> ignore

  method feature_maps = feature_maps
  method hash_family: ( 'b -> int ) list = []
  method table_size = 0

end