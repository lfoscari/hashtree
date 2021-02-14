(**************************************************
 Utilities *)

let random_from ls = List.length ls |> Random.int |> List.nth ls
let count ls v = List.fold_left ( fun acc t -> if t = v then acc + 1 else acc ) 0 ls |> float_of_int
let max_index ls =
	let ( i, m ), _ = List.fold_left ( fun ( ( i, max ), cur ) -> function
		| v when v > max -> ( cur, v ), cur + 1
		| _ -> ( i, max ), cur + 1
	) ( ( 0, List.nth ls 0 ), 0 ) ls in i, m


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
	( min_gini: float )
	( attempts: int )
= object (self)

	val mutable root: ( 'a, 'b ) tree = Leaf ( [], initial_bucket_size )
	method root = root

	val mutable insertion_costs = []
	method insertion_costs = insertion_costs

	val mutable search_costs = []
	method search_costs = search_costs

	method private create_node bucket size =
		let rec aux iteration =
			if iteration >= attempts then
				Leaf ( bucket, size * 2 )
			else
				match self#pick_best_feature bucket with
				| _, _, _, gini when gini <= min_gini -> aux ( iteration + 1 )
				| feature_index, feature_map, hash_map, _ ->
					Node ( Array.make table_size ( Leaf ( [], initial_bucket_size ) ), feature_index, feature_map, hash_map )
		in aux 0

	method private pick_best_feature bucket =
		let hash_map = random_from hash_family in
		let index_max, gini = List.map ( self#featured_gini bucket hash_map ) feature_maps |> max_index in
		index_max, List.nth feature_maps index_max, hash_map, gini

	method featured_gini bucket hash_map feature_map =
		let featured = List.map ( fun v -> feature_map v |> hash_map ) bucket in
		let len = List.length bucket |> float_of_int in
		let unique = List.sort_uniq compare featured in
		let len_unique = List.length unique |> float_of_int in
		let normalize = len_unique /. ( len_unique -. 1. ) in
		if len_unique = 1. then 0. else
		( 1. -. ( List.fold_left ( fun sum v -> sum +. ( count featured v /. len ) ** 2. ) 0. unique ) ) *. normalize

	method insert el =
		let count = ref 0 in
		let rec aux dest el =
			let _ = count := !count + 1 in
			match dest with
			| Leaf ( bucket, max ) when List.length bucket <= max ->
				Leaf ( el :: bucket, max )
			| Leaf ( bucket, max ) ->
				( match self#create_node ( el :: bucket ) max with
				| Leaf _ as leaf -> leaf
				| Node _ as node -> List.fold_left aux node ( el :: bucket ) )
			| Node ( children, index, feature, hash ) ->
				let dest = feature el |> hash in
				let _ = children.(dest) <- aux children.(dest) el in
				Node ( children, index, feature, hash ) in
		root <- aux root el;
		insertion_costs <- !count :: insertion_costs

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
		in aux root |> ignore; search_costs <- !count :: search_costs

	method multi_feature_search features =
		let count = ref 0 in
		let rec aux node =
			let _ = count := !count + 1 in
			match node with
			| Leaf ( bucket, _ ) -> bucket
			| Node ( children, index, _, hash ) when List.mem_assoc index features ->
				List.assoc index features |> hash |> Array.get children |> aux
			| Node ( children, _, _, _ ) ->
				Array.fold_left ( fun res node -> ( aux node ) @ res ) [] children
		in aux root |> ignore; search_costs <- !count :: search_costs

	method depth =
		let rec aux = function
			| Leaf _ -> 1
			| Node ( children, _, _, _ ) -> 1 + Array.fold_left ( fun max child ->
					match aux child with
					| d when d > max -> d
					| _ -> max
				) 0 children
		in aux root |> float_of_int


	method usage =
		let rec aux = function
			| Leaf ( bucket, max ) -> ( List.length bucket |> float_of_int ) /. ( float_of_int max )
			| Node ( children, _, _, _ ) ->
				( Array.fold_left ( fun mean child -> mean +. aux child ) 0. children ) /. ( Array.length children |> float_of_int )
		in aux root

	method size =
		let rec aux = function
			| Leaf ( bucket, _ ) -> List.length bucket
			| Node ( children, _, _, _ ) -> Array.fold_left ( fun sum node -> sum + 1 + aux node ) 0 children
		in aux root

	method feature_maps = feature_maps
	method hash_family = hash_family
	method table_size = table_size
end
