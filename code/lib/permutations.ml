type direction = L | R

let swap a i j = let tmp = a.(j) in a.(j) <- a.(i); a.(i) <- tmp

let is_movable a i =
    let x, d = a.(i) in
    match d with
    | L -> i > 0 && x > ( fst a.( i - 1 ) )
    | R -> i < Array.length a - 1 && x > ( fst a.( i + 1 ) )

let move a i =
    let _, d = a.(i) in
    if is_movable a i then
        match d with
        | L -> swap a i ( i - 1 )
        | R -> swap a i ( i + 1 )
    else
        failwith "Not movable"

let scan_movable_largest a =
    let rec aux acc i =
        if i >= Array.length a then acc
        else if not ( is_movable a i ) then aux acc ( i + 1 )
        else
            let x, _ = a.(i) in
            match acc with
            | None -> aux ( Some i ) ( i + 1 )
            | Some j -> aux ( if x < fst a.(j) then acc else Some i ) ( i + 1 )
    in aux None 0

let flip = function | L -> R | R -> L

let scan_flip_larger x a =
    Array.iteri ( fun i ( y, d ) -> if y > x then a.(i) <- y, flip d ) a

let attach_direction a = Array.map ( fun x -> x, L ) a

let permutations_generator l =
  let a = Array.of_list l |> attach_direction in
  let r = ref ( Some l ) in
  let next () =
    let p = !r in
    let _ = match scan_movable_largest a with (* find largest movable *)
      | None -> r := None (* no more permutations *)
      | Some i ->
        let x, _ = a.(i) in
        let _ = move a i in (* move *)
        let _ = scan_flip_larger x a in (* after move, scan to flip *)
        r := Some ( Array.map fst a |> Array.to_list )
    in p
  in next

let stream_of_permutations l =
    let generator = permutations_generator l in
    Stream.from ( fun _ -> generator() )
