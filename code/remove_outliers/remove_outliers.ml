(* Print stack trace *)
let _ = Printexc.record_backtrace true


(**************************************************
 Utilities *)

exception Invalid_input

let save_to_csv header filename test_results =	
    print_string "Results saved in "; print_endline filename;
    header :: test_results |> Csv.save filename

let load_data file =
    match Csv.load file with
    | header :: data -> header, List.map ( List.map float_of_string ) data
    | _ -> raise Invalid_input

let percentile q ls = 
    let n = List.length ls |> float_of_int in
    let srt = List.sort compare ls in
    q /. 100. *. n |> int_of_float |> List.nth srt    


(**************************************************
 Variables *)

let q_min, q_max = 10., 90.


(**************************************************
 Outliers *)

let remove_outliers data column min_percentile max_percentile =

    let data_column = List.map ( fun a -> List.nth a column ) data in
    
    let min, max = percentile min_percentile data_column, percentile max_percentile data_column in
    
    List.filter ( fun row -> let v = List.nth row column in v <= max && v >= min ) data
    |> List.map ( List.map string_of_float )


let help_remove source destination column =
    
    let header, data = load_data source in
    
    let results = remove_outliers data column q_min q_max in
    
    save_to_csv header destination results


(**************************************************
 Main *)

let () =

    help_remove "../data/tree/cloud.out" "../data/tree/nooutliers/cloud/depth.out" 2;
    help_remove "../data/tree/cloud.out" "../data/tree/nooutliers/cloud/usage.out" 3;
    help_remove "../data/tree/cloud.out" "../data/tree/nooutliers/cloud/insertion.out" 6;
    help_remove "../data/tree/cloud.out" "../data/tree/nooutliers/cloud/size.out" 7;
    help_remove "../data/tree/cloud.out" "../data/tree/nooutliers/cloud/access.out" 8;

    help_remove "../data/tree/magic04.out" "../data/tree/nooutliers/magic04/depth.out" 2;
    help_remove "../data/tree/magic04.out" "../data/tree/nooutliers/magic04/usage.out" 3;
    help_remove "../data/tree/magic04.out" "../data/tree/nooutliers/magic04/insertion.out" 6;
    help_remove "../data/tree/magic04.out" "../data/tree/nooutliers/magic04/size.out" 7;
    help_remove "../data/tree/magic04.out" "../data/tree/nooutliers/magic04/access.out" 8;

    help_remove "../data/tree/shelterdogs.out" "../data/tree/nooutliers/shelterdogs/depth.out" 2;
    help_remove "../data/tree/shelterdogs.out" "../data/tree/nooutliers/shelterdogs/usage.out" 3;
    help_remove "../data/tree/shelterdogs.out" "../data/tree/nooutliers/shelterdogs/insertion.out" 6;
    help_remove "../data/tree/shelterdogs.out" "../data/tree/nooutliers/shelterdogs/size.out" 7;
    help_remove "../data/tree/shelterdogs.out" "../data/tree/nooutliers/shelterdogs/access.out" 8;

    ()
