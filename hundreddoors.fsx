let rec visit xs n =
    match n with
    | n when n > 0 && n <= 100 ->
        visit (List.map (fun (i,x) -> if i % n = 0 then i, not x else i, x) xs) (n+1)
    | n -> xs
    
visit (List.init 100 (fun i -> i+1, false)) 1