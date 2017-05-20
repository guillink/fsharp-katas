let rec visit xs n =
    if (n > 0 && n <= 100) then
        visit (List.map (fun (i,x) -> if i % n = 0 then i, not x else i, x) xs) (n+1)
    else 
        xs
    
visit (List.init 100 (fun i -> i+1, false)) 1