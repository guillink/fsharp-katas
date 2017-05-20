type Book = One | Two | Three | Four | Five

let price = 8m
let fullPrice n = decimal(n) * price
let discount = function
    | 2 -> 0.05m
    | 3 -> 0.10m
    | 4 -> 0.20m
    | 5 -> 0.25m
    | x -> 0.0m
let discountedPrice n = (fullPrice n) - (discount n) * (fullPrice n)

let computePrice bag = 
    let rec comp xs amount =
        match xs with 
        | [] -> amount
        | [x,n] -> amount + fullPrice(n)
        | (x,n)::_ -> 
            let differentBooks = List.length xs
            let reducedList = 
                xs 
                |> List.map (fun (x,n) -> x, n-1)
                |> List.where (fun (x,n) -> n > 0)
            comp reducedList (amount + discountedPrice differentBooks)
    comp (List.countBy id bag) 0m
    
