open System

let rec heads = function
    | [] -> []
    | [x] -> [x,[]]
    | x::xs -> (x, xs) :: (List.map (fun (y,l) -> y,x::l) (heads xs))
    
let rec permutations = function
    | [] -> [[]]
    | xs -> 
        heads xs
        |> List.collect (fun (h, t) -> 
            permutations t
            |> List.map (fun l -> h::l))

let anagrams word = 
    Seq.toList word
    |> permutations
    |> List.map (List.toArray >> String)