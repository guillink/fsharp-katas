type Tree<'a> = 
    | Leaf of 'a
    | Node of 'a * Tree<'a> list

let possibleMutation x y =
    Seq.zip x y |> Seq.sumBy (fun (x,y) -> if x = y then 0 else 1) <= 1

let buildTree start bank = 
    let rec buildTree cur bank =
        match List.where (possibleMutation cur) bank with 
        | [] -> Leaf cur
        | xs -> Node (cur, List.map (fun x -> (buildTree x (List.except [x] bank))) xs)
    buildTree start bank

let getPaths tree = 
    let rec dfs depth tree =
        match tree with
        | Leaf x -> [(x, depth)]
        | Node (x, xs) -> (x, depth)::(List.collect (dfs (depth+1)) xs)
    dfs 0 tree

let findShortestPath goal paths = 
    match List.filter (fun (x,d) -> x = goal) paths with
    | [] -> None
    | x -> Some (List.minBy (fun (x,d) -> d) x)

let minimumGeneticMutation start goal bank = buildTree start bank |> getPaths |> findShortestPath goal

minimumGeneticMutation "AACCGGTT" "AACCGGTA" ["AACCGGTA"]
minimumGeneticMutation "AACCGGTT" "AAACGGTA" ["AACCGGTA";"AACCGCTA";"AAACGGTA"]
minimumGeneticMutation "AAAAACCC" "AACCCCCC" ["AAAACCCC";"AAACCCCC";"AACCCCCC"]
minimumGeneticMutation "AACCGGTT" "AAACGGTC" ["AACCGGTA";"AAACGCTA";"AAACGGTA";"AACCGGTC";"AAACGGTC";"CAACGGTA"]
