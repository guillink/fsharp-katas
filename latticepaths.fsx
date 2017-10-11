let pathsCombinatory n =
    let fact n = 
        let rec fact n acc =
            match n with 
            | 0 | 1 -> acc
            | _ -> fact (n - 1) (acc * bigint(n))
        fact n 1I

    fact (2 * n) / (pown (fact n) 2)

pathsCombinatory 20
