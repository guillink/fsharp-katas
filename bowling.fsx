let bowling game = 
    let rec score xs turn acc =
        match turn, xs with
        | 10,_ -> 
            acc
        | _,x::y::z::tail when x = 10 ->
            score (y::z::tail) (turn + 1) (acc + x + y + z)
        | _,x::y::z::tail when x + y = 10 ->
            score (z::tail) (turn + 1) (acc + x + y + z)
        | _,x::y::tail ->
            score tail (turn + 1) (acc + x + y)
        | _ -> failwith "Invalid game"
    score game 0 0

bowling [10;7;3;9;0;10;0;8;8;2;0;6;10;10;10;8;1]