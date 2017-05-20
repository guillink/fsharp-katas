type Cell = D | A

let countNeighbors x y grid = 
    let neighbors = [-1,-1;0,-1;1,-1;-1,0;1,0;-1,1;0,1;1,1]
    let rec count x y grid neighbors n = 
        match neighbors with
        | [] -> n
        | (dx,dy)::tail -> 
            if (x + dx >= 0 && x + dx < Array2D.length1 grid
                && y + dy >= 0 && y + dy < Array2D.length2 grid
                && grid.[x+dx,y+dy] = A) 
            then
                count x y grid tail (n+1)
            else
                count x y grid tail n
    count x y grid neighbors 0

let nextGenCell grid x y cell = 
        match countNeighbors x y grid, cell with
        | 2,A -> A
        | 3,_ -> A
        | _,_ -> D

let nextGen grid = Array2D.mapi (nextGenCell grid) grid

let grid = Array2D.init 4 8 (fun x y -> 
    match x,y with
    | 1,4 | 2,3 | 2,4 -> A
    | _ -> D)

nextGen grid 