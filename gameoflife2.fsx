type Cell = D | A
let inBounds n bound = n >= 0 && n < bound
let neighbors x y grid = 
    [-1,-1;0,-1;1,-1;-1,0;1,0;-1,1;0,1;1,1]
    |> List.filter (fun (dx, dy) -> inBounds (x+dx) (Array2D.length1 grid) && inBounds (y+dy) (Array2D.length2 grid) && grid.[x+dx,y+dy] = A)
    |> List.length
let gameOfLife grid = Array2D.mapi (fun x y cell -> match neighbors x y grid, cell with | 2,A -> A | 3,_ -> A | _,_ -> D) grid
