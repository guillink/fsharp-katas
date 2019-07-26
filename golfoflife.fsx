let g(b:int[,])=Array2D.mapi(fun x y c->match ([-1,-1;-1,0;-1,1;0,-1;0,1;1,-1;1,0;1,1]|>List.sumBy(fun(i,j)->try b.[x+i,y+j]with _->0)),c with 2,1|3,_->1|_->0)b
