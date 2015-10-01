data Tree a = EmptyTree | Node a (Tree a) (Tree a)

makeNode :: Int -> Tree Int
makeNode a = Node a (makeNode (a*2)) (makeNode (a*2+1))