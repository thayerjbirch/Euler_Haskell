import Data.Matrix
import Data.List

--Using technique from https://en.wikipedia.org/wiki/Tree_of_primitive_Pythagorean_triples
fstTriplet = fromList 3 1 [3,4,5]
a = fromList 3 3 [1,-2,2,2,-1,2,2,-2,3]
b = fromList 3 3 [1,2,2,2,1,2,2,2,3]
c = fromList 3 3 [-1,2,2,-2,1,2,-2,2,3]

data FuncTree a = Empty | Node a (FuncTree a) (FuncTree a) (FuncTree a)

pyProduct :: Int -> Int
pyProduct target = findTar target primPList
    where
        findTar target (x:xs) = if(sum x > target) then -1 --not found
                                else if (scaledList /= []) then product scaledList
                                else findTar target xs
                                where scaledList = (checkMultiples target x 1)

        checkMultiples target list iter = if(theSum > target) then []
                                          else if(theSum == target) then map (*iter) list
                                          else checkMultiples target list (iter+1)
                                          where theSum = sum(map (*iter) list)
primPList = map toList (flattenBF pTree)

pTree :: FuncTree (Matrix Int)
pTree = build fstTriplet
            where build k = Node k (build(a * k)) (build(b * k)) (build(c * k))

--Adapted from http://aryweb.nl/2013/10/28/haskell-tree-traversal/
flattenBF :: (FuncTree a) -> [a]
flattenBF tree = fbf [tree]
    where
        fbf [] = []
        fbf xs = map nodeValue xs ++ fbf (concat (map getChildren xs))
        
        nodeValue (Node a _ _ _) = a
        
        getChildren (Node _ a b c) = filter(myFilter)[a,b,c]
        myFilter Empty          = False
        myFilter (Node _ _ _ _) = True

{- Testing Purposes
aTree :: Int -> FuncTree(Int)
aTree a = Node a
                (Node (a+1)
                       (Node (a+4)
                            Empty
                            Empty
                            Empty
                        )
                       (Node (a+5)
                            Empty
                            Empty
                            Empty
                        )
                        (Node (a+6)
                            Empty
                            Empty
                            Empty
                        )
                )
                (Node (a+2)
                      (Node (a+7)
                            Empty
                            Empty
                            Empty
                        )
                       (Node (a+8)
                            Empty
                            Empty
                            Empty
                        )
                        (Node (a+9)
                            Empty
                            Empty
                            Empty
                        )
                )
                (Node (a+3)
                      (Node (a+10)
                            Empty
                            Empty
                            Empty
                        )
                       (Node (a+11)
                            Empty
                            Empty
                            Empty
                        )
                        (Node (a+12)
                            Empty
                            Empty
                            Empty
                        )
                )-}