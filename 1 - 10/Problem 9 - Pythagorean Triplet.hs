import Data.Matrix
--import Data.Tree

--Using technique from https://en.wikipedia.org/wiki/Tree_of_primitive_Pythagorean_triples
fstTriplet = fromList 1 3 [3,4,5]
a = fromList 3 3 [1,-2,2,2,-1,2,2,-2,3]
b = fromList 3 3 [1,2,2,2,1,2,2,2,3]
c = fromList 3 3 [-1,2,2,-2,1,2,-2,2,3]

data FuncTree a = Empty | Node a (FuncTree a) (FuncTree a) (FuncTree a)
 
pTree :: FuncTree (Matrix *)
pTree = build fstTriplet
            where build k = (Node k) build(k * a) build(k * b) build(k * c)