sumSquares :: Int -> Int -> Int -> Int -> [Int]
sumSquares total square delta 0 = []
sumSquares total square delta rep = newTotal : sumSquares (newTotal) (square + delta) (delta + 2) (rep - 1)
    where newTotal = total + square + delta
    
startSum :: Int -> Int
startSum x = last $ sumSquares 0 0 1 x

squareDifference :: Int -> Int
squareDifference x = (sum[1..x])^2 - startSum x