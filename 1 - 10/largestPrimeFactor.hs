target = 600851475143
largestPrimeFactor a = head $ primeFactorization

primeFactorization a = findFactors [] a

findFactors :: [Int] -> Int -> [Int]
findFactors factList 1 = factList
findFactors factList remainder = findFactors (newFactor:factList) (remainder `div` newFactor)/* `div` is integer division, / doesn't work*/
    where newFactor = head $ dropWhile(\x -> remainder `mod` x /= 0)[2,3..remainder]