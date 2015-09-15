lowestCommonMultiple :: [Int] -> Int
lowestCommonMultiple [x] = x
lowestCommonMultiple (x:xs) = lcm x (lowestCommonMultiple xs)