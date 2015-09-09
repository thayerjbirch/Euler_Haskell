isPalindrone :: String -> Bool
isPalindrone [x] = True
isPalindrone (x:xs)
    | x == (last xs) = isPalindrone(init xs)
    | otherwise = False
isPalindrone [] = True

findProduct :: Int -> Int
findProduct a = maximum [x*y | x <- [a,(a-1)..1], y <- [a,(a-1)..x], isPalindrone $ show(x*y)]

palindroneProduct :: Int -> Int
palindroneProduct digits = findProduct ((10 ^ digits) - 1)