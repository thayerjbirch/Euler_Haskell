--finds the nth prime where n is supplied via command line arguement
import System.Environment
import System.IO  
import System.IO.Error

main = do
        (n:_) <- getArgs                --Take the first arguement
        let target = maybeRead n        --Attempt to extract an int from the first arguement
        let upper = findUpperBound target
        primesUnderBound upper target
        
maybeRead :: String -> Maybe Int
maybeRead n = case reads n of
                [(x,_)] -> Just x
                _       -> Nothing
                
findUpperBound :: Maybe Int -> Maybe Int
findUpperBound Nothing  = Nothing
findUpperBound (Just x) = if(x >= 6)
                          then if(x >= 7022)
                               then Just(truncate (n * (log(n) + log(log(n - 0.9385)))))--shortcuts some work out later
                               else Just(truncate (n * (log(n) + log(log(n)))))         --from wikipedia
                          else Just(13)                                                 --estimation only works for at least 6
    where n = fromIntegral x
    
primesUnderBound :: Maybe Int -> Maybe Int -> IO ()
primesUnderBound Nothing      _      = putStrLn "Improper input, exiting..."
                                                                 --we're not checking the evens above 2
primesUnderBound (Just upper) (Just target) = putStrLn(show((2:3:sieve[3,5..upper]) !! target))
    where sieve (x:xs)
            | x * x > upper = x:xs                               --stops sieving when no more values will be removed
            | otherwise     = x : sieve(xs `minus` [x*x,x*x+x..])--we can start removing multiples at x^2 because lower
                                                                 --values will have already removed multiples below x^2
                                                                    
minus :: Ord a => [a] -> [a] -> [a]
minus [] _ = []
minus xs [] = xs
minus l1@(x:xs) l2@(y:ys)
    | x > y = minus l1 ys
    | x < y = x : minus xs l2
    | otherwise = minus xs l2