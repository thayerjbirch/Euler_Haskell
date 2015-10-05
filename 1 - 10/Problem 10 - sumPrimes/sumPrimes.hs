--sums all primes under an arguement supplied by command line
import System.Environment
import System.IO  
import System.IO.Error

main = do
        (n:_) <- getArgs                --Take the first arguement
        let upper = maybeRead n        --Attempt to extract an int from the first arguement
        let output = sum $ primesUnderBound upper
        putStrLn $ show output
        
maybeRead :: String -> Maybe Int
maybeRead n = case reads n of
                [(x,_)] -> Just x
                _       -> Nothing
                
primesUnderBound :: Maybe Int -> [Int]
primesUnderBound Nothing      = []                               --we're not checking the evens above 2
primesUnderBound (Just upper) = 2:sieve[3,5..upper]
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