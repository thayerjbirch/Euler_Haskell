fibs = 0 : 1 : zipWith (+) fibs(tail fibs)
result = sum[fibs!!n | n <- takeWhile(\x->fibs!!x < 4000000)[1..], even( fibs!!n )]