twice :: (t->t) -> (t->t)
twice f = f . f

multiply :: Int -> (Int -> Int)
multiply n = (*n)

getEvens :: [Int] -> [Int]
getEvens l = filter ((==0) . (`mod` 2)) l

getOdds :: [Int] -> [Int]
getOdds l = filter ((/=0) . (`mod` 2)) l