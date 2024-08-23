module ListComprehensionExamples where

pairs as bs = 
    let as' = filter (`elem` bs) as
        bs' = filter odd bs
        mkPairs a = map (a,) bs'
    in concatMap mkPairs as'

pairs' as bs = 
    [(a,b) | a <- as, a `elem` bs, 
             b <- bs, odd b]
