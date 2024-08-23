module Exercises where
import Prelude hiding (reverse, zipWith, concatMap)

reverse lst = foldl helper [] lst
    where helper accum elem = elem : accum 

reverse' lst = foldr helper [] lst
    where helper elem accum = accum <> [elem] 

-- The foldl implementation is more simple, as prepending/consing (:) 
-- is common in Haskell. The foldr implementation is more costly, as 
-- it has to concatenate (<>) which requires copying its elements.
--
-- Time complexity:
-- Prepending/consing via the ":" operator is an O(1) operation.
-- Concatenation via the "<>" operator is an O(n) operation where
-- n is the length of accum.

zipWith func lst1 lst2 
    | null lst1 = []
    | null lst2 = []
    | otherwise = helper func lst1 lst2
    where
        helper func (a:as) (b:bs) =
            func a b : zipWith func as bs

zipWith' func lst1 lst2 =
    [func (lst1 !! i) (lst2 !! i) | i <- [0..min (length lst1) (length lst2) - 1]]

zipWith'' func lst1 lst2 = 
    reverse $ foldl helper [] (zipLists lst1 lst2)
    where
        zipLists (a:as) (b:bs) = (a, b) : zipLists as bs
        zipLists _ _ = []
        helper accum (a, b) = func a b : accum

zipWith''' func (a:as) (b:bs) = func a b : zipWith''' func as bs
zipWith''' func _ _ = []

concatMap func = 
    foldl (\accum x -> accum <> func x) []

concatMap' func = 
    foldr (\x accum -> func x <> accum) []
