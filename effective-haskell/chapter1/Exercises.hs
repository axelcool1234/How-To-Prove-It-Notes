module Exercises where

factorial = accum 1 
    where 
        accum res num
            | num == 0 || num == 1 = res 
            | otherwise = accum (res * num) (num - 1)
