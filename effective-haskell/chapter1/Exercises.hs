module Exercises where

factorial = accum 1 
    where 
        accum res num
            | num == 0 || num == 1 = res 
            | otherwise = accum (res * num) (num - 1)

fibonacci = accum 1 0 
    where
        accum prev res counter
            | counter == 0 = res   
            | otherwise = accum res (prev + res) (counter - 1)

myCurry func arg1 arg2 = func (arg1, arg2)
myUncurry func tuple = func (fst tuple) (snd tuple)
