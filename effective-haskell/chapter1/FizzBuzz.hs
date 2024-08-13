module FizzBuzz where

fizzBuzzFor number
    | number |/ 15 = "fizzbuzz"
    | number |/ 5  = "buzz"
    | number |/ 3  = "fizz"
    | otherwise = show number
    where
        (|/) x y = 0 == x `rem` y
