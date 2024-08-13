module FizzBuzz where

fizzBuzzFor number
    | number |/ 15 = "fizzbuzz"
    | number |/ 5  = "buzz"
    | number |/ 3  = "fizz"
    | otherwise = show number
    where
        -- Divides custom operator 
        -- (I made this just to try out custom operators. This isn't in the book)
        (|/) x y = 0 == x `rem` y

naiveFizzBuzz fizzBuzzCount curNum fizzBuzzString =
    if curNum > fizzBuzzCount
    then fizzBuzzString
    else
        let nextFizzBuzzString = fizzBuzzString <> fizzBuzzFor curNum <> " "
            nextNumber = curNum + 1
        in naiveFizzBuzz fizzBuzzCount nextNumber nextFizzBuzzString
