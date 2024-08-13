module MaybeTooBig where

-- Conditional branches
printSmallNumber num = 
    let msg = if num < 10
        then show num
        else "the number is too big!"
    in print msg

guardSize num
    | num > 0 =
        let size = "positive"
        in exclaim size
    | num < 3 = exclaim "small"
    | num < 100 = exclaim "medium"
    | otherwise = exclaim "large"
    where
        exclaim message = "that's a " <> message <> " number!"

main = printSmallNumber 3
