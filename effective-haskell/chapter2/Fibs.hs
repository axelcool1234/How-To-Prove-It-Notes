module Fibs where

-- Naive way to calculate fibonacci
-- fib n
--     | n == 0 = 0
--     | n == 1 = 1
--     | otherwise = (fib $ n - 1) + (fib $ n - 2)
--
-- fibs = map fib [0..]
--
-- smallFibs =
--     takeWhile (< 100) fibs

fibs = 0 : 1 : helper fibs (tail fibs)
    where
        helper (a:as) (b:bs) =
            a + b : helper as bs
