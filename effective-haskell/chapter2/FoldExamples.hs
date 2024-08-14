module FoldExamples where
import Prelude hiding (foldl, foldr)

-- This function has the "shape" of any general recursive function over a list.
-- isBalanced s = 
--     0 == isBalanced' 0 s
--     where
--        isBalanced' count s
--         | null s = count
--         | head s == '(' = isBalanced' (count + 1) (tail s)
--         | head s == ')' = isBalanced' (count - 1) (tail s)
--         | otherwise = isBalanced' count (tail s)

-- Accumulating a value recursively (by destructuring a list)
-- This is essentially the same as the foldl function which is available in Prelude
reduce func carryValue lst =
    if null lst then carryValue
    else 
        let intermediateValue = func carryValue (head lst)
        in reduce func intermediateValue (tail lst)

-- Reimplementation of isBalanced with the reduce function
isBalanced str = 0 == reduce checkBalance 0 str
    where
    checkBalance count letter 
        | letter == '(' = count + 1
        | letter == ')' = count - 1
        | otherwise = count

-- Left-associative fold
foldl func carryValue lst = 
    if null lst
    then carryValue
    else foldl func (func carryValue (head lst)) (tail lst)

-- Right-associative fold
foldr func carryValue lst = 
    if null lst
    then carryValue
    else func (head lst) $ foldr func carryValue (tail lst)

doubleElems = foldr doubleElem []
    where
        doubleElem num lst = (2 * num) : lst

doubleElems' elems = foldr (applyElem (*2)) [] elems
    where
        applyElem f elem accumulator = f elem : accumulator

map' f = foldr (applyElem f) []
    where applyElem f elem accumulator = f elem : accumulator

doubleWithMap elems = map' (*2) elems

map'' f xs =
    if null xs then []
    else f (head xs) : map'' f (tail xs)
