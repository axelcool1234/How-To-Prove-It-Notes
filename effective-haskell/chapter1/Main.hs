module Main where

-- Pointful function
makeGreeting salutation person = salutation <> " " <> person

-- Pointfree programming: Writing functions with no parameters at all
--
-- This function is partially applied, allowing us to remove the 
-- second parameter
-- makeGreeting' salutation = ((salutation <> " ") <>)
--
-- This function is partially applied. By writing (<>), we make it a
-- prefix function.
-- makeGreeting' salutation = (<>) (salutation <> " ")
--
-- Composing a partially applied lambda function
-- makeGreeting' = (<>) . (\salutation -> salutation <> " ")
--
-- Fully pointfree definition:
makeGreeting' = (<>) . (<> " ")

-- Partially applies the first argument of makeGreeting.
-- This is eta reduction (opposite of eta expansion), because 
-- we don't include the second argument of makeGreeting here.
enthusiasticGreeting salutation = 
    makeGreeting (salutation <> "!")

main = print "no salutation to show yet"


