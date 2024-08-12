module Main where

makeGreeting salutation person =
    salutation <> " " <> person

-- Partially applies the first argument of makeGreeting.
-- This is eta reduction (opposite of eta expansion), because 
-- we don't include the second argument of makeGreeting here.
enthusiasticGreeting salutation = 
    makeGreeting (salutation <> "!")

main = print "no salutation to show yet"


