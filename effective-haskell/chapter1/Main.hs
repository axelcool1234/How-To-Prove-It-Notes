module Main where

-- Pointful function
-- makeGreeting salutation person = 
--     salutation <> " " <> person

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

-- Intermediate values
makeGreeting salutation person = 
    let messageWithTrailingSpace = salutation <> " "
    in messageWithTrailingSpace <> person

-- Let bindings
extendedGreeting person =
    let joinWithNewlines a b = a <> "\n" <> b
        helloAndGoodbye hello goodbye =
            let hello' = makeGreeting hello person
                goodbye' = makeGreeting goodbye person
            in joinWithNewlines hello' goodbye'
    in helloAndGoodbye "Hello" "Goodbye"

-- Where bindings
-- Parameters are available to where bindings, but not
-- variables defined in the let bindings.
--
-- Let bindings have access to both parameters and variables
-- defined in where bindings.
letWhereGreeting name place =
    let
        salutation = "Hello" <> name
        meetingInfo = location "Tuesday"
    in salutation <> " " <> meetingInfo
    where
        location day = "we met at " <> place <> " on a " <> day

-- Version of extendedGreeting with nested where bindings
extendedGreeting' person = 
    helloAndGoodbye "Hello" "Goodbye"
    where
        helloAndGoodbye hello goodbye =
            joinWithNewLines hello' goodbye'
            where
                hello' = makeGreeting hello person
                goodbye' = makeGreeting goodbye person
        joinWithNewLines a b = a <> "\n" <> b

main = print $ makeGreeting "Hello" "George" 

