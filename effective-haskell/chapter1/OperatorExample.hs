module OperatorExample where

-- Custom operator
infixl 6 +++
(+++) a b = a + b

-- The fixity declaration doesn't change anything 
-- about how the function will behave if it's not 
-- being called infix.
infixl 7 `divide`
divide = (/)
