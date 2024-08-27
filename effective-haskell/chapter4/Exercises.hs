module Exercises where

data BinaryTree a = Leaf | Branch (BinaryTree a) a (BinaryTree a)

-- Turn a binary tree of strings into a pretty-printed string
showStringTree :: BinaryTree String -> String
showStringTree Leaf = ""
showStringTree (Branch l m r) =
  "(" <> showStringTree l <> ") - "
  <> m <>
  " - (" <> showStringTree r <> ")"

-- Get the root of the BinaryTree
root :: BinaryTree a -> Maybe a
root Leaf = Nothing
root (Branch l m r) = Just m

-- Get the left branch of the BinaryTree
lchild :: BinaryTree a -> Maybe (BinaryTree a)
lchild Leaf = Nothing
lchild (Branch l m r) = Just l

-- Get the right branch of the BinaryTree
rchild :: BinaryTree a -> Maybe (BinaryTree a)
rchild Leaf = Nothing
rchild (Branch l m r) = Just r

-- Add a new integer into a binary tree of integers
addElementToIntTree :: BinaryTree Int -> Int -> BinaryTree Int
addElementToIntTree Leaf val = Branch Leaf val Leaf
addElementToIntTree (Branch l m r) val
  | val < m = Branch (addElementToIntTree l val) m r
  | otherwise = Branch l m (addElementToIntTree r val) 

-- Check to see if an int value exists in a binary tree of ints
doesIntExist :: BinaryTree Int -> Int -> Bool
doesIntExist Leaf val = False
doesIntExist (Branch l m r) val =
  m == val || doesIntExist l val || doesIntExist r val
