-- Haskell 99 Problems
-- Solution to Lisp99 Problems in haskell
-- author: kisai
-- problem P01: Find the last box of a list.

import Test.QuickCheck

myLast :: [a] -> a
myLast [] = error "Empty list!!!"
myLast (x:[]) = x
myLast (_:xs) = myLast xs

prop_plusOne xs x = (myLast (xs ++ [x])) == x

prop_reverse xs = (head . reverse) xs == myLast xs