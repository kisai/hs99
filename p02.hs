-- Haskell 99 Problems
-- Solution to Lisp99 Problems in haskell
-- author: kisai
-- problem P02: Find the last but one box of a list.

import Test.QuickCheck

myButLast :: [a] -> a
myButLast [] error "empty List!!!"
myButLast (x:[]) error "List has only one element"
myButLast (x:y:[]) = x
myButLast (x:xs) = myButLast xs

prop_plusOne xs x y = (myLast (xs ++ [x, y])) == x

prop_reverse xs = not (null xs) ==> (head . tail . reverse) xs == myLast xs