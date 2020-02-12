-- Anthony Diaz
-- 014058185
-- Due: 2/11/2020
-- Dr. Jurgensen
-- CECS 424-03

module Merge where

merge :: Ord a => [a] -> [a] -> [a]		-- merge two sorted lists to a sorted list
merge [] [] = []						-- if none, return empty list
merge [] ys = ys						-- if one
merge xs [] = xs						-- or the other list is empty merging is trivial
merge (x:xs) (y:ys)						-- if we are taking in two lists
	| x <= y 	= x : merge xs (y:ys)	-- if the left element is smaller then output it first
	| otherwise = y : merge (x:xs) ys	-- otherwise output the right element first

msort :: Ord a => [a] -> [a]									-- merge sort a list in Haskell
msort a 														-- msort function to divide the list
	| (length a) <= 1 	= a 									-- If the length of the list is less than or equal to 1, then return the list.
	| otherwise 		= merge (msort first) (msort second)	-- Otherwise, call the merge function while dividing the list given list in half.
		where													-- Where
			half = (length a) `div` 2							-- Half is the middle of the list.
			first = take half a 								-- First contains the first half of the list.
			second = drop half a 								-- Second contains the second half of the list.

main :: IO()												-- Main function.
main = print(msort [4, 65, 2, -31, 0, 99, 2, 83, 782, 1])	-- Prints list after being merge sorted.
