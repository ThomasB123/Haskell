-- This is the standard library definition
-- drop the first n elements from a list
drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop _ [] = []
drop n (x:xs) = drop (n-1) xs

fac n = if n == 0 then 1 else n * fac (n-1)

-- Linear Recursion
length' [] = []
length' (_:xs) = 1 + length' xs

-- Multiple Recursion
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- Direct Recursion
product' [] = []
product' (x:xs) = x * product' xs

-- Mutual/Indirect Recursion
even' :: Integral a => a -> Bool
even' 0 = True
even' n = odd' (n - 1)

odd' :: Integral a => a -> Bool
odd' 0 = False
odd' n = even' (n - 1)

-- Tail Recursion
factorial n = loop n 1
    where loop n res    | n < 0     = undefined
                        | n > 1     = loop (n - 1) (res * n)
                        | otherwise = res

-- Not Tail Recursive
product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product' xs

-- Tail Recursive
product' :: Num a => [a] -> a
product' xs = loop xs 1
    where loop [] n     = n
        loop (x:xs) n   = loop xs (x * n)

-- Mutual Tail Recursion
even 0 = True
even n = odd (n-1)
odd 0 = False
odd n = even (n-1)


binomial :: Integral a => a -> a -> a
binomial n 0 = 1
binomial n k
    | n == k    = 1
    | otherwise = binomial (n - 1) (k - 1) + binomial (n - 1) k

binomial' :: Integral a => a -> a -> a
binomial' n k = product [n-(k-1)..n] `div` product [1..k]

-- Currying means that every function of more than one argument is higher-order in Haskell
add :: Num a => a -> a -> a
add x y = x + y

-- Many linear recursive functions on lists can be written using higher order library functions
-- apply a function to a list
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f xs = [f x | x <- xs]

-- remove entries from a list
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p xs = [x | x <- xs, p x]
-- + these: any, all, concatMap, takeWhile, dropWhile, ...

-- Haskell uses the (.) operator
(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x) -- example
odd a = not (even a)
odd = not . even -- No need for the a variable

-- Folds: -- Standard prelude, more available in the Data.List module
-- Right associative fold
-- Generally the right choice (pun)
-- Works even for infinite lists
-- foldr (:) [] == id
-- can terminate early
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z []        = z
foldr f z (x:xs)    = x `f` (foldr f z xs)
-- Replace (:) by the given function, and [] by given value
-- Think of it this way:
-- sum [1, 2, 3]
-- = foldr (+) 0 [1, 2, 3]
-- = foldr (+) 0 (1:(2:(3:[])))
-- = 1 + (2 + (3 + 0))
-- =6
--- foldr's type is actually:
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b


-- Left associative fold
-- usually best to use strict version:
import Data.List
-- Doesn't work on infinite lists (needs to start at the end)
-- Can't terminate early
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f z []        = z
foldl f z (x:xs)    = foldl f (z `f` x) xs -- tail recursive!
-- Same idea but associating to the left:
-- sum [1, 2, 3]
-- = foldl (+) 0 [1, 2, 3]
-- = foldl (+) 0 (1:(2:(3:[])))
-- = (((1 + 2) + 3) + 0)
-- =6

-- we can write code for lists and (say) trees identically
-- Many library functions on lists are written using folds
product = foldr (*) 1
sum = foldr (+) 0
maximum = foldr1 max