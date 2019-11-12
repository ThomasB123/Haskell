factorial :: Int -> Int
factorial n = product [1..n]

startsWithA :: [Char] -> Bool
startsWithA ('a':_) = True
startsWithA _ = False

startsWithAB :: [Char] -> Bool
startsWithAB ('a':'b':_) = True
startsWithAB _ = False

sumTwo :: Num a => [a] -> a
sumTwo (x:y:_) = x + y

--[(x, y) | x <- [1,2,3], y <- [4, 5]]
--[(x, y) | x <- [1..3], y <- [x..3]]
--[(x, y) | x <- [1..3], even x, y <- [x..3]]
--[(x, y) | x <- [1..3], y <- [x..3], even x, even y]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1, n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

--zip ['a'..'d'] [1..10]

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (xz,i) <- zip xs [0..], x == xz]