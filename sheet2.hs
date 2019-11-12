-- :type
['a', 'b', 'c'] :: [Char]
('a', 'b', 'c') :: (Char, Char, Char)
[(False, 0), (True, 10)] :: Num b => [(Bool, b)]
([False, True], [0, 1]) :: Num a => ([Bool], [a])
[tail, reverse, init] :: [[a] -> [a]]
--swap (x, y) = (y, x) ::
--pair x y = (x, y) ::
--double x = x * x ::
--palindrome xs = xs == reverse xs ::
--twice f x = f (f x) ::