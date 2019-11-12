double x = x + x
quadruple x = double (double x)

-- Compute the factorial of an integer
fac :: Int -> Int
{- Base case: 0! = 1
   Recursive case: n! = n (n-1)! -}
fac 0 = 1
fac n = n * fac (n - 1)