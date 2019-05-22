maplist _ [] = []
maplist f (x:xs) = (f x):(maplist f xs)

double x = 2*x

fold _ z [] = z
fold f z (x:xs) = fold f (f x z) xs