unos :: [Integer]
unos = 1:unos
mas :: [Char]
mas = "Mas " ++ ymas where ymas = "y mas " ++ ymas

--No ciclica
forever :: t -> [t]
forever x = x: forever x

--Ciclica
foreverC :: p -> [p]
foreverC x = s where s= x:s

iterate :: (p -> p) -> p -> [p]
iterate f x = s where s = x: map f s