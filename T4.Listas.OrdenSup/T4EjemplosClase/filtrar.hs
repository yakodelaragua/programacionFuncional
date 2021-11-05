par::Int->Bool 
par x = mod x 2 == 0

menor5::Int->Bool
menor5 x = x < 5

filtrar::(a->Bool)->[a]->[a]
filtrar p [] = []
filtrar p (x:s) = if p x then x:filtrar p s else filtrar p s

doble :: Num a => a -> a
doble x = x * 2

mapv::(a->b) -> [a]->[b]
mapv f [] = []
mapv f (x:s) = f x : mapv f s

--Definicion recursiva
dobPares :: [Int] -> [Int]
dobPares [] = []
dobPares (x:s) = if par x then x*2 : dobPares s else dobPares s

--Usando map y filter
dobPares2 :: [Int] -> [Int]
dobPares2 x = map (2*) (filter par x)