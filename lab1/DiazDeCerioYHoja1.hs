area :: Floating a => a -> a
area r = pi * r^2

perimetro :: Floating a => a -> a
perimetro r = 2 * pi * r

agregar :: Int -> Int -> Int
agregar x y = if y < 10 then x * 10 + y else x

sumcuad :: Int -> Int -> Int -> Int 
sumcuad x y z
 | x <= y && x <= z = y * y + z * z
 | y <= x && y <= z = x * x + z * z
 | otherwise = y * y + x * x

divMod::(Int, Int) -> (Int, Int)
divMod (x, y) = (x `div` y, x `mod` y)

sigLetra::Char->Char
sigLetra 'Z' = 'A'
sigLetra l = succ l

digitoVal::Char->Int
digitoVal c = fromEnum c - 48

prod:: Int -> Int -> Int 
prod n m 
 | n == m = n
 | n < m = n * prod(n+1) m
 | otherwise = error "Numeros incorrectos"

edad :: (Int , Int, Int) -> (Int, Int, Int) -> Int
edad (d, m, a) (d2, m2, a2) 
 |a2 >= a && (m2 > m || (m2 == m && d2 >= d)) = a2 - a
 |a2 >= a && (m2 == m && d2 < d) || m2 < m = a2 - a - 1
 |otherwise = error "Fechas incorrectas"

orExclusivo::Bool->Bool->Bool 
orExclusivo b1 b2 
 | (b1 && b2) || (not b1 && not b2) = False
 | (not b1 && b2) || (b1 && not b2) = True 

tresIgual :: Eq a => a -> a -> a -> Bool
tresIgual x y z = x == y && y == z

hms::Int->(Int, Int, Int)
hms s = (horas, minutos, segundos)
 where horas = s `div` 3600
       minutos = (s - horas*3600) `div` 60
       segundos = s - minutos * 60 - horas * 3600
 
triangulo::(Int, Int, Int)->String
triangulo (x, y, z)
 |x + y <= z = error "no es triangulo"
 |x == y && y == z = "Equilatero"
 |x == y || y == z = "Isosceles"
 |otherwise = "Escaleno"

nAND :: Bool -> Bool -> Bool
nAND x y
 |x == y = True
 |x == not y = False 
 |otherwise = False

nOR :: Bool -> Bool -> Bool
nOR x y 
 |x == not y = True 
 |y = True 
 |otherwise = True