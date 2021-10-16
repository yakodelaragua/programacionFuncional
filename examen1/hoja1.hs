import Data.Char (isDigit)
--Ej3--
area :: Float -> Float
area r = pi * r ^ 2

perimetro :: Float -> Float
perimetro r = 2 * pi * r

--ej4--
agregar:: Int -> Int -> Int 
agregar x y = if y >= 0 && y <=9 then x*10+ y else error "no es un digito"

--ej5--
sumcuad :: Int -> Int -> Int -> Int
sumcuad x y z 
 | x <= y && x <= z = y*y + z*z
 | y <= x && y <= z = x*x + z*z
 |otherwise = x*x + y*y

--ej6--
divModP :: (Int,Int) -> (Int, Int)
divModP (x, y) = (div x y, mod x y)

--ej7--
sigLetra :: Char -> Char
sigLetra c 
 |c == 'z' = 'a'
 |c == 'Z' = 'A'
 |otherwise = toEnum (fromEnum c + 1)

--ej8--
digitoVal :: Char -> Int
digitoVal c = if isDigit c then fromEnum c - 48 else error "no es un digito"

--ej9--
prod :: Int -> Int -> Int 
prod n m 
 | n > m = error "n es mayor a m"
 |n == m = m
 | n < m = n * prod (n+1) m

--ej10--
edad :: (Int,Int,Int) -> (Int,Int,Int) -> Int 
edad (x, y, z) (x1, y1, z1)
 | y1 < y = edad - 1
 | y1 == y && x1 < x = edad -1
 |otherwise = edad
  where edad = z1 - z

--ej11--
(|-|) :: Bool -> Bool -> Bool 
(|-|) b1 b2 = not (b1 == b2)

--ej12--
tresIgual :: Eq a => a -> a -> a -> Bool
tresIgual x y z = x == y && y == z

--ej13--
hms :: Int -> (Int, Int, Int)
hms s = (horas, minutos, segundos)
        where horas = div s 3600
              minutos = div (s-horas*3600) 60 
              segundos = s - minutos * 60 + horas * 3600
