blancos :: Int-> String
blancos n = [' ' | i <- [1..n]]

divisores :: Int -> [Int]
divisores x = [z | z <- [1..div x 2], mod x z == 0] ++ [x]

esPrimo :: Int -> Bool 
esPrimo n = divisores n == [1,n]

listaPrimos :: [Int]
listaPrimos = [n | n <-[2..], esPrimo n]