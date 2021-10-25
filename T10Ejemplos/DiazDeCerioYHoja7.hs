--1--
pideCadenas :: IO()
pideCadenas = do
     putStrLn "Introduce una cadena"
     palabra <- getLine
     putStrLn "Introduce otra cadena"
     palabra2 <- getLine
     if palabra == palabra2 then putStrLn "Las cadenas son iguales" else putStrLn "Las cadenas son diferentes"

--2--
escribeylee :: String -> IO String 
escribeylee m = do 
                 putStrLn m
                 c <- getLine 
                 return c

pideCadenas' :: IO()
pideCadenas' = do
    p1 <- escribeylee "Introduce una cadena"
    p2 <- escribeylee "Introduce otra cadena"
    if p1 == p2 then putStrLn "Las cadenas son iguales" else putStrLn "Las cadenas son diferentes"

--3--
escribeyleenum :: String -> IO Int
escribeyleenum m = do
                    putStrLn m
                    i <- getLine
                    return (read i)

pideNumeros :: IO()
pideNumeros = do
    num1 <- escribeyleenum "Introduce un numero entero"
    num2 <- escribeyleenum "Introduce otro numero entero"
    if num1 == num2 then print(num1 + num2) else print(num1 * num2)

--4--
pideNumeros' :: IO()
pideNumeros' = do
    putStrLn "----"
    num1 <- escribeyleenum "Introduce un numero entero"
    num2 <- escribeyleenum "Introduce otro numero entero"
    if num1 == num2 then print(num1 + num2) else pideNumeros'

--5--
juego :: IO ()
juego = do
 clave <- escribeyleenum "Dame un numero clave entre 1 y 30: "
 putStrLn ( concat ["\n" | _ <- [1..50]] )
 adivina clave
 
adivina :: Int -> IO() 
adivina n = do
 putStrLn "-----------------\n"
 num <- escribeyleenum "Introduce un numero"
 putStrLn "\n-----------------"
 if num == n then 
     putStrLn "Has acertado la clave" 
 else if num > n then do
      putStrLn "La clave es menor"
      adivina n
 else do
     putStrLn "La clave es mayor"
     adivina n

--6--
juego' :: IO ()
juego' = do
 clave <- escribeyleenum "Dame un numero clave entre 1 y 30: "
 putStrLn (concat ["\n" | _ <- [1..50]] )
 adivina' clave 1

adivina' :: Int -> Int -> IO() 
adivina' n i = do
 putStrLn "-----------------\n"
 num <- escribeyleenum "Introduce un numero"
 putStrLn "\n-----------------"
 if num == n then do
     putStrLn ("Has acertado la clave en " ++ show i ++ " intentos") 
 else if num > n then do
      putStrLn "La clave es menor"
      adivina' n (i+1)
 else do
     putStrLn "La clave es mayor"
     adivina' n (i+1)