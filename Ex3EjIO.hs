-- 1 --
pideCadenas :: IO()
pideCadenas = do
             putStrLn "String 1: "
             palabra1 <- getLine
             putStrLn "String 2: "
             palabra2 <- getLine
             if palabra1 == palabra2 then putStrLn "Son iguales" else putStrLn "Son diferentes"

-- 2 --
escribeylee :: String -> IO String
escribeylee s = do
                  putStrLn s
                  getLine

pideCadenas' :: IO()
pideCadenas' = do
                p1 <- escribeylee "String 1:"
                p2 <- escribeylee "String 2: "
                if p1 == p2 then putStrLn "Iguales" else putStrLn "Diferentes"

-- 3 --
escribeyleenum :: String -> IO Int 
escribeyleenum s = do
                    putStrLn s
                    n <- getLine 
                    return (read n)
pideNumeros :: IO()
pideNumeros = do
               n1 <- escribeyleenum "Num 1"
               n2 <- escribeyleenum "Num 2"
               if n1 == n2 then putStrLn ("Son iguales:" ++ show (n1+n2)) else putStrLn ("Son diferentes: " ++ show (n1*n2))

-- 4 --
pideNumeros' :: IO() 
pideNumeros' = do
               n1 <- escribeyleenum "Num 1"
               n2 <- escribeyleenum "Num 2"
               if n1 == n2 then putStrLn ("Son iguales:" ++ show (n1+n2)) else putStrLn "Son diferentes: " >> pideNumeros'

-- 5 --
juego :: IO ()
juego = do
 clave <- escribeyleenum "Dame un numero clave entre 1 y 30: "
 putStrLn ( concat ["\n" | _ <- [1..50]] )
 adivina clave

adivina :: Int -> IO() 
adivina n = do
    i <- escribeyleenum "Adivina la clave"
    if i == n then putStrLn "Clave correcta" else putStrLn "Clave incorrecta... Vuelve a intentarlo" >> adivina n

-- 6 --
juego' :: IO ()
juego' = do
 clave <- escribeyleenum "Dame un numero clave entre 1 y 30: "
 putStrLn ( concat ["\n" | _ <- [1..50]] )
 adivina' clave 1

adivina' :: Int -> Int -> IO() 
adivina' n inten = do
    i <- escribeyleenum "Adivina la clave"
    if i == n then putStrLn ("Clave correcta!!! La has adivinado en " ++ show inten ++ " intentos") else (if n < i then putStrLn "La clave es menor... Vuelve a intentarlo\n" else putStrLn "La clave es mayor... vuelve a intentarlo\n") >> adivina' n (inten+1)
