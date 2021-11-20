
module Interactivo where
import CalendarioN (printCalendario)

escribeYLee :: String -> IO Int
escribeYLee s = do
                 putStrLn s
                 n <- getLine 
                 return (read n)

getAnyo :: String -> IO Int
getAnyo s = do
    n <- escribeYLee s
    return n

getCols :: String -> IO Int 
getCols s = do
    n <- escribeYLee s
    return n

main :: IO ()
main = do
    anyo <- getAnyo "Introduce un anyo: "
    col <- getCols "Introduce el numero de meses a mostrar por fila: "
    printCalendario col anyo

