-- Nombres: Yara Diaz de Cerio, Iván Garoña 

module CalendarioInteractivo where
import DiazDeCerioYGaroñaICalendario (printCalendario)

--IO
anyo:: String -> IO Int
anyo s = do
                    n <- escribeyleenum s
                    if not(n > 999 && n < 10000) then putStrLn "El año debe estar entre 1000 y 9999" >> anyo s else return n

columnas:: String -> IO Int
columnas s = do
                    n <- escribeyleenum s
                    if not (n>2 && n<5) then putStrLn "El un número debe estar entre 3 y 4" >> columnas s else return n

escribeyleenum:: String -> IO Int 
escribeyleenum s = do
                    putStrLn s
                    n <- getLine
                    return (read n)

main :: IO()
main = do
       anyo <- anyo "Introduce el anyo a buscar: "
       n <- columnas "Introduce el número de meses a mostrar por fila (3 o 4): "
       printCalendario n anyo
                      