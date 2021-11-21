module InteracN2 where
import CalendariosN2 (printCalendario)

escribeYLee :: String -> IO Int 
escribeYLee s = do
    putStrLn s
    n <- getLine 
    return (read n)

main :: IO()
main = do
  anyo <- escribeYLee "Escribe anyo" 
  col <- escribeYLee "Columnas"
  printCalendario col anyo
