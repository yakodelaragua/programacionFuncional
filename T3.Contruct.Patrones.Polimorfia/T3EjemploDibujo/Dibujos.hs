
-- ######################
-- Dibujos.hs
-- ######################

module Dibujos where

type Dibujo = [Linea]
type Linea = [Char]

casa:: Dibujo

casa = [".....##.....",
        "....#..#....",
        "...#....#.##",
        "..#......###",
        ".#........##",
        "############",
        "#..........#",
        "#..........#",
        "#..........#",
        "#..........#",
        "#..........#",
        "############"]

blanco :: Dibujo 

blanco = ["............",
          "............",
          "............",
          "............",
          "............",
          "............",
          "............",
          "............",
          "............",
          "............",
          "............",
          "............"]

arbol:: Dibujo
arbol = [ ".....&&.....",
          ".....&&.....",
          "....&&&&....",
          "....&&&&....",
          "....&&&&....",
          "...&&&&&&...",
          "...&&&&&&...",
          "...&&&&&&...",
          ".....&&.....",
          ".....&&.....",
          ".....&&.....",
          ".....&&....."]

-- Imprimir un Dibujo en pantalla

printDibujo :: Dibujo -> IO()
printDibujo = putStr . concat . map (++"\n")


-- Transformaciones de dibujos:

reflejoV :: Dibujo -> Dibujo
reflejoV = map reverse

reflejoH :: Dibujo -> Dibujo
reflejoH  = reverse

rotar :: Dibujo -> Dibujo
rotar = reflejoH . reflejoV

encima :: Dibujo -> Dibujo -> Dibujo  
-- los dibujos deben tener la misma anchura
encima = (++)

alLadoDe ::Dibujo -> Dibujo -> Dibujo  
-- los dibujos deben tener la misma altura
alLadoDe = zipWith (++)

superponer :: Dibujo -> Dibujo -> Dibujo
superponer = zipWith (zipWith combinar)

combinar :: Char -> Char -> Char
combinar x y = if (x=='.' && y=='.') then '.' else '#'

invertirColor :: Dibujo -> Dibujo
invertirColor = map (map invertir)

invertir :: Char -> Char
invertir ch = if ch=='.' then '#' else '.'



