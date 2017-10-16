{-
Module      : Módulo principal
License     : GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
Maintainer  : Rafael Guimarães Sakurai - rafaelsakurai.github.io

Exercício 03: Faça uma função que calcule a soma da diagonal secundária de uma matriz.
-}
module Main where

-- | Função que soma os valores da diagonal secundária de uma matriz.
somaDiagonalSecundaria :: [[Integer]] -> Integer
somaDiagonalSecundaria matriz = foldl (+) 0 [matriz !! x !! (length matriz - 1 - x) | x <- [0..(length matriz - 1)]]

-- | Função principal
main :: IO()
main = do
  print(somaDiagonalSecundaria [[0,0,0,1],[0,0,1,0],[0,1,0,0],[1,0,0,0]])
  print(somaDiagonalSecundaria [[0,0,0,1],[0,0,2,0],[0,3,0,0],[4,0,0,0]])