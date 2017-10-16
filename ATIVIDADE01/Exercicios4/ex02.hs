{-
Module      : Módulo principal
License     : GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
Maintainer  : Rafael Guimarães Sakurai - rafaelsakurai.github.io

Exercício 02: Faça uma função que calcule a soma da diagonal principal de uma matriz.
-}
module Main where

-- | Função que soma os valores da diagonal principal de uma matriz.
somaDiagonalPrincipal :: [[Integer]] -> Integer
somaDiagonalPrincipal matriz = foldl (+) 0 [matriz !! x !! x | x <- [0..(length matriz - 1)]]

-- | Função principal
main :: IO()
main = do
  print(somaDiagonalPrincipal [[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]])
  print(somaDiagonalPrincipal [[1,0,0,0],[0,2,0,0],[0,0,3,0],[0,0,0,4]])