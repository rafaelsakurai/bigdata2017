{-
Module      : Módulo principal
License     : GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
Maintainer  : Rafael Guimarães Sakurai - rafaelsakurai.github.io

Exercício 06: Faça uma função que recebe um tipo Integer e retorna ele dividido por 2:
div2d :: Integer -> Double
-}
module Main where

-- | Função que recebe um inteiro e devolve seu valor dividido por 2
div2d :: Integer -> Double
div2d x = fromInteger x / 2

-- | Função principal
main :: IO()
main = do
  print (div2d 1)
  print (div2d 2)
  print (div2d 3)