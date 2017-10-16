{-
Module      : Módulo principal
License     : GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
Maintainer  : Rafael Guimarães Sakurai - rafaelsakurai.github.io

Exercício 04: Faça uma função que determine se um número é primo.
-}
module Main where

-- | Função que verifica se um número é primo.
ehPrimo :: Integer -> Bool
ehPrimo num = length [1 | x <- [1..num], num `rem` x == 0] <= 2

-- | Função principal
main :: IO()
main = do
  print (ehPrimo 2)
  print (ehPrimo 3)
  print (ehPrimo 4)
  print (ehPrimo 5)
  print (ehPrimo 6)