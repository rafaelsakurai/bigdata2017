{-|
Module      : Módulo principal
License     : GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
Maintainer  : Rafael Guimarães Sakurai - rafaelsakurai.github.io

Exercício 05: Faça um programa que retorne True caso a entrada seja menor que -1 ou 
(maior que 1 E múltiplo de 2), e False caso contrário.
-}
module Main where

-- | Função que retorna True caso a entrada seja menor que -1 ou (maior que 1 E múltiplo de 2),
--   e False caso contrário.
ex05 :: Integer -> Bool
ex05 x = x < -1 || (x > 1 && x `mod` 2 == 0)

-- | Função principal
main :: IO()
main = do
  print (ex05 (-2))
  print (ex05 (-1))
  print (ex05 0)
  print (ex05 1)
  print (ex05 2)
  print (ex05 3)