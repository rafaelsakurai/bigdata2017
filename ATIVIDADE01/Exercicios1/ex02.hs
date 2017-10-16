{-|
Module      : Módulo principal
License     : GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
Maintainer  : Rafael Guimarães Sakurai - rafaelsakurai.github.io

Exercício 02: Faça uma função mult3 x que retorne True caso a entrada seja múltiplo de 3 
e False caso contrário.
-}
module Main where

-- | Função que retorna True caso a entrada seja múltiplo de 3 e False caso contrário.
mult3 :: Integer -> Bool
mult3 x = x `mod` 3 == 0

-- | Função principal
main :: IO()
main = do
  print (mult3 3)
  print (mult3 4)