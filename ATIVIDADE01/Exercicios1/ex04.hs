{-|
Module      : Módulo principal
License     : GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
Maintainer  : Rafael Guimarães Sakurai - rafaelsakurai.github.io

Exercício 04: Faça uma função mult35 x que retorne True caso a entrada seja múltiplo de 
3 e 5 e False caso contrário.
-}
module Main where

-- | Função que retorna True caso a entrada seja múltiplo de 3 e False caso contrário.
mult3 x = x `mod` 3 == 0
mult3 :: Integer -> Bool
-- | Função que retorna True caso a entrada seja múltiplo de 5 e False caso contrário.
mult5 :: Integer -> Bool
mult5 x = x `mod` 5 == 0
-- | Função que retorna True caso a entrada seja múltiplo de 3 e 5 e False caso contrário.
mult35 :: Integer -> Bool
mult35 x = mult3 x && mult5 x

-- | Função principal
main :: IO()
main = do
  print (mult35 10)
  print (mult35 15)