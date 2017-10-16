{-|
Module      : Módulo principal
License     : GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
Maintainer  : Rafael Guimarães Sakurai - rafaelsakurai.github.io

Exercício 03: Faça uma função mult5 x que retorne True caso a entrada seja múltiplo de 5 
e False caso contrário.
-}
module Main where
-- | Função que retorna True caso a entrada seja múltiplo de 5 e False caso contrário.
mult5 :: Integer -> Bool
mult5 x = x `mod` 5 == 0

-- | Função principal
main :: IO()
main = do
  print (mult5 4)
  print (mult5 5)