{-
Module      : Módulo principal
License     : GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
Maintainer  : Rafael Guimarães Sakurai - rafaelsakurai.github.io

Exercício 01: Crie uma função divisivel20 x que retorna verdadeiro se x for 
  divisível por todos os números de 1 a 20.
-}
module Main where

-- | Função que verifica se um número x é divisível por todos os números de 1 a 20.
divisivel20 :: Integer -> Bool
divisivel20 x = divisivel20' x 20
  where
    divisivel20' x i
      | i == 1 = True
      | x `mod` i == 0 = divisivel20' x (i - 1)
      | otherwise = False
--divisivel20 x = length [i | i <- [1..20], x `mod` i == 0] == 20

-- | Função principal
main :: IO()
main = do
  print (divisivel20 232792560)
  print (divisivel20 1234567890)
  print (divisivel20 2432902008176640000)