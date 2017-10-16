{-
Module      : Módulo principal
License     : GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
Maintainer  : Rafael Guimarães Sakurai - rafaelsakurai.github.io

Exercício 02: Crie uma função projectEuler5 que retorna o primeiro número natural que 
retorna True para a função do exercício anterior. Pense em como reduzir o custo computacional.
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

projectEuler5 :: Integer
projectEuler5 = head [x | x <- [1..], divisivel20 x]

-- | Função principal
main :: IO()
main = do
  print (projectEuler5)

--232792560
--[Finished in 350.8s]