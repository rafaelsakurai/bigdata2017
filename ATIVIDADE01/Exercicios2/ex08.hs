{-
Module      : Módulo principal
License     : GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
Maintainer  : Rafael Guimarães Sakurai - rafaelsakurai.github.io

Exercício 08: Faça uma função que calcule o elemento (i, j) do triângulo de pascal.
-}
module Main where

-- | Função que calcula o fatorial de um número n.
fatorial :: Integer -> Integer
fatorial 0 = 1
fatorial 1 = 1
fatorial n = fatorial' n 1
  where
    fatorial' 1 r = r
    fatorial' n r = fatorial' (n - 1) (n * r)

-- | Função que calcula o coeficiente binomial de (m, n).
coeficienteBinomial :: Integer -> Integer -> Integer
coeficienteBinomial m n = (fatorial m) `quot` (fatorial n * fatorial (m - n))

-- | Função que recebe o elemento (i, j) e devolve seu valor no triângulo de pascal.
trianguloPascal :: (Integer, Integer) -> Integer
trianguloPascal x = coeficienteBinomial (fst (x) - 1) (snd (x) - 1)

-- | Função principal
main :: IO()
main = do
  print (trianguloPascal (5, 3))
  print (trianguloPascal (6, 4))
  print (trianguloPascal (8, 6))
  print (trianguloPascal (9, 9))