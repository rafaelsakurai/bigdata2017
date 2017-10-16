{-
Module      : Módulo principal
License     : GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
Maintainer  : Rafael Guimarães Sakurai - rafaelsakurai.github.io

Exercício 07: Faça uma função que calcule o coeficiente binomial de (m,n).
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

-- | Função principal
main :: IO()
main = do
  print (coeficienteBinomial 10 2)
  print (coeficienteBinomial 10 5)
  print (coeficienteBinomial 10 10)