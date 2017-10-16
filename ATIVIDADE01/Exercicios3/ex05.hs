{-
Module      : Módulo principal
License     : GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
Maintainer  : Rafael Guimarães Sakurai - rafaelsakurai.github.io

Exercício 05: Faça uma função para calcular o produto escalar entre dois vetores.
-}
module Main where

-- | Calcula o produto escalar entre dois vetores.
produtoEscalar :: [Integer] -> [Integer] -> Integer
produtoEscalar a b = foldl (+) 0 (zipWith (*) a b)

-- | Função principal
main :: IO()
main = do
  print(produtoEscalar [1,2,3] [4,5,6])