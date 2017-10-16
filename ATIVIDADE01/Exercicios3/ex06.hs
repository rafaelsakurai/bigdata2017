{-
Module      : Módulo principal
License     : GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
Maintainer  : Rafael Guimarães Sakurai - rafaelsakurai.github.io

Exercício 06: Crie a função collatz x que retorna x/2, se x for par e (3x+1) se for ímpar.
-}
module Main where

-- | Função que retorna x/2, se x for par e (3x+1) se for ímpar.
collatz :: Integer -> Integer
collatz x
    | (x `mod` 2 == 0) = x `quot` 2
    | otherwise = 3 * x + 1

-- | Função principal
main :: IO()
main = do
  print([collatz x | x <- [1 .. 10]])