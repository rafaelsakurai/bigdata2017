{-
Module      : Módulo principal
License     : GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
Maintainer  : Rafael Guimarães Sakurai - rafaelsakurai.github.io

Exercício 07: Implemente uma função collatzLen x que retorna o tamanho da lista formada 
pela aplicação repetida de collatz sobre o valor x até que essa chegue no número 1.
-}
module Main where

-- | Função que retorna x/2, se x for par e (3x+1) se for ímpar.
collatz :: Integer -> Integer
collatz x
    | (x `mod` 2 == 0) = x `quot` 2
    | otherwise = 3 * x + 1

-- | Função que retorna o tamanho da lista da aplicação repetida de collatz sobre o valor x até que chegue em 1.
collatzLen :: Integer -> Integer
collatzLen x = collatzLen' (collatz x) 1
  where
    collatzLen' 1 length = length
    collatzLen' x length = collatzLen' (collatz x) (length+1)

-- | Função principal
main :: IO()
main = do
  print([collatzLen x | x <- [1 .. 10]])