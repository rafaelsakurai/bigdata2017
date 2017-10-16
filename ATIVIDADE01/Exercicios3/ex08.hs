{-
Module      : Módulo principal
License     : GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
Maintainer  : Rafael Guimarães Sakurai - rafaelsakurai.github.io

Exercício 08: Encontre o número x entre 1 e 1.000.000 que tem a maior sequência de Collatz.
(Project Euler 14)
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
  -- | Calcula o collatzLen para todos os números entre 1 e 1.000.000 e monta uma tupla (tamanho, x).
  let collatzLista = [(collatzLen x, x) | x <- [1 .. 1000000]]
  -- | Pega a tupla com o maior tamanho.
  let maior = maximum collatzLista
  -- | Imprime o valor de x que contém o maior collatzLen.
  print(snd maior)