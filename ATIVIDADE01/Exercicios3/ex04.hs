{-
Module      : Módulo principal
License     : GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
Maintainer  : Rafael Guimarães Sakurai - rafaelsakurai.github.io

Exercício 04: Utilizando a lista anterior, calcule a soma dos números de Fibonacci 
pares dos valores que não excedem 4.000.000. (Project Euler 2)
-}
module Main where

-- | Função principal
main :: IO()
main = do
  -- | Cria uma lista com números de Fibonacci.
  let fib = 1 : 2 : prox fib where prox (x : t@(y : _)) = (x + y) : prox t

  -- | Obtem os números de Fibonacci menor que 4.000.000.
  let menor4Mi = takeWhile (\x -> (x < 4000000)) fib
  -- | Obtem os números que são pares. 
  let pares = [x | x <- menor4Mi, x `rem` 2 == 0]
  -- | Soma e imprime todos os números pares.
  print(foldl (+) 0 pares)