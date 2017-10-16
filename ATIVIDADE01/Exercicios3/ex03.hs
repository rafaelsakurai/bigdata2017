{-
Module      : Módulo principal
License     : GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
Maintainer  : Rafael Guimarães Sakurai - rafaelsakurai.github.io

Exercício 03: Crie a lista de números de Fibonacci utilizando uma função geradora.
-}
module Main where

-- | Função principal
main :: IO()
main = do
  -- | Cria uma lista com números de Fibonacci.
  let fib = 1 : 2 : prox fib where prox (x : t@(y : _)) = (x + y) : prox t
  print(take 10 fib)