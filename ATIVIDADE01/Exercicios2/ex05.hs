{-
Module      : Módulo principal
License     : GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
Maintainer  : Rafael Guimarães Sakurai - rafaelsakurai.github.io

Exercício 05: Faça uma função que calcule a soma dos dígitos de um número.
-}
module Main where

-- | Função que soma os dígitos de um número.
somaDigitos :: Integer -> Integer
somaDigitos num = somaDigitos' num 0
  where
    somaDigitos' 0 soma = soma
    somaDigitos' num soma = somaDigitos' (num `quot` 10) (num `rem` 10 + soma)

-- | Função principal
main :: IO()
main = do
  print (somaDigitos 10)
  print (somaDigitos 59)
  print (somaDigitos 160)
  print (somaDigitos 12345)