{-
Module      : Módulo principal
License     : GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
Maintainer  : Rafael Guimarães Sakurai - rafaelsakurai.github.io

Exercício 06: Faça uma função que calcule a persistência aditiva de um número.
-}
module Main where

-- | Função que soma os dígitos de um número.
somaDigitos :: Integer -> Integer
somaDigitos num = somaDigitos' num 0
  where
    somaDigitos' 0 soma = soma
    somaDigitos' num soma = somaDigitos' (num `quot` 10) (num `rem` 10 + soma)

-- | Função que calcula a persistência aditiva de um número.
persistenciaAditiva :: Integer -> Integer
persistenciaAditiva num = persistenciaAditiva' num 0
  where
    persistenciaAditiva' num count 
      | num < 10 = count
      | otherwise = persistenciaAditiva' (somaDigitos num) (count + 1)

-- | Função principal
main :: IO()
main = do
  print (persistenciaAditiva 3)
  print (persistenciaAditiva 10)
  print (persistenciaAditiva 59)
  print (persistenciaAditiva 160)
  print (persistenciaAditiva 12345)