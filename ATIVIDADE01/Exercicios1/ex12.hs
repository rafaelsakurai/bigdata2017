{-
Module      : Módulo principal
License     : GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
Maintainer  : Rafael Guimarães Sakurai - rafaelsakurai.github.io

Exercício 12: Dada a string “0123456789”, crie uma lista com os dígitos em formato Integer.
-}
module Main where

-- | Função que recebe uma String e devolve uma lista com os dígitos em formato Integer.
separarNumeros :: String -> [Integer]
separarNumeros texto = [read [c] :: Integer | c <- texto]

-- | Função principal
main :: IO()
main = do
  print (separarNumeros "0123456789")