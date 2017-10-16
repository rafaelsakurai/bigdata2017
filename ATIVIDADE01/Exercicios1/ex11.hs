{-
Module      : Módulo principal
License     : GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
Maintainer  : Rafael Guimarães Sakurai - rafaelsakurai.github.io

Exercício 11: Crie um concatenador de strings que concatena duas strings separadas por espaço.
-}
module Main where

-- | Função que concatena duas Strings separadas por um espaço.
concatenar :: String -> String -> String
concatenar a b = a ++ (' ' : b)

-- | Função principal
main :: IO()
main = do
  print (concatenar "Rafael" "Sakurai")