{-|
Module      : Módulo principal
License     : GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
Maintainer  : Rafael Guimarães Sakurai - rafaelsakurai.github.io

Exercício 01: Execute as seguintes operações utilizando o menor número de parênteses:
 2⋅3+5
 2+2⋅3+1
 3^4+5⋅2^5+1
-}
module Main where

-- | Função principal
main :: IO()
main = do
  print (2 * 3 + 5)
  print (2 + 2 * 3 + 5)
  print (3 ^ 4 + 5 * 2 ^ 5 + 1)