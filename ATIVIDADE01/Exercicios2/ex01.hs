{-
Module      : Módulo principal
License     : GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
Maintainer  : Rafael Guimarães Sakurai - rafaelsakurai.github.io

Exercício 01: Crie uma função ehTriangulo que determina se três lados x, y, z 
podem formar um triângulo.
-}
module Main where

-- | Função que verifica se os três lados x, y e z podem formar um triângulo.
ehTriangulo :: Double -> Double -> Double -> Bool
ehTriangulo x y z = (x /= y && x /= z && y /= z)

-- | Função principal
main :: IO()
main = do
  print (ehTriangulo 1.0 2.0 3.0)
  print (ehTriangulo 1.0 1.0 3.0)
  print (ehTriangulo 1.0 2.0 2.0)
  print (ehTriangulo 1.0 1.0 1.0)