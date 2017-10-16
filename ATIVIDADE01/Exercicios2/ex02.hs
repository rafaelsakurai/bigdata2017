{-
Module      : Módulo principal
License     : GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
Maintainer  : Rafael Guimarães Sakurai - rafaelsakurai.github.io

Exercício 02: Crie uma função tipoTriangulo que determina o tipo do triângulo formado 
pelos três lados x, y, z.
-}
module Main where

-- | Função que verifica o tipo de um triângulo a partir dos seus três lados x, y e z.
tipoTriangulo :: Double -> Double -> Double -> String
tipoTriangulo x y z 
  | (x == y && x == z && y == z) = "Equilatero"
  | ((x == y || x == z || y == z)) && (x /= y || x /= z || y /= z) = "Isosceles"
  | (x /= y && x /= z && y /= z) = "Escaleno"

-- | Função principal
main :: IO()
main = do
  print (tipoTriangulo 1.0 2.0 3.0)
  print (tipoTriangulo 1.0 1.0 3.0)
  print (tipoTriangulo 1.0 2.0 2.0)
  print (tipoTriangulo 1.0 1.0 1.0)