{-
Module      : Módulo principal
License     : GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
Maintainer  : Rafael Guimarães Sakurai - rafaelsakurai.github.io

Exercício 07: Faça uma função que receba um ângulo a e retorne uma tupla contendo o seno 
da metade desse ângulo utilizando a identidade:
-}
module Main where

-- | Função que recebe um ângulo e returna o seno da metade desse ângulo utilizando a identidade.
identidade :: Double -> (Double, Double)
identidade x = (- sqrt (1 - cos(x)) / 2, sqrt (1 - cos(x)) / 2)

-- | Função principal
main :: IO()
main = do
  print (identidade 90)