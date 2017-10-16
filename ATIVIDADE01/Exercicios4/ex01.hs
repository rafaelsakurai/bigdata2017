{-
Module      : Módulo principal
License     : GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
Maintainer  : Rafael Guimarães Sakurai - rafaelsakurai.github.io

Exercício 01: Faça uma função que gere uma matriz identidade de tamanho n.
-}
module Main where

-- | Função que converte um booleano True para 1 e False para 0.
bool2Int :: Bool -> Integer
bool2Int x
  | x == True = 1
  | otherwise = 0

-- | Função que monta uma matriz identidade de tamanho n.
matrizIdentidade :: Integer -> [[Integer]]
matrizIdentidade n = [[bool2Int(coluna == linha) | coluna <- [1..n]] | linha <- [1..n]]

-- | Função principal
main :: IO()
main = do
  print(matrizIdentidade 4)