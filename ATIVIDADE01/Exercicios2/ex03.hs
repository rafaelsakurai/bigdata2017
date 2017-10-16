{-
Module      : Módulo principal
License     : GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
Maintainer  : Rafael Guimarães Sakurai - rafaelsakurai.github.io

Exercício 03: Implemente uma função que faz a multiplicação etíope entre dois números.
-}
module Main where

-- | Função que verifica se um número é par ou impar
impar :: Integer -> Bool
impar x = (x `rem` 2) /= 0

-- | Função que calcula a multiplicação etíope entre dois números.
etiope :: Integer -> Integer -> Integer
etiope m n = etiope' m n 0
  where
    etiope' 1 n r = n + r
    etiope' m n r 
      | impar m = etiope' (m `quot` 2) (n * 2) (n + r)
      | otherwise = etiope' (m `quot` 2) (n * 2) r

-- | Função principal
main :: IO()
main = do
  print (etiope 14 12)