{-
Module      : Módulo principal
License     : GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
Maintainer  : Rafael Guimarães Sakurai - rafaelsakurai.github.io

Exercício 10: Crie uma tupla em que o primeiro elemento tem metade dos anos bissextos 
              e o segundo elemento a outra metade.
-}
module Main where

-- | Função que recebe o ano e devolve um True se o ano for bissexto ou False caso contrário.
bissexto :: Integer -> Bool
bissexto ano = (ano `rem` 400 == 0) || ((ano `rem` 4 == 0) && (ano `rem` 100 /= 0))

-- | Função que divide os anos bissextos meio a meio e devolve uma tupla.
divisaoBissexto :: [Integer] -> ([Integer], [Integer])
divisaoBissexto anosBissestos = (a, b)
  where
    a = take parte1 anosBissestos
    b = take parte2 anosBissestos
    parte1 = length anosBissestos `div` 2
    parte2 = length anosBissestos - parte1

-- | Função principal
main :: IO()
main = do
  let anosBissestos = [i | i <- [1 .. 2017], bissexto i]
  print (divisaoBissexto anosBissestos)