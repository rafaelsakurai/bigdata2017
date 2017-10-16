{-
Module      : Módulo principal
License     : GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
Maintainer  : Rafael Guimarães Sakurai - rafaelsakurai.github.io

Exercício 09: Encontre os 10 primeiros anos bissextos.
          09: Encontre os 10 últimos anos bissextos (dica: use a função length para 
          determinar o tamanho da lista).
-}
module Main where

-- | Função que recebe o ano e devolve um True se o ano for bissexto ou False caso contrário.
bissexto :: Integer -> Bool
bissexto ano = (ano `rem` 400 == 0) || ((ano `rem` 4 == 0) && (ano `rem` 100 /= 0))

-- | Função principal
main :: IO()
main = do
  let anosBissestos = [i | i <- [1 .. 2017], bissexto i]
  print (drop (length anosBissestos - 10) anosBissestos)