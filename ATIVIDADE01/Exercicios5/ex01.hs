{-
Module      : Módulo principal
License     : GPL version 3 or later (see http://www.gnu.org/copyleft/gpl.html)
Maintainer  : Rafael Guimarães Sakurai - rafaelsakurai.github.io

Exercício 01: Resolva o problema da Zebra (https://en.wikipedia.org/wiki/Zebra_Puzzle) 
utilizando ADTs para representar as soluções. Para encontrar a resposta você deve enumerar 
todas as combinações até que encontre uma que atenda todas as restrições.
-}
module Main where

import Data.List

data Color = Red | Green | Ivory | Yellow | Blue
  deriving (Show, Enum, Eq)

data Nationality = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
  deriving (Show, Enum, Eq)

data Drink = Coffee | Tea | Milk | OrangeJuice | Water
  deriving (Show, Enum, Eq)

data Smoke = OldGold | Kools | Chesterfield | LuckyStrike | Parliament
  deriving (Show, Enum, Eq)

data Pet = Dog | Snails | Fox | Horse | Zebra
  deriving (Show, Enum, Eq)

data House = House {color :: Color,
                    nationality :: Nationality,
                    drink :: Drink,
                    smoke :: Smoke, pet :: Pet}
  deriving (Show, Eq)

-- | The Englishman lives in the red house.
englishmanRedHouse :: [House] -> Bool
englishmanRedHouse houses = or [color (houses !! x) == Red && nationality (houses !! x) == Englishman | x <- [0 .. length houses - 1]]

-- | The Spaniard owns the dog.
spaniardDog :: [House] -> Bool
spaniardDog houses = or [nationality (houses !! x) == Spaniard && pet (houses !! x) == Dog | x <- [0 .. length houses - 1]]

-- | Coffee is drunk in the green house.
coffeeGreenHouse :: [House] -> Bool
coffeeGreenHouse houses = or [drink (houses !! x) == Coffee && color (houses !! x) == Green | x <- [0 .. length houses - 1]]

-- | The Ukrainian drinks tea.
ukrainianTea :: [House] -> Bool
ukrainianTea houses = or [nationality (houses !! x) == Ukrainian && drink (houses !! x) == Tea | x <- [0 .. length houses - 1]]

-- | The green house is immediately to the right of the ivory house.
greenHouseRightIvoryHouse :: [House] -> Bool
greenHouseRightIvoryHouse houses = or [color (houses !! x) == Ivory && color (houses !! (x + 1)) == Green | x <- [0 .. length houses - 2]]

-- | The Old Gold smoker owns snails.
oldGoldSnails :: [House] -> Bool
oldGoldSnails  houses = or [smoke (houses !! x) == OldGold && pet (houses !! x) == Snails | x <- [0 .. length houses - 1]]

-- | Kools are smoked in the yellow house.
koolsYellowHouse :: [House] -> Bool
koolsYellowHouse  houses = or [smoke (houses !! x) == Kools && color (houses !! x) == Yellow | x <- [0 .. length houses - 1]]

-- | Milk is drunk in the middle house.
milkMiddleHouse :: [House] -> Bool
milkMiddleHouse houses
  | drink (houses !! 2) == Milk = True
  | otherwise = False

-- | The Norwegian lives in the first house.
norwegianFirstHouse :: [House] -> Bool
norwegianFirstHouse houses
  | nationality (houses !! 0) == Norwegian = True
  | otherwise = False

-- | Verifica se uma casa está próxima da outra, levando em consideração o cigarro e o animal de estimação.
isNext :: [House] -> Smoke -> Pet -> Bool
isNext houses s p = or [((smoke (houses !! x) == s && pet (houses !! (x + 1)) == p) || (smoke (houses !! (x + 1)) == s && pet (houses !! x) == p)) | x <- [0 .. length houses - 2]]

-- | The man who smokes Chesterfields lives in the house next to the man with the fox.
chesterfieldsNextFox :: [House] -> Bool
chesterfieldsNextFox houses = isNext houses Chesterfield Fox

-- | Kools are smoked in the house next to the house where the horse is kept.
koolsNextHorse :: [House] -> Bool
koolsNextHorse houses = isNext houses Kools Horse

-- | The Lucky Strike smoker drinks orange juice.
luckyStrikeOrangeJuice :: [House] -> Bool
luckyStrikeOrangeJuice houses = or [smoke (houses !! x) == LuckyStrike && drink (houses !! x) == OrangeJuice | x <- [0 .. length houses - 1]]

-- | The Japanese smokes Parliaments.
japaneseParliaments :: [House] -> Bool
japaneseParliaments houses = or [nationality (houses !! x) == Japanese && smoke (houses !! x) == Parliament | x <- [0 .. length houses - 1]]

-- | The Norwegian lives next to the blue house.
norwegianNextBlueHouse :: [House] -> Bool
norwegianNextBlueHouse houses = or [((nationality (houses !! x) == Norwegian && color (houses !! (x + 1)) == Blue) || (nationality (houses !! (x + 1)) == Norwegian && color (houses !! x) == Blue)) | x <- [0 .. length houses - 2]]

-- | Verifica se uma combinação de casas atende as regras do problema da Zebra.
puzzle :: [House] -> Bool
puzzle houses 
  | length houses < 5 = False
  | otherwise = and [englishmanRedHouse houses,
                     spaniardDog houses,
                     coffeeGreenHouse houses,
                     ukrainianTea houses,
                     greenHouseRightIvoryHouse houses,
                     oldGoldSnails houses,
                     koolsYellowHouse houses,
                     milkMiddleHouse houses,
                     norwegianFirstHouse houses,
                     chesterfieldsNextFox houses,
                     koolsNextHorse houses,
                     luckyStrikeOrangeJuice houses,
                     japaneseParliaments houses,
                     norwegianNextBlueHouse houses]

generateEnumValues :: (Enum a) => [a]
generateEnumValues = enumFrom (toEnum 0)

colors :: [Color]
colors = generateEnumValues

nationalities :: [Nationality]
nationalities = generateEnumValues

drinks :: [Drink]
drinks = generateEnumValues

smokes :: [Smoke]
smokes = generateEnumValues

pets :: [Pet]
pets = generateEnumValues

-- | Monta um conjunto de vetores de casas de acordo com os vetores de configurações recebidos.
makeHouses :: ([Color], [Nationality], [Drink], [Smoke], [Pet]) -> [House]
makeHouses (c, n, d, s, p) = [House (c !! x) (n !! x) (d !! x) (s !! x) (p !! x) | x <- [0.. length c - 1]]

combinations cs ns ds ss ps = [makeHouses (c, n, d, s, p) | c <- cs, n <- ns, d <- ds, s <- ss, p <- ps]

-- | Testa as combinações de casas e devolve a combinação que atende todas as regras.
test :: [[House]] -> [House]
test x = test' x 0
  where
    test' x i
      | i == (length x - 1) = Nothing
      | puzzle (x !! i) == True = Just (x !! i)
      | otherwise = test' x (i + 1)

-- | Função principal
main :: IO()
main = do
  -- Teste que não atende as regras do Zebra Puzzle
  print(puzzle [(House Red Spaniard Tea Kools Zebra), 
                (House Yellow Norwegian Coffee OldGold Horse), 
                (House Blue Englishman Water Chesterfield Dog), 
                (House Green Japanese OrangeJuice Parliament Snails), 
                (House Ivory Ukrainian Milk LuckyStrike Fox)])

  -- Teste que atende todas as regras do Zebra Puzzle
  print(puzzle [(House Yellow Norwegian Water Kools Fox), 
                (House Blue Ukrainian Tea Chesterfield Horse), 
                (House Red Englishman Milk OldGold Snails), 
                (House Ivory Spaniard OrangeJuice LuckyStrike Dog), 
                (House Green Japanese Coffee Parliament Zebra)])

  let cores = permutations $ colors
  let nacionalidades = permutations $ nationalities
  let bebidas = permutations $ drinks
  let cigarros = permutations $ smokes
  let animais = permutations $ pets

  -- Gera todas as combinações possíveis
  let combinacoes = combinations cores nacionalidades bebidas cigarros animais
  -- Verifica qual combinação está correta
  let resposta = test combinacoes
  print(resposta)