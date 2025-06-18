module Hands where

import Cards
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE

type Cards = NonEmpty Card

newtype Kickers = Kickers [Value] deriving (Eq, Ord, Show)

kickers :: [Value] -> Kickers
kickers = Kickers . sort

noKicker :: Kickers
noKicker = kickers []

data Hand = High Value Kickers | Pair Value Kickers | TwoPair Value Value Kickers | ThreeOfAKind Value Kickers | Straight Value | Flush Value | FullHouse Value Value | FourOfAKind Value Kickers | StraightFlush Value deriving (Eq, Ord, Show)

sortByColor :: Cards -> NonEmpty Color
sortByColor = NE.sort . fmap color

sortByValue :: Cards -> NonEmpty Value
sortByValue = NE.sort . fmap value

highestCard :: Cards -> Value
highestCard = NE.last . sortByValue

isFlush :: Cards -> Bool
isFlush cards = NE.head sorted == sorted NE.!! 4
  where
    sorted = sortByColor cards

isStraight :: Cards -> Bool
isStraight cards = communStraight || lowStraight
  where
    sorted = sortByValue cards
    communStraight = NE.tail sorted == fmap succ (NE.init sorted)
    lowStraight = sorted == NE.fromList [Two, Three, Four, Five, Ace]

isFlushStraight :: Cards -> Bool
isFlushStraight cards = isStraight cards && isFlush cards

isFourOfAKind :: Cards -> Bool
isFourOfAKind cards = (NE.head sorted == sorted NE.!! 3) || (sorted NE.!! 1 == sorted NE.!! 4)
  where
    sorted = sortByValue cards

isThreeOfAKind :: Cards -> Bool
isThreeOfAKind cards = (NE.head sorted == sorted NE.!! 2) || (sorted NE.!! 1 == sorted NE.!! 3) || (sorted NE.!! 2 == sorted NE.!! 4)
  where
    sorted = sortByValue cards

isPair :: Cards -> Bool
isPair cards = (NE.head sorted == sorted NE.!! 1) || (sorted NE.!! 1 == sorted NE.!! 2) || (sorted NE.!! 2 == sorted NE.!! 3) || (sorted NE.!! 3 == sorted NE.!! 4)
  where
    sorted = sortByValue cards

isTwoPairs :: Cards -> Bool
isTwoPairs cards = not (isFourOfAKind cards) && (((NE.head sorted == sorted NE.!! 1) && (sorted NE.!! 2 == sorted NE.!! 3)) || ((NE.head sorted == sorted NE.!! 1) && (sorted NE.!! 3 == sorted NE.!! 4)) || ((sorted NE.!! 3 == sorted NE.!! 4) && (sorted NE.!! 2 == sorted NE.!! 3)))
  where
    sorted = sortByValue cards

isFull :: Cards -> Bool
isFull cards = ((NE.head sorted == sorted NE.!! 2) && (sorted NE.!! 3 == sorted NE.!! 4)) || ((NE.head sorted == sorted NE.!! 1) && (sorted NE.!! 2 == sorted NE.!! 4))
  where
    sorted = sortByValue cards
