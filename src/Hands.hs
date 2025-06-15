module Hands where

import Cards
import Data.List (sort, sortOn)

type Cards = [Card]

newtype Kickers = Kickers [Value] deriving (Eq, Ord, Show)

kickers :: [Value] -> Kickers
kickers = Kickers . sort

noKicker :: Kickers
noKicker = kickers []

data Hand = High Value Kickers | Pair Value Kickers | TwoPair Value Value Kickers | ThreeOfAKind Value Kickers | Straight Value | Flush Value | FullHouse Value Value | FourOfAKind Value Kickers | StraightFlush Value deriving (Eq, Ord, Show)

extractListColors :: Cards -> [Color]
extractListColors = fmap color

extractListValues :: Cards -> [Value]
extractListValues = fmap value

sortByColor :: Cards -> [Color]
sortByColor cards = sort (extractListColors cards)

sortByValue :: Cards -> [Value]
sortByValue = fmap value . sortOn value

highest_card :: Cards -> Value
highest_card cards = last (sortByValue (cards))

isFlush :: Cards -> Bool
isFlush cards = sorted !! 0 == sorted !! 4
  where
    sorted = sortByColor cards

isStraight :: Cards -> Bool
isStraight cards = communStraight || lowStraight
  where
    sorted = sortByValue cards
    communStraight = (drop 1 sorted) == (fmap succ (init sorted))
    lowStraight = sorted == [Two, Three, Four, Five, Ace]

isFlushStraight :: Cards -> Bool
isFlushStraight cards = isStraight cards && isFlush cards

isFourOfAKind :: Cards -> Bool
isFourOfAKind cards = (sorted !! 0 == sorted !! 3) || (sorted !! 1 == sorted !! 4)
  where
    sorted = sortByValue cards

isThreeOfAKind :: Cards -> Bool
isThreeOfAKind cards = (sorted !! 0 == sorted !! 2) || (sorted !! 1 == sorted !! 3) || (sorted !! 2 == sorted !! 4)
  where
    sorted = sortByValue cards

isPair :: Cards -> Bool
isPair cards = (sorted !! 0 == sorted !! 1) || (sorted !! 1 == sorted !! 2) || (sorted !! 2 == sorted !! 3) || (sorted !! 3 == sorted !! 4)
  where
    sorted = sortByValue cards

isTwoPairs :: Cards -> Bool
isTwoPairs cards = not (isFourOfAKind cards) && (((sorted !! 0 == sorted !! 1) && (sorted !! 2 == sorted !! 3)) || ((sorted !! 0 == sorted !! 1) && (sorted !! 3 == sorted !! 4)) || ((sorted !! 3 == sorted !! 4) && (sorted !! 2 == sorted !! 3)))
  where
    sorted = sortByValue cards

isFull :: Cards -> Bool
isFull cards = ((sorted !! 0 == sorted !! 2) && (sorted !! 3 == sorted !! 4)) || ((sorted !! 0 == sorted !! 1) && (sorted !! 2 == sorted !! 4))
  where
    sorted = sortByValue cards
