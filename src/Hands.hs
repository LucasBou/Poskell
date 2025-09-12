module Hands where

import Cards
import Control.Applicative ((<|>))
import Control.Monad (join)
import Data.List (find, maximumBy, sort, uncons)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust, listToMaybe)
import Data.Ord (Down(..), comparing)

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
isFlush = isJust . toFlush

toFlush :: Cards -> Maybe Hand
toFlush = fmap (Flush . maximum . fmap value . fst) . uncons . NE.filter ((> 4) . length) . NE.groupWith1 color

toCommonStraight :: Cards -> Maybe Hand
toCommonStraight cards = if NE.tail sorted == fmap succ (NE.init sorted) then Just $ Straight $ NE.head sorted else Nothing
  where
    sorted = sortByValue cards

toLowStraight :: Cards -> Maybe Hand
toLowStraight cards = if sorted == NE.fromList [Two, Three, Four, Five, Ace] then Just $ Straight Five else Nothing
  where
    sorted = sortByValue cards

toStraight :: Cards -> Maybe Hand
toStraight = liftA2 (<|>) toCommonStraight toLowStraight

isStraight :: Cards -> Bool
isStraight = isJust . toStraight

toStraightFlush :: Cards -> Maybe Hand
toStraightFlush = join . (liftA2 returnsifequal <$> toStraight <*> toFlush)
  where
    returnsifequal a b = if a == b then Just a else Nothing

isFlushStraight :: Cards -> Bool
isFlushStraight = isJust . toStraightFlush

toKinds :: Cards -> Maybe Hand
toKinds cs = case lengths of
  4 NE.:| _ -> Just $ FourOfAKind extractedDominant extractedKickersWithoutSecondary
  3 NE.:| 2 : _ -> Just $ FullHouse extractedDominant extractedSecondary
  3 NE.:| _ -> Just $ ThreeOfAKind extractedDominant extractedKickersWithoutSecondary
  2 NE.:| 2 : _ -> Just $ TwoPair extractedDominant extractedSecondary extractedKickersWithSecondary
  2 NE.:| _ -> Just $ Pair extractedDominant extractedKickersWithoutSecondary
  1 NE.:| _ -> Just $ High extractedDominant extractedKickersWithoutSecondary
  _ -> Nothing
  where
    grouped = NE.sortBy (comparing (Down . NE.length)) $ NE.groupAllWith1 value cs
    lengths = fmap NE.length grouped
    extractedDominant = NE.head (NE.head (fmap (fmap value) grouped))
    extractedSecondary = NE.head (fmap (fmap value) grouped NE.!! 1)
    extractedKickersWithoutSecondary = kickers $ fmap value $ NE.toList $ grouped NE.!! 1
    extractedKickersWithSecondary = kickers $ fmap value $ NE.toList $ grouped NE.!! 2

isFourOfAKind :: Cards -> Bool
isFourOfAKind cs = case toKinds cs of
  Just (FourOfAKind _ _) -> True
  _ -> False

-- isFourOfAKind cards = (NE.head sorted == sorted NE.!! 3) || (sorted NE.!! 1 == sorted NE.!! 4)
--   where
--     sorted = sortByValue cards

isThreeOfAKind :: Cards -> Bool
isThreeOfAKind cs = case toKinds cs of
  Just (ThreeOfAKind _ _) -> True
  _ -> False

-- isThreeOfAKind cards = (NE.head sorted == sorted NE.!! 2) || (sorted NE.!! 1 == sorted NE.!! 3) || (sorted NE.!! 2 == sorted NE.!! 4)
--   where
--     sorted = sortByValue cards

isPair :: Cards -> Bool
isPair cs = case toKinds cs of
  Just (Pair _ _) -> True
  _ -> False

-- isPair cards = (NE.head sorted == sorted NE.!! 1) || (sorted NE.!! 1 == sorted NE.!! 2) || (sorted NE.!! 2 == sorted NE.!! 3) || (sorted NE.!! 3 == sorted NE.!! 4)
--   where
--     sorted = sortByValue cards

isTwoPairs :: Cards -> Bool
isTwoPairs cards = not (isFourOfAKind cards) && (((NE.head sorted == sorted NE.!! 1) && (sorted NE.!! 2 == sorted NE.!! 3)) || ((NE.head sorted == sorted NE.!! 1) && (sorted NE.!! 3 == sorted NE.!! 4)) || ((sorted NE.!! 3 == sorted NE.!! 4) && (sorted NE.!! 2 == sorted NE.!! 3)))
  where
    sorted = sortByValue cards

isFull :: Cards -> Bool
isFull cards = ((NE.head sorted == sorted NE.!! 2) && (sorted NE.!! 3 == sorted NE.!! 4)) || ((NE.head sorted == sorted NE.!! 1) && (sorted NE.!! 2 == sorted NE.!! 4))
  where
    sorted = sortByValue cards
