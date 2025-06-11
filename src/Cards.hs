module Cards where
import Data.List
import Debug.Trace 
data Color = Hearts | Spades | Diamonds | Clubs deriving (Eq,Ord,Show)


data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten| Jack | Queen | King | Ace deriving (Ord, Eq,Show, Enum)

data Card = Card { color :: Color, value :: Value} deriving (Show)
instance Eq Card  where 
    Card _ valuea == Card _ valueb = valuea == valueb
    

instance Ord Card where
    Card _ valuea <= Card _ valueb = valuea<=valueb

type Cards = [Card]

extractListColors:: Cards -> [Color]
extractListColors = fmap color

extractListValues:: Cards -> [Value]
extractListValues = fmap value

sortByColor :: Cards->[Color]
sortByColor cards = sort (extractListColors cards)

sortByValue :: Cards -> [Value]
sortByValue = fmap value . sortOn value

highest_card::Cards -> Value
highest_card cards = last (sortByValue(cards))

isFlush :: Cards->Bool
isFlush cards = sorted !!0== sorted !!4
    where sorted = sortByColor cards

isStraight :: Cards->Bool
isStraight cards = communStraight || lowStraight
    where sorted = sortByValue cards
          communStraight = (tail sorted) == (fmap succ (init sorted))
          lowStraight = sorted == [Two, Three, Four, Five, Ace]

isFlushStraight :: Cards->Bool
isFlushStraight cards = isStraight cards && isFlush cards 

isFourOfAKind :: Cards -> Bool
isFourOfAKind cards = (sorted !! 0 == sorted !!3 ) || (sorted !! 1 == sorted !!4 )
    where sorted = sortByValue cards

isThreeOfAKind:: Cards -> Bool
isThreeOfAKind cards = (sorted !! 0 == sorted !!2 ) || (sorted !! 1 == sorted !!3 ) || (sorted !! 2 == sorted !!4 )
    where sorted = sortByValue cards


isPair:: Cards -> Bool
isPair cards = (sorted !! 0 == sorted !!1 ) || (sorted !! 1 == sorted !!2 ) || (sorted !! 2 == sorted !!3 )|| (sorted !! 3 == sorted !!4 )
    where sorted = sortByValue cards

isTwoPairs:: Cards->Bool
isTwoPairs cards = not(isFourOfAKind cards) && (((sorted !! 0 == sorted !!1 ) && (sorted !! 2 == sorted !!3 )) ||((sorted !! 0 == sorted !!1 ) && (sorted !! 3 == sorted !!4 )) ||((sorted !! 3 == sorted !!4 ) && (sorted !! 2 == sorted !!3 )))
    where sorted = sortByValue cards

isFull:: Cards -> Bool
isFull cards = ((sorted !! 0 == sorted !! 2) && (sorted !! 3 == sorted !! 4)) ||((sorted !! 0 == sorted !! 1) && (sorted !! 2 == sorted !! 4))
    where sorted = sortByValue cards

newtype Kickers = Kickers [Value] deriving (Eq, Ord, Show)

kickers :: [Value] -> Kickers
kickers = Kickers . sort

noKicker :: Kickers
noKicker = kickers []

data Hand = High Value Kickers | Pair Value Kickers | TwoPair Value Value Kickers | ThreeOfAKind Value Kickers | Straight Value | Flush Value | FullHouse Value Value | FourOfAKind Value Kickers | StraightFlush Value deriving(Eq, Ord, Show)
