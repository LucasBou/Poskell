{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Main (main) where
import Test.Hspec (Spec, describe, hspec, it, shouldBe,shouldSatisfy, Expectation)
import Cards (Value(..), Color(..), Card(..), Hand(..), isFlush, isStraight, isFourOfAKind, isThreeOfAKind, isPair,isTwoPairs, isFull)


main :: IO ()
main = hspec globalSpec

globalSpec :: Spec
globalSpec = do
  describe "The Poskell tests for the library" $ do
    dummyTest
    queenBetterThanJack
    queenDiamondEqualQueenHeart
    aceHeartNotEqualToEightClubs
    tenClubsBetterThanthreeHearts
    detectFlush
    detectNotFlush1
    detectNotFlush2
    detectStraight1
    detectStraight2
    detectNotStraight1
    detectNotStraight2
    detectFlushStraight
    detectNotFlushStraight
    detectFourOfAKind
    detectNotFourOfAKind
    detectNotThreeOfAKind
    detectThreeOfAKind
    detectPair
    detectNotPair
    detectTwoPairs
    detectNotTwoPairs
    detectFull
    detectNotFull
    detectStraightWithHoles 
    detectLowestStraight 
    pairBetterThanHighCard 
    pairOfKingsBetterThanPairOfQueens 
shouldBeGreaterThan :: (Ord a, Show a) => a -> a -> Expectation
shouldBeGreaterThan a b = a `shouldSatisfy` (>b)

dummyTest = it "ensures the test suite runs" $ do
  1 `shouldBe` 1

queenBetterThanJack = it "ensures a queen beats a jack" $ do
    Queen `shouldBeGreaterThan` Jack

queenDiamondEqualQueenHeart =  it "ensures that a queen diamond is equal to queen heart" $ do
    Card Diamonds Queen == Card Hearts Queen `shouldBe` True

aceHeartNotEqualToEightClubs = it "ensures that a ace heart is not equal to eight clubs" $ do
    Card Hearts Ace /= Card Clubs  Eight `shouldBe` True

tenClubsBetterThanthreeHearts = it "ensures that 10 clubs us better than three hearts" $ do
  Card Clubs Ten> Card Hearts Three `shouldBe` True

detectFlush = it "ensures that 2, 3, 7,8, and 10 of spades is a flush" $ do
  isFlush [Card Spades Two, Card Spades Three, Card Spades Seven, Card Spades Eight, Card Spades Ten] `shouldBe` True

detectNotFlush1 = it "ensures that 2, 3, 7,8 of spades + Jack of heart is not a flush" $ do
  isFlush [Card Spades Two, Card Spades Three, Card Spades Seven, Card Spades Eight, Card Hearts Jack] `shouldBe` False

detectNotFlush2 = it "ensures that 2 of heart + ace of spades + king of clubs + queen of diamons + Jack of heart is not a flush" $ do
  isFlush [Card Hearts Two, Card Spades Ace, Card Clubs King, Card Diamonds Queen, Card Hearts Jack] `shouldBe` False

detectStraight1 = it "ensures that 2 of heart, 3 of spades, 4 of diamonds, 5 of clubs and 6 of spades is a straight" $ do
  isStraight [Card Hearts Two, Card Spades Three, Card Diamonds Four, Card Clubs Five, Card Spades Six] `shouldBe` True

detectStraight2 = it "ensures that 10 of heart, jack of spades, queen of diamonds, king of clubs and ace of spades is a straight" $ do
  isStraight [Card Hearts Ten, Card Spades Jack, Card Diamonds Queen, Card Clubs King, Card Spades Ace] `shouldBe` True

detectNotStraight1 = it "ensures that 2 of heart, 3 of spades, 4 of diamonds, Jack of clubs and 6 of spades is a straight" $ do
  isStraight [Card Hearts Two, Card Spades Three, Card Diamonds Four, Card Clubs Jack, Card Spades Six] `shouldBe` False

detectNotStraight2 = it "ensures that 10 of heart, jack of spades, queen of diamonds, king of clubs and two of spades is a straight" $ do
  isStraight [Card Hearts Ten, Card Spades Jack, Card Diamonds Queen, Card Clubs King, Card Spades Two] `shouldBe` False

detectFlushStraight = it "ensures that 2, 3, 4,5, and 6 of diamonds is a straight flush" $ do
  isFlush [Card Diamonds Two, Card Diamonds Three, Card Diamonds Four, Card Diamonds Five, Card Diamonds Six] `shouldBe` True

detectNotFlushStraight = it "ensures that 2, 3, 4,5 of diamonds + 6 of Spades is not a straight flush" $ do
  isFlush [Card Diamonds Two, Card Diamonds Three, Card Diamonds Four, Card Diamonds Five, Card Spades Six] `shouldBe` False

detectFourOfAKind = it "ensures that 2 of hearts, diamons, spades, clubs + 3 of spades is four of a kind" $ do
  isFourOfAKind [Card Diamonds Two, Card Spades Two, Card Clubs Two, Card Hearts Two, Card Spades Three] `shouldBe` True

detectNotFourOfAKind = it "ensures that 2 of hearts, diamons, spades, + 5 of clubs + 3 of spades is four of a kind" $ do
  isFourOfAKind [Card Diamonds Two, Card Spades Two, Card Clubs Two, Card Clubs Five, Card Spades Three] `shouldBe` False

detectNotThreeOfAKind = it "ensures that 2 of hearts, diamons, Jack of spades, queen of clubs + 3 of spades is not three of a kind" $ do
  isThreeOfAKind [Card Diamonds Two, Card Spades Jack, Card Clubs Queen, Card Hearts Two, Card Spades Three] `shouldBe` False

detectThreeOfAKind = it "ensures that 2 of hearts, diamons, spades, + 5 of clubs + 3 of spades is three of a kind" $ do
  isThreeOfAKind [Card Diamonds Two, Card Spades Two, Card Clubs Two, Card Clubs Five, Card Spades Three] `shouldBe` True

detectPair = it "ensures that Jack of spades and hearts + 3 of clubs + king Diamonds + Ace of spades" $ do
  isPair [Card Spades Jack, Card Hearts Jack, Card Clubs Three, Card Diamonds King, Card Spades Ace] `shouldBe` True

detectNotPair= it "ensures that Jack of spades + seven of spades, hearts + 3 of clubs + king Diamonds + Ace of spades" $ do
  isPair [Card Spades Jack, Card Spades Seven, Card Clubs Three, Card Diamonds King, Card Spades Ace] `shouldBe` False

detectTwoPairs = it "ensures that Jack of spades and hearts + 3 of clubs and diamonds + Ace of spades" $ do
  isTwoPairs [Card Spades Jack, Card Hearts Jack, Card Clubs Three, Card Diamonds Three, Card Spades Ace] `shouldBe` True

detectNotTwoPairs =it "ensures that Jack of spades and hearts + 3 of clubs + king Diamonds + Ace of spades" $ do
  isTwoPairs [Card Spades Jack, Card Hearts Jack, Card Clubs Three, Card Diamonds King, Card Spades Ace] `shouldBe` False

detectFull= it "ensures that Jack of spades and hearts and diamonds + 3 of clubs and diamonds " $ do
  isFull [Card Spades Jack, Card Hearts Jack, Card Diamonds Jack, Card Diamonds Three, Card Clubs Three] `shouldBe` True

detectNotFull = it "ensures that Jack of spades and hearts and diamonds + 3 of diamonds + 2 of clubs " $ do
  isFull [Card Spades Jack, Card Hearts Jack, Card Diamonds Jack, Card Diamonds Three, Card Clubs Two] `shouldBe` False

detectStraightWithHoles = it "ensures that quads of 2s and a six is not a straight" $ do
  isStraight [Card Spades Two, Card Hearts Two, Card Diamonds Two, Card Clubs Two, Card Clubs Six] `shouldBe` False
detectLowestStraight = it "ensures that a series of Ace to 4 is a straight" $ do
  isStraight [Card Spades Ace, Card Hearts Two, Card Diamonds Three, Card Clubs Four, Card Clubs Five] `shouldBe` True
  Card Clubs Ten `shouldBeGreaterThan` Card Hearts Three

pairBetterThanHighCard = it "ensures a pair wins over a high card" $ do
  Pair (Jack) `shouldBeGreaterThan` High King
pairOfKingsBetterThanPairOfQueens = it "ensures a pair of kings wins over a pair of queens" $ do
  Pair King `shouldBeGreaterThan` Pair Queen
