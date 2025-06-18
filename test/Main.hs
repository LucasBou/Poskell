{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main (main) where

import Cards (Card (..), Color (..), Value (..))
import Hands
import Test.Hspec (Expectation, Spec, describe, hspec, it, shouldBe, shouldSatisfy)

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
    samePairShouldBeDifferentiatedWithkickers

shouldBeGreaterThan :: (Ord a, Show a) => a -> a -> Expectation
shouldBeGreaterThan a b = a `shouldSatisfy` (> b)

dummyTest = it "ensures the test suite runs" $ do
  True `shouldBe` True

queenBetterThanJack = it "ensures a queen beats a jack" $ do
  Queen `shouldBeGreaterThan` Jack

queenDiamondEqualQueenHeart = it "ensures that a queen diamond is equal to queen heart" $ do
  Card Diamonds Queen == Card Hearts Queen `shouldBe` True

aceHeartNotEqualToEightClubs = it "ensures that a ace heart is not equal to eight clubs" $ do
  Card Hearts Ace /= Card Clubs Eight `shouldBe` True

tenClubsBetterThanthreeHearts = it "ensures that 10 clubs us better than three hearts" $ do
  Card Clubs Ten > Card Hearts Three `shouldBe` True

detectFlush = it "ensures that 2, 3, 7, 8, and 10 of spades is a flush" $ do
  isFlush ["2♠", "3♠", "7♠", "8♠", "10♠"] `shouldBe` True

detectNotFlush1 = it "ensures that 2, 3, 7, 8 of spades + Jack of heart is not a flush" $ do
  isFlush ["2♠", "3♠", "7♠", "8♠", "J♥"] `shouldBe` False

detectNotFlush2 = it "ensures that 2 of heart + ace of spades + king of clubs + queen of diamonds + Jack of heart is not a flush" $ do
  isFlush ["2♥", "A♠", "K♣", "Q♦", "J♥"] `shouldBe` False

detectStraight1 = it "ensures that 2 of heart, 3 of spades, 4 of diamonds, 5 of clubs and 6 of spades is a straight" $ do
  isStraight ["2♥", "3♠", "4♦", "5♣", "6♠"] `shouldBe` True

detectStraight2 = it "ensures that 10 of heart, jack of spades, queen of diamonds, king of clubs and ace of spades is a straight" $ do
  isStraight ["10♥", "J♠", "Q♦", "K♣", "A♠"] `shouldBe` True

detectNotStraight1 = it "ensures that 2 of heart, 3 of spades, 4 of diamonds, Jack of clubs and 6 of spades is a straight" $ do
  isStraight ["2♥", "3♠", "4♦", "J♣", "6♠"] `shouldBe` False

detectNotStraight2 = it "ensures that 10 of heart, jack of spades, queen of diamonds, king of clubs and two of spades is a straight" $ do
  isStraight ["10♥", "J♠", "Q♦", "K♣", "2♠"] `shouldBe` False

detectFlushStraight = it "ensures that 2, 3, 4,5, and 6 of diamonds is a straight flush" $ do
  isFlush ["2♦", "3♦", "4♦", "5♦", "6♦"] `shouldBe` True

detectNotFlushStraight = it "ensures that 2, 3, 4,5 of diamonds + 6 of Spades is not a straight flush" $ do
  isFlush ["2♦", "3♦", "4♦", "5♦", "6♠"] `shouldBe` False

detectFourOfAKind = it "ensures that 2 of hearts, diamons, spades, clubs + 3 of spades is four of a kind" $ do
  isFourOfAKind ["2♦", "2♠", "2♣", "2♥", "3♠"] `shouldBe` True

detectNotFourOfAKind = it "ensures that 2 of hearts, diamons, spades, + 5 of clubs + 3 of spades is four of a kind" $ do
  isFourOfAKind ["2♦", "2♠", "2♣", "5♣", "3♠"] `shouldBe` False

detectNotThreeOfAKind = it "ensures that 2 of hearts, diamons, Jack of spades, queen of clubs + 3 of spades is not three of a kind" $ do
  isThreeOfAKind ["2♦", "J♠", "Q♣", "2♥", "3♠"] `shouldBe` False

detectThreeOfAKind = it "ensures that 2 of hearts, diamons, spades, + 5 of clubs + 3 of spades is three of a kind" $ do
  isThreeOfAKind ["2♦", "2♠", "2♣", "5♣", "3♠"] `shouldBe` True

detectPair = it "ensures that Jack of spades and hearts + 3 of clubs + king Diamonds + Ace of spades" $ do
  isPair ["J♠", "J♥", "3♣", "K♦", "A♠"] `shouldBe` True

detectNotPair = it "ensures that Jack of spades + seven of spades, hearts + 3 of clubs + king Diamonds + Ace of spades" $ do
  isPair ["J♠", "7♠", "3♣", "K♦", "A♠"] `shouldBe` False

detectTwoPairs = it "ensures that Jack of spades and hearts + 3 of clubs and diamonds + Ace of spades" $ do
  isTwoPairs ["J♠", "J♥", "3♣", "3♦", "A♠"] `shouldBe` True

detectNotTwoPairs = it "ensures that Jack of spades and hearts + 3 of clubs + king Diamonds + Ace of spades" $ do
  isTwoPairs ["J♠", "J♥", "3♣", "K♦", "A♠"] `shouldBe` False

detectFull = it "ensures that Jack of spades and hearts and diamonds + 3 of clubs and diamonds " $ do
  isFull ["J♠", "J♥", "J♦", "3♦", "3♣"] `shouldBe` True

detectNotFull = it "ensures that Jack of spades and hearts and diamonds + 3 of diamonds + 2 of clubs " $ do
  isFull ["J♠", "J♥", "J♦", "3♦", "2♣"] `shouldBe` False

detectStraightWithHoles = it "ensures that quads of 2s and a six is not a straight" $ do
  isStraight ["2♠", "2♥", "2♦", "2♣", "6♣"] `shouldBe` False

detectLowestStraight = it "ensures that a series of Ace to 4 is a straight" $ do
  isStraight ["A♠", "2♥", "3♦", "4♣", "5♣"] `shouldBe` True

pairBetterThanHighCard = it "ensures a pair wins over a high card" $ do
  Pair Jack noKicker `shouldBeGreaterThan` High King noKicker

pairOfKingsBetterThanPairOfQueens = it "ensures a pair of kings wins over a pair of queens" $ do
  Pair King noKicker `shouldBeGreaterThan` Pair Queen noKicker

samePairShouldBeDifferentiatedWithkickers = it "ensures two competing pairs are differientiated through the kickers" $ do
  Pair King (kickers [Queen]) `shouldBeGreaterThan` Pair King (kickers [Two])
