module Main (main) where
import Test.Hspec (Spec, describe, hspec, it, shouldBe, shouldSatisfy, Expectation)
import Cards (Value(..), Color(..), Card(..), Hand(..))

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
    Card Hearts Ace /= Card Clubs  (Num 8) `shouldBe` True

tenClubsBetterThanthreeHearts = it "ensures that 10 clubs us better than three hearts" $ do
  Card Clubs (Num 10) `shouldBeGreaterThan` Card Hearts (Num 3)

pairBetterThanHighCard = it "ensures a pair wins over a high card" $ do
  Pair (Jack) `shouldBeGreaterThan` High King
pairOfKingsBetterThanPairOfQueens = it "ensures a pair of kings wins over a pair of queens" $ do
  Pair King `shouldBeGreaterThan` Pair Queen