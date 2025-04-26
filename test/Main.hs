module Main (main) where
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Cards (Value(..), Color(..), Card(..))

main :: IO ()
main = hspec globalSpec

globalSpec :: Spec
globalSpec = do
  describe "The Poskell tests for the library" $ do
    dummyTest
    queenBetterThanJack
    queenDiamondEqualQueenHeart

dummyTest = it "ensures the test suite runs" $ do
  1 `shouldBe` 1

queenBetterThanJack = it "ensures a queen beats a jack" $ do
    Jack < Queen  `shouldBe` True

queenDiamondEqualQueenHeart =  it "ensures that a queen diamond is equal to queen heart" $ do
    Card Diamonds Queen == Card Hearts Queen `shouldBe` True