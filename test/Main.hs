module Main (main) where
import Test.Hspec (Spec, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec globalSpec

globalSpec :: Spec
globalSpec = do
  describe "The Poskell tests for the library" $ do
    dummyTest


dummyTest = it "ensures the test suite runs" $ do
  1 `shouldBe` 1