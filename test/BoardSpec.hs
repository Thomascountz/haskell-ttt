module BoardSpec (spec) where

import Test.Hspec
import Board

spec :: Spec
spec =  do
  describe "initBoard" $ do
    it "returns a board with a length of 9" $
      length initBoard `shouldBe` 9
    it "returns a board full of Empty" $
      (foldr (&&) True (map (\cell -> cell == Empty) initBoard)) `shouldBe` True
