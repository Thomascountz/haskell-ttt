module BoardSpec (spec) where

import Test.Hspec
import Board

spec :: Spec
spec =  do
  describe "initBoard" $ do
    it "returns a board with a length of 9" $
      length initBoard `shouldBe` 9

    it "returns a board full of Empty" $
      and (map (\cell -> cell == Empty) initBoard) `shouldBe` True

  describe "replaceCellAt" $ do
    it "returns an empty board if an empty board is given" $
      replaceCellAt [] 0 Player1 `shouldBe` []

    it "returns board if index is out of bounds" $
      replaceCellAt [Empty] 2 Player1 `shouldBe` [Empty]

    it "returns a board with a Player1 Cell at index 0" $
      replaceCellAt [Empty] 0 Player1 !! 0 `shouldBe` Player1

    it "returns a board with a Player2 Cell at index 1" $
      replaceCellAt [Empty, Empty] 1 Player2 !! 1 `shouldBe` Player2

    it "returns a board with a Player Cell at index 8" $
      replaceCellAt [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty] 8 Player1 !! 8 `shouldBe` Player1