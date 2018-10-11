module BoardSpec (spec) where

import Test.Hspec
import Data.List
import Board

spec :: Spec
spec =  do
  describe "initBoard" $ do
    it "returns a square board of the given length" $
      length (initBoard 3) `shouldBe` 9

    it "returns a board full of Empty" $
      and (map (\cell -> cell == Empty) (initBoard 3)) `shouldBe` True
  
  describe "size" $ do
    it "returns the size of the board" $
      size (initBoard 3) `shouldBe` 3

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

  describe "validPlacement" $ do
    it "returns True if board at index is Empty" $
      validPlacement [Empty] 0 `shouldBe` True

    it "returns False if board at index is not Empty" $
      validPlacement [Player1] 0 `shouldBe` False

    it "returns False if board at index is out of bounds" $
      validPlacement [Player1] 2 `shouldBe` False

  describe "win" $ do
    it "returns false if the board does not have a winning combination" $
      win [Player1, Empty, Empty, Player2, Empty, Empty, Player1, Player2, Empty] `shouldBe` False

    it "returns true if board has a winning row combination of Player1" $
      win [Player1, Player1, Player1, Empty, Empty, Empty, Empty, Empty, Empty] `shouldBe` True 

    it "returns true if board has a winning row combination of Player2" $
      win [Player2, Player2, Player2, Empty, Empty, Empty, Empty, Empty, Empty] `shouldBe` True

    it "returns true if board has a winning column combination of Player1" $
      win [Player2, Player2, Player2, Empty, Empty, Empty, Empty, Empty, Empty] `shouldBe` True

    it "returns true if board has a winning column combination of Player2" $
      win [Player2, Player2, Player2, Empty, Empty, Empty, Empty, Empty, Empty] `shouldBe` True

    it "returns true if board has a winning diagonal combination of Player1" $
      win [Player2, Player2, Player2, Empty, Empty, Empty, Empty, Empty, Empty] `shouldBe` True

    it "returns true if board has a winning diagonal combination of Player2" $
      win [Player2, Player2, Player2, Empty, Empty, Empty, Empty, Empty, Empty] `shouldBe` True

  describe "tie" $ do
    it "returns false if the board does not have a tie combination" $
      tie [Player1, Player1, Player1, Empty, Empty, Empty, Empty, Empty, Empty] `shouldBe` False

    it "returns true if the board does have a tie combination" $ 
      tie [Player1, Player1, Player2, Player2, Player2, Player1, Player1, Player2, Player1] `shouldBe` True

  describe "winningCombos" $ do
    it "returns a list of winning combination for a given board" $
      sort (winningIndices (initBoard 3)) `shouldBe` sort [[0, 1, 2], [3, 4, 5], [6, 7, 8], [0, 3, 6], [1, 4, 7], [2, 5, 8], [2, 4, 6], [0, 4, 8]]

  describe "switchPlayer" $ do
    it "returns Player2 when given Player1" $
      switchPlayer Player1 `shouldBe` Player2

    it "returns Player1 when given Player2" $
      switchPlayer Player2 `shouldBe` Player1

  describe "cellStr" $ do
    it "returns 'X' for Player1" $
      cellStr Player1  `shouldBe` "X"

    it "returns 'O' for Player2" $
      cellStr Player2  `shouldBe` "O"

    it "returns '-' for Empty" $
      cellStr Empty  `shouldBe` "-"

  describe "boardStr" $ do
    it "returns a string representation of a 3x3 board" $
      boardStr (initBoard 3) `shouldBe` "\9484\9472\9472\9472\9516\9472\9472\9472\9516\9472\9472\9472\9488\n| - | - | - |\n\9474\9472\9472\9472\9474\9472\9472\9472\9474\9472\9472\9472\9474\n| - | - | - |\n\9474\9472\9472\9472\9474\9472\9472\9472\9474\9472\9472\9472\9474\n| - | - | - |\n\9492\9472\9472\9472\9524\9472\9472\9472\9524\9472\9472\9472\9496"