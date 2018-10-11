module Board where
  
data Cell = Player1 | Player2 | Empty deriving (Eq, Show)
type Board = [Cell]
  
initBoard :: Board
initBoard = take 9 (repeat Empty)

replaceCellAt :: Board -> Int -> Cell -> Board
replaceCellAt [] _ _ = []
replaceCellAt (x:xs) index cell 
  | index == 0 = cell:xs
  | otherwise = x:replaceCellAt xs (index - 1) cell

validPlacement :: Board -> Int -> Bool
validPlacement board index = index < length board && board !! index == Empty

win :: Board -> Bool
win board = case board of
  [Player1, Player1, Player1, _, _, _, _, _, _] -> True
  [Player2, Player2, Player2, _, _, _, _, _, _] -> True
  [_, _, _, Player1, Player1, Player1, _, _, _] -> True
  [_, _, _, Player2, Player2, Player2, _, _, _] -> True
  [_, _, _, _, _, _, Player1, Player1, Player1] -> True
  [_, _, _, _, _, _, Player2, Player2, Player2] -> True
  [Player1, _, _, Player1, _, _, Player1, _, _] -> True
  [Player2, _, _, Player2, _, _, Player2, _, _] -> True
  [_, Player1, _, _, Player1, _, _, Player1, _] -> True
  [_, Player2, _, _, Player2, _, _, Player2, _] -> True
  [_, _, Player1, _, _, Player1, _, _, Player1] -> True
  [_, _, Player2, _, _, Player2, _, _, Player2] -> True
  [Player1, _, _, _, Player1, _, _, _, Player1] -> True
  [Player2, _, _, _, Player2, _, _, _, Player2] -> True
  [_, _, Player1, _, Player1, _, Player1, _, _] -> True
  [_, _, Player2, _, Player2, _, Player2, _, _] -> True
  _ -> False

tie :: Board -> Bool
tie board = not (win board) && not (any (\x -> x == Empty) board)

-- FIXME - returns true when a wining combination of Empty is found
-- win:: Board -> Bool
-- win board = or (map (winCombo board) [[0, 1, 2], [3, 4, 5], [6, 7, 8], [0, 3, 6], [1, 4, 7], [2, 5, 8], [2, 4, 6], [0, 4, 8]])

-- winCombo :: Board -> [Int] -> Bool
-- winCombo board winningCombo = allTheSame (map (\i -> (board !! i)) (winningCombo))

-- allTheSame :: [Cell] -> Bool
-- allTheSame cells = and $ map (== head cells) (tail cells)
