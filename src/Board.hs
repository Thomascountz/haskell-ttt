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