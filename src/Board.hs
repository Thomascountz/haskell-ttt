module Board where
  
data Cell = Player1 | Player2 | Empty deriving (Eq, Show)
type Board = [Cell]
  
initBoard :: Board
initBoard = take 9 (repeat Empty)