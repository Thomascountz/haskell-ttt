module Board where
  
data Cell = Player1 | Player2 | Empty deriving (Eq, Show)
type Board = [Cell]
  
initBoard :: Board
initBoard = take 9 (repeat Empty)

winSize :: Board -> Int
winSize board = round (sqrt (fromIntegral (length board)))

replaceCellAt :: Board -> Int -> Cell -> Board
replaceCellAt [] _ _ = []
replaceCellAt (x:xs) index cell 
  | index == 0 = cell:xs
  | otherwise = x:replaceCellAt xs (index - 1) cell

validPlacement :: Board -> Int -> Bool
validPlacement board index = index < length board && board !! index == Empty

tie :: Board -> Bool
tie board = not (win board) && not (any (\x -> x == Empty) board)

winningCombos :: Board -> [[Int]]
winningCombos board = winningRows board ++ winningCols board ++ winningDiags board

winningRows :: Board -> [[Int]]
winningRows board = map (\start -> combination start 1 (winSize board)) (rowStarts board)

rowStarts :: Board -> [Int]
rowStarts board = combination 0 (winSize board) (winSize board)

winningCols :: Board -> [[Int]]
winningCols board = map (\start -> combination start (winSize board) (winSize board)) (colStarts board)

colStarts :: Board -> [Int]
colStarts board = combination 0 1 (winSize board)

winningDiags :: Board -> [[Int]]
winningDiags board = combination 0 (winSize board + 1) (winSize board)
                     : combination (winSize board - 1) (winSize board - 1) (winSize board)
                     : []

combination :: Int -> Int -> Int -> [Int]
combination start step size = take size (iterate (\x -> x + step) start)

-- FIXME - returns true when a wining combination of Empty is found
win:: Board -> Bool
win board = or (map (winCombo board) (winningCombos board))
-- 
winCombo :: Board -> [Int] -> Bool
winCombo board winningCombo = allTheSame (map (\i -> (board !! i)) (winningCombo))
-- 
allTheSame :: [Cell] -> Bool
allTheSame cells = not (head cells == Empty) && and (map (\x -> x == head cells) (tail cells))
