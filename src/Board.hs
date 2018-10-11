module Board where
  
data Cell = Player1 | Player2 | Empty deriving (Eq, Show)
type Board = [Cell]
  
initBoard :: Int -> Board
initBoard size = take (size ^ 2) (repeat Empty)

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

win:: Board -> Bool
win board = or (map (winningSpaces board) (winningIndices board))

winningSpaces :: Board -> [Int] -> Bool
winningSpaces board indicies = allTheSameAndNotEmpty (map (\i -> (board !! i)) (indicies))

winningIndices :: Board -> [[Int]]
winningIndices board = winningRows board ++ winningCols board ++ winningDiags board

allTheSameAndNotEmpty :: [Cell] -> Bool
allTheSameAndNotEmpty cells = not (head cells == Empty) && and (map (\x -> x == head cells) (tail cells))

winningRows :: Board -> [[Int]]
winningRows board = map (\start -> take (winSize board) [start..]) (rowStarts board)

winningCols :: Board -> [[Int]]
winningCols board = map (\start -> take (winSize board) [start, start + (winSize board)..]) (colStarts board)

winningDiags :: Board -> [[Int]]
winningDiags board = take (winSize board) [0, (winSize board + 1)..]
                     : take (winSize board) [winSize board - 1, ((winSize board - 1) ^ 2)..]
                     : []

rowStarts :: Board -> [Int]
rowStarts board = take (winSize board) [0, (0 + winSize board)..]

colStarts :: Board -> [Int]
colStarts board = take (winSize board) [0..]