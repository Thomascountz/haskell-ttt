module Board where
  
data Cell = Player1 | Player2 | Empty deriving (Eq, Show)
type Board = [Cell]
  
initBoard :: Int -> Board
initBoard size = take (size ^ 2) (repeat Empty)

size :: Board -> Int
size board = round (sqrt (fromIntegral (length board)))

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
winningRows board = map (\start -> take (size board) [start..]) (rowStarts board)

winningCols :: Board -> [[Int]]
winningCols board = map (\start -> take (size board) [start, start + (size board)..]) (colStarts board)

winningDiags :: Board -> [[Int]]
winningDiags board = take (size board) [0, (size board + 1)..]
                     : take (size board) [size board - 1, ((size board - 1) ^ 2)..]
                     : []

rowStarts :: Board -> [Int]
rowStarts board = take (size board) [0, (0 + size board)..]

colStarts :: Board -> [Int]
colStarts board = take (size board) [0..]

switchPlayer :: Cell -> Cell
switchPlayer player = 
  case player of
    Player1 -> Player2
    Player2 -> Player1

cellStr :: Cell -> String
cellStr player = 
  case player of
    Empty -> "-"
    Player1 -> "X"
    Player2 -> "O"

boardStr :: Board -> String
boardStr board = do
  ("┌───┬───┬───┐\n" )
  ++ "| " ++ (cellStr (board !! 0) ++ " | " ++ cellStr (board !! 1) ++ " | " ++ cellStr (board !! 2)) ++ " |\n"
  ++ "│───│───│───│\n"
  ++ "| " ++ (cellStr (board !! 3) ++ " | " ++ cellStr (board !! 4) ++ " | " ++ cellStr (board !! 5)) ++ " |\n"
  ++ "│───│───│───│\n"
  ++ "| " ++ (cellStr (board !! 6) ++ " | " ++ cellStr (board !! 7) ++ " | " ++ cellStr (board !! 8)) ++ " |\n"
  ++ ("└───┴───┴───┘")