module Board where
  
import Data.List

data Cell = Player1 | Player2 | Empty deriving (Eq, Show)
type Board = [Cell]
  
initBoard :: Int -> Board
initBoard size = replicate (size ^ 2) Empty

size :: Board -> Int
-- size = round . sqrt . fromIntegral . length
-- size board = round $ sqrt $ fromIntegral $ length board
size board = round (sqrt (fromIntegral (length board)))

replaceCellAt :: Board -> Int -> Cell -> Board
replaceCellAt [] _ _ = []
replaceCellAt (x:xs) index cell 
  | index == 0 = cell:xs
  | otherwise = x:replaceCellAt xs (index - 1) cell

validPlacement :: Board -> Int -> Bool
validPlacement board index = index < length board && board !! index == Empty

tie :: Board -> Bool
tie board = not (win board || Empty `elem` board)

win:: Board -> Bool
win board = any (winningSpaces board) (winningIndices board)

winningSpaces :: Board -> [Int] -> Bool
winningSpaces board indicies = allTheSameAndNotEmpty (map (\i -> board !! i) indicies)

winningIndices :: Board -> [[Int]]
winningIndices board = winningRows board ++ winningCols board ++ winningDiags board

allTheSameAndNotEmpty :: [Cell] -> Bool
allTheSameAndNotEmpty cells = (head cells /= Empty) && all (\x -> x == head cells) (tail cells)

winningRows :: Board -> [[Int]]
winningRows board = map (\start -> take (size board) [start..]) (rowStarts board)

winningCols :: Board -> [[Int]]
winningCols board = map (\start -> take (size board) [start, start + size board..]) (colStarts board)

winningDiags :: Board -> [[Int]]
winningDiags board = [
                      take (size board) [0, (size board + 1)..],
                      take (size board) [size board - 1, ((size board - 1) ^ 2)..]
                     ]

rowStarts :: Board -> [Int]
rowStarts board = take (size board) [0, (0 + size board)..]

colStarts :: Board -> [Int]
colStarts board = take (size board) [0..]

switchPlayer :: Cell -> Cell
switchPlayer player = 
  case player of
    Player1 -> Player2
    Player2 -> Player1

player :: Board -> Cell
player s = if odd (length (filter (==Empty) s)) then Player1 else Player2

actions :: Board -> [Int]
actions = elemIndices Empty

result :: Board -> Int -> Cell -> Board
result [] _ _= []
result s a player
  | a == 0 = player:tail s
  | otherwise = head s:result (tail s) (a - 1) player

terminal :: Board -> Bool
terminal s = tie s || win s

utility :: Board -> Int
utility s 
  | terminal s = 0
  | tie s = 0
  | win s && player s == Player1 = 1
  | win s && player s == Player2 = -1

minimax :: Board -> Int
minimax s = if terminal s
            then utility s
            else case player s of
              Player1 -> snd (minimumBy (\(a, _) (b, _) -> compare a b) (zip (map (\a -> minimax (result s a (player s))) (actions s)) (actions s)))
              Player2 -> snd (maximumBy (\(a, _) (b, _) -> compare a b) (zip (map (\a -> minimax (result s a (player s))) (actions s)) (actions s)))


cellStr :: Cell -> String
cellStr Player1 = "X"
cellStr Player2 = "O"
cellStr Empty = "-"

boardStr :: Board -> String
boardStr board = 
   "\n " ++ (cellStr (board !! 0) ++ " | " ++ cellStr (board !! 1) ++ " | " ++ cellStr (board !! 2)) ++ " \n"
  ++ "───│───│───\n"
  ++ " " ++ (cellStr (board !! 3) ++ " | " ++ cellStr (board !! 4) ++ " | " ++ cellStr (board !! 5)) ++ " \n"
  ++ "───│───│───\n"
  ++ " " ++ (cellStr (board !! 6) ++ " | " ++ cellStr (board !! 7) ++ " | " ++ cellStr (board !! 8)) ++ " \n"