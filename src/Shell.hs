module Shell where

import Board

run :: IO ()
run = playRound initBoard Player1

playRound :: Board -> Cell-> IO ()
playRound board player = do
  -- Print the board
  putStrLn (boardStr board)

  -- Get user input
  input <- getLine
  let move = read input :: Int

  -- Validate input
  let foo = validPlacement board move

  -- Update board
  let nextBoard = replaceCellAt board move player

  -- Check for gameover
  if win nextBoard
  then do
    putStrLn ("Congrats " ++ cellStr player ++ ", you've won!")
    putStrLn (boardStr nextBoard)
  else if tie nextBoard
       then do 
        putStrLn "It's a tie!"
        putStrLn (boardStr nextBoard)
       else do
        let nextPlayer = switchPlayer player
        playRound nextBoard (switchPlayer player)


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