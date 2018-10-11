module Shell where

import Board

run :: IO ()
run = playRound (initBoard 3) Player1

playRound :: Board -> Cell-> IO ()
playRound board player = do
  putStrLn (boardStr board)
  putStrLn (cellStr player ++ ", enter a number [0-8]")
  input <- getLine
  let move = read input :: Int
  if validPlacement board move
    then do
      let nextBoard = replaceCellAt board move player
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
    else putStrLn "Try again." >> playRound board player
