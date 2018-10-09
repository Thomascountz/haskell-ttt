module Board where
  
import qualified Data.Sequence as Seq

data Cell = Player1 | Player2 | Empty deriving Show
type Board = Seq.Seq Cell
  
initBoard :: Board
initBoard = Seq.replicate 9 Empty