import qualified Data.Map
import qualified Control.Monad

data Color = Black | White deriving Show
data Type = Pawn | King deriving Show
newtype Figure = Maybe (Type, Color) deriving Show
type Board = Data.Map.Map (Int, Int) Figure

initBoard = ".b.b.b.b\nb.b.b.b.\n.b.b.b.b\n........\n........\nw.w.w.w.\n.w.w.w.w\nw.w.w.w."

charToField 'b' = Just (Pawn, Black)
charToField 'w' = Just (Pawn, White)
charToField a = Nothing


stringToRow string row = zip [(x,row)| x <- [1..8]] $ map charToField string
stringToBoardList string = foldl (\all (row, string) -> all ++ (stringToRow string row)) [] $ zip [1..8] (lines string)
stringToBoard string = Data.Map.fromList $ stringToBoardList string

fieldToChar (Just (Pawn, Black)) = '♟'
fieldToChar (Just (King, Black)) = '♛'
fieldToChar (Just (Pawn, White)) = '♙'
fieldToChar (Just (King, White)) = '♕'
fieldToChar Nothing = '.'

rowToString board row = foldl 
				(\all k -> all ++ [fieldToChar (Control.Monad.join (Data.Map.lookup k board))] ) 
				[] 
				$ [(x, row) | x <- [1..8]]
boardToString board = unlines (map (rowToString board) [1..8])

putBoard board = putStr $  boardToString board

