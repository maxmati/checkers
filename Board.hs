module Board (Pos, Board, Color(White, Black), Figure, Board.lookup, insert, delete, Board.foldl, opositeColor, b, putBoard) where

import qualified Data.Map as Map
import Utils


data Color = Black | White deriving (Show, Eq)
data Type = Pawn | King deriving (Show, Eq)
type Figure = (Type, Color)
type Pos = (Int, Int)
newtype Board = Board (Map.Map Pos Figure)

instance Show Board where
    show = ("\n" ++) . boardToString

initBoard :: String
initBoard = ".b.b.b.b\nb.b.b.b.\n.b.b.b.b\n........\n........\nw.w.w.w.\n.w.w.w.w\nw.w.w.w."
b :: Board
b = stringToBoard initBoard


opositeColor :: Color -> Color
opositeColor Black = White
opositeColor White = Black

charToField :: Char -> Maybe Figure
charToField 'b' = Just (Pawn, Black)
charToField 'B' = Just (King, Black)
charToField 'w' = Just (Pawn, White)
charToField 'W' = Just (King, White)
charToField _ = Nothing

stringToRow :: String -> Int -> [(Pos, Figure)]
stringToRow string row = removeEmpty $ zip [(x,row)| x <- [1..8]] $ map charToField string

stringToBoardList :: String -> [(Pos, Figure)]
stringToBoardList string = Prelude.foldl (\all (row, str) -> all ++ (stringToRow str row)) [] $ zip [1..8] (lines string)

stringToBoard :: String -> Board
stringToBoard string = Board $ Map.fromList $ stringToBoardList string

fieldToChar :: Maybe Figure -> Char
fieldToChar (Just (Pawn, Black)) = '♟'
fieldToChar (Just (King, Black)) = '♛'
fieldToChar (Just (Pawn, White)) = '♙'
fieldToChar (Just (King, White)) = '♕'
fieldToChar Nothing = '.'

rowToString  :: Board -> Int -> String
rowToString brd row = Prelude.foldl
                (\old k -> old ++ [fieldToChar (Board.lookup k brd)] )
                []
                $ [(x, row) | x <- [1..8]]

boardToString :: Board -> String
boardToString brd = unlines (map (rowToString brd) [1..8])

putBoard :: Board -> IO ()
putBoard brd = putStr $  boardToString brd


lookup :: Pos -> Board -> Maybe Figure
lookup pos (Board brd) = Map.lookup pos brd

insert :: Pos -> Figure -> Board -> Board
insert pos fig (Board brd) = Board $ Map.insert pos fig brd

delete :: Pos -> Board -> Board
delete pos (Board brd) = Board $ Map.delete pos brd

foldl :: (a -> Figure -> a) -> a -> Board -> a
foldl f acc (Board brd) = Map.foldl f acc brd