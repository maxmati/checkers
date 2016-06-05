module Board where

import qualified Data.Map as Map
import Utils


data Color = Black | White deriving (Show, Eq)
data Type = Pawn | King deriving (Show, Eq)
type Figure = (Type, Color)
type Pos = (Integer, Integer)
type Board = Map.Map Pos Figure

initBoard :: String
initBoard = ".b.b.b.b\nb.b.b.b.\n.b.b.b.b\n........\n........\nw.w.w.w.\n.w.w.w.w\nw.w.w.w."


charToField :: Char -> Maybe Figure
charToField 'b' = Just (Pawn, Black)
charToField 'B' = Just (King, Black)
charToField 'w' = Just (Pawn, White)
charToField 'W' = Just (King, White)
charToField _ = Nothing

stringToRow :: String -> Integer -> [(Pos, Figure)]
stringToRow string row = removeEmpty $ zip [(x,row)| x <- [1..8]] $ map charToField string

stringToBoardList :: String -> [(Pos, Figure)]
stringToBoardList string = foldl (\all (row, string) -> all ++ (stringToRow string row)) [] $ zip [1..8] (lines string)

stringToBoard :: String -> Board
stringToBoard string = Map.fromList $ stringToBoardList string

fieldToChar :: Maybe Figure -> Char
fieldToChar (Just (Pawn, Black)) = '♟'
fieldToChar (Just (King, Black)) = '♛'
fieldToChar (Just (Pawn, White)) = '♙'
fieldToChar (Just (King, White)) = '♕'
fieldToChar Nothing = '.'

rowToString  :: Board -> Integer -> String
rowToString brd row = foldl
                (\old k -> old ++ [fieldToChar (Map.lookup k brd)] )
                []
                $ [(x, row) | x <- [1..8]]

boardToString :: Board -> String
boardToString brd = unlines (map (rowToString brd) [1..8])

putBoard :: Board -> IO ()
putBoard brd = putStr $  boardToString brd