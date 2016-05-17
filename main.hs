import qualified Control.Monad
import qualified Data.Map
import Data.Tree
import Data.Maybe

data Color = Black | White deriving (Show, Eq)
data Type = Pawn | King deriving (Show, Eq)
type Figure = (Type, Color)
type Board = Data.Map.Map (Int, Int) Figure

initBoard = ".b.b.b.b\nb.b.b.b.\n.b.b.b.b\n........\n........\nw.w.w.w.\n.w.w.w.w\nw.w.w.w."

charToField 'b' = Just (Pawn, Black)
charToField 'B' = Just (King, Black)
charToField 'w' = Just (Pawn, White)
charToField 'W' = Just (King, White)
charToField a = Nothing


removeEmpty list = let removeJust (a,b) = (a, fromJust b)
                       isNotEmpty (_, b) = isJust b
                   in  map removeJust $ filter isNotEmpty list

stringToRow string row = removeEmpty $ zip [(x,row)| x <- [1..8]] $ map charToField string
stringToBoardList string = foldl (\all (row, string) -> all ++ (stringToRow string row)) [] $ zip [1..8] (lines string)
stringToBoard string = Data.Map.fromList $ stringToBoardList string

fieldToChar (Just (Pawn, Black)) = '♟'
fieldToChar (Just (King, Black)) = '♛'
fieldToChar (Just (Pawn, White)) = '♙'
fieldToChar (Just (King, White)) = '♕'
fieldToChar Nothing = '.'

rowToString board row = foldl 
                (\all k -> all ++ [fieldToChar (Data.Map.lookup k board)] )
                []
                $ [(x, row) | x <- [1..8]]
boardToString board = unlines (map (rowToString board) [1..8])

putBoard board = putStr $  boardToString board

move board from to = let figure = Data.Map.lookup from board
                     in  Data.Map.update (\_ -> figure) to $ Data.Map.update (\_ -> Nothing) from board

generateMoves (fromX, fromY) color = let yMod = if color == Black then 1 else -1
                                     in  [(fromX + xAdd, fromY + yAdd*yMod ) | xAdd <- [-2..2], yAdd <- [1..2], abs xAdd == yAdd]

removeOffBoard moves = let onBoard (x, y) = x > 0 && x <= 8 && y > 0 && y <= 8
                       in filter onBoard moves

removeOccupied board moves = let isAvailable pos = Data.Map.lookup pos board == Nothing
                             in  filter isAvailable moves

removeReqBeating board color from moves = let haveToBeat (x, y) = abs (x - fst from) == 2
                                              yMod = if color == Black then 1 else -1
                                              beatPos (x, y) = if x > fst from then (x - 1, y - yMod)
                                                                               else (x + 1, y - yMod)
                                              unableToBeat pos = case (Data.Map.lookup (beatPos pos) board) of Nothing -> True
                                                                                                               Just fig -> snd fig == color
                                              remove pos = haveToBeat pos && unableToBeat pos
                                              keep pos = not $ remove pos
                                          in  filter keep moves

removeInvalidMoves board color from moves = removeReqBeating board color from $ removeOccupied board $ removeOffBoard moves

generateValidMoves board from color = let fig = Data.Map.lookup from board
                                      in  if isJust fig && snd (fromJust fig) == color
                                                then removeInvalidMoves board color from $ generateMoves from color
                                                else []

prependAll pref list = foldl (\acc val -> acc ++ [(pref, val)]) [] list


generateAllValidMoves board color = let positions = [(x,y) | x <- [1..8], y <- [1..8]]
                                        appendMoves acc pos = acc ++ (prependAll pos $ generateValidMoves board pos color)
                                    in foldl appendMoves [] positions


generateMovesTree color board = let makeMove (from, to) = move board from to
                                    nextColor = if color == Black then White else Black
                                    nextBoards = map makeMove $ generateAllValidMoves board color
                                    nextTrees = map (generateMovesTree nextColor) nextBoards
                                in  Node board nextTrees

b = stringToBoard initBoard
--m = removeOffBoard $ generateMoves (1,2) Black