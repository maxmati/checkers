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

beatenPoses (fromX, fromY) (toX, toY) = let endX   = toX - fromX
					    startX = signum endX 
					    xMod   = if endX > startX then [startX..endX - 1] else [endX..startX + 1]
					    endY   = toY - fromY
					    startY = signum endY
					    yMod   = if endY > startY then [startY..endY - 1] else [endY..startY + 1]
					in [(fromX + x, fromY + y) | x <- xMod, y <- yMod, abs x == abs y ] 

move board from to = let figure = fromJust $ Data.Map.lookup from board
                     in  Data.Map.insert to figure $ Data.Map.delete from $ foldr Data.Map.delete board $ beatenPoses from to

generateMoves (fromX, fromY) color = let yMod = if color == Black then 1 else -1
                                     in  [(fromX + xAdd, fromY + yAdd*yMod ) | xAdd <- [-2..2], yAdd <- [1..2], abs xAdd == yAdd] ++ [(fromX + xAdd, fromY - 2*yMod) | xAdd <- [-2,2]]

removeOffBoard moves = let onBoard (x, y) = x > 0 && x <= 8 && y > 0 && y <= 8
                       in filter onBoard moves

removeOccupied board moves = let isAvailable pos = Data.Map.lookup pos board == Nothing
                             in  filter isAvailable moves

removeReqBeating board color (fromX, fromY) moves = let haveToBeat (x, y) = abs (x - fromX) == 2
                                              		yMod (x, y)= if y > fromY then 1 else -1
                                              		beatPos (x, y) = if x > fromX then (x - 1, y - yMod (x, y))
                                                                            	      else (x + 1, y - yMod (x, y))
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


nodesAtLevel :: Int -> Tree a -> [a]
nodesAtLevel 0 (Node a _) = [a]
nodesAtLevel n (Node _ subtrees) = concatMap (nodesAtLevel (n-1)) subtrees

b = stringToBoard initBoard
--m = removeOffBoard $ generateMoves (1,2) Black
testBoard1 = ".b.b.b.b\nb.b.b.b.\n.b.b.w.b\n........\n........\nw.w.w.w.\n.w.w.w.w\nw.w.w.w."
tb1 = stringToBoard testBoard1
