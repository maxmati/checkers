import qualified Control.Monad
import qualified Data.Map
import Data.Tree
import Data.Maybe

data Color = Black | White deriving (Show, Eq)
data Type = Pawn | King deriving (Show, Eq)
type Figure = (Type, Color)
type Pos = (Integer, Integer)
type Board = Data.Map.Map Pos Figure
data Move = Move {beats:: Bool, from:: Pos, to:: Pos, board:: (Maybe Board)} deriving Show

initBoard = ".b.b.b.b\nb.b.b.b.\n.b.b.b.b\n........\n........\nw.w.w.w.\n.w.w.w.w\nw.w.w.w."

charToField :: Char -> Maybe Figure
charToField 'b' = Just (Pawn, Black)
charToField 'B' = Just (King, Black)
charToField 'w' = Just (Pawn, White)
charToField 'W' = Just (King, White)
charToField a = Nothing

removeEmpty :: [(t, Maybe a)] -> [(t, a)]
removeEmpty list = let removeJust (a,b) = (a, fromJust b)
                       isNotEmpty (_, b) = isJust b
                   in  map removeJust $ filter isNotEmpty list

stringToRow :: String -> Integer -> [(Pos, Figure)]
stringToRow string row = removeEmpty $ zip [(x,row)| x <- [1..8]] $ map charToField string

stringToBoardList :: String -> [(Pos, Figure)]
stringToBoardList string = foldl (\all (row, string) -> all ++ (stringToRow string row)) [] $ zip [1..8] (lines string)

stringToBoard :: String -> Board
stringToBoard string = Data.Map.fromList $ stringToBoardList string

fieldToChar :: Maybe Figure -> Char
fieldToChar (Just (Pawn, Black)) = '♟'
fieldToChar (Just (King, Black)) = '♛'
fieldToChar (Just (Pawn, White)) = '♙'
fieldToChar (Just (King, White)) = '♕'
fieldToChar Nothing = '.'

rowToString  :: Board -> Integer -> String
rowToString board row = foldl 
                (\all k -> all ++ [fieldToChar (Data.Map.lookup k board)] )
                []
                $ [(x, row) | x <- [1..8]]

boardToString :: Board -> String
boardToString board = unlines (map (rowToString board) [1..8])

putBoard :: Board -> IO ()
putBoard board = putStr $  boardToString board

beatenPoses :: Pos -> Pos -> [Pos]
beatenPoses (fromX, fromY) (toX, toY) = let endX   = toX - fromX
                                            startX = signum endX
                                            xMod   = if endX > startX then [startX..endX - 1] else [endX..startX + 1]
                                            endY   = toY - fromY
                                            startY = signum endY
                                            yMod   = if endY > startY then [startY..endY - 1] else [endY..startY + 1]
                                        in  [(fromX + x, fromY + y) | x <- xMod, y <- yMod, abs x == abs y ]

makeMove :: Board -> Pos -> Pos -> Board
makeMove board from to = let figure = fromJust $ Data.Map.lookup from board
                         in  Data.Map.insert to figure $ Data.Map.delete from $ foldr Data.Map.delete board $ beatenPoses from to

generateMoves :: Pos -> Color -> [Move]
generateMoves from@(fromX, fromY) color = let yMod = if color == Black then 1 else -1
                                              positions = [(fromX + xAdd, fromY + yMod ) | xAdd <- [-1,1]]
                                          in  map (\to -> Move True from to Nothing) positions

generateJumps :: Pos -> [Move]
generateJumps from@(fromX, fromY) =  let positions = [(fromX + xAdd, fromY + yAdd ) | xAdd <- [-2,2], yAdd <- [-2,2]]
                                     in  map (\to -> Move True from to Nothing) positions

removeOffBoard :: [Move] -> [Move]
removeOffBoard moves = let onBoard (Move _ _ (x, y) _) = x > 0 && x <= 8 && y > 0 && y <= 8
                       in filter onBoard moves

removeOccupied :: Board -> [Move] -> [Move]
removeOccupied board moves = let isAvailable (Move _ _ pos _) = Data.Map.lookup pos board == Nothing
                             in  filter isAvailable moves

removeReqBeating :: Board -> Color -> Pos -> [Move] -> [Move]
removeReqBeating board color (fromX, fromY) moves = let haveToBeat (x, y) = abs (x - fromX) == 2
                                                        yMod (x, y)= if y > fromY then 1 else -1
                                                        beatPos (x, y) = if x > fromX then (x - 1, y - yMod (x, y))
                                                                                      else (x + 1, y - yMod (x, y))
                                                        unableToBeat pos = case (Data.Map.lookup (beatPos pos) board) of Nothing -> True
                                                                                                                         Just fig -> snd fig == color
                                                        remove pos = haveToBeat pos && unableToBeat pos
                                                        keep (Move _ _ pos _) = not $ remove pos
                                                    in  filter keep moves

removeInvalidMoves :: Board -> Color -> Pos -> [Move] -> [Move]
removeInvalidMoves board color from moves = removeReqBeating board color from $ removeOccupied board $ removeOffBoard moves

generateValidMoves :: Board -> Pos -> Color -> [Move]
generateValidMoves board from color = let fig = Data.Map.lookup from board
                                      in  if isJust fig && snd (fromJust fig) == color
                                          then removeInvalidMoves board color from $ generateMoves from color ++ generateJumps from
                                          else []

generateAllValidMoves :: Board -> Color -> [Move]
generateAllValidMoves board color = let positions = [(x,y) | x <- [1..8], y <- [1..8]]
                                        appendMoves acc pos = acc ++ (generateValidMoves board pos color)
                                    in  foldl appendMoves [] positions

setBoard :: Move -> Board -> Move
setBoard (Move beats from to _) board = Move beats from to $ Just board

generateAndSetBoard :: Board -> Move -> Move
generateAndSetBoard board move2 =  setBoard move2 $ makeMove board (from move2) (to move2)

getBoard :: Move -> Board
getBoard move = fromJust $ board move

generateMovesTree :: Color -> Board -> [Tree Move]
generateMovesTree color board = let makeMove move = generateAndSetBoard board move
                                    nextColor = if color == Black then White else Black
                                    nextMoves = map makeMove $ generateAllValidMoves board color
                                    createNode move = Node move (generateMovesTree nextColor $ getBoard move)
                                in  map createNode nextMoves


nodesAtLevel :: Int -> Tree a -> [a]
nodesAtLevel 0 (Node a _) = [a]
nodesAtLevel n (Node _ subtrees) = concatMap (nodesAtLevel (n-1)) subtrees

b = stringToBoard initBoard
--m = removeOffBoard $ generateMoves (1,2) Black
testBoard1 = ".b.b.b.b\nb.b.b.b.\n.b.b.w.b\n........\n........\nw.w.w.w.\n.w.w.w.w\nw.w.w.w."
tb1 = stringToBoard testBoard1
