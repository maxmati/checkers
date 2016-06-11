module Moves (Turn(Turn), Move(Move, getFrom,getTo), makeTurn, generateAllValidMoves, generateAndSetBoard, getBoard) where


import qualified Data.Map as Map
import Data.Maybe
import Board hiding (foldl)

data Move = Move {getFrom:: Pos, getTo:: Pos} deriving Show
data Turn = Turn Bool [Move] (Maybe Board) deriving Show
data Direction = NE | SE | SW | NW

beatenPoses :: Pos -> Pos -> [Pos]
beatenPoses (fromX, fromY) (toX, toY) = let endX   = toX - fromX
                                            startX = signum endX
                                            xMod   = if endX > startX then [startX..endX - 1] else [endX..startX + 1]
                                            endY   = toY - fromY
                                            startY = signum endY
                                            yMod   = if endY > startY then [startY..endY - 1] else [endY..startY + 1]
                                        in  [(fromX + x, fromY + y) | x <- xMod, y <- yMod, abs x == abs y ]

changePawnForKing :: Color -> Pos -> Board -> Board
changePawnForKing color pos brd
    | (getColor <$> fig) == Just color = Board.insert pos (King, color) brd
    | otherwise = brd
    where fig = Board.lookup pos brd

checkForPawnKingChange :: Board -> Board
checkForPawnKingChange brd = foldr (changePawnForKing Black) (foldr (changePawnForKing White) brd whitePoses) blackPoses
                             where whitePoses = [(x*2, 1) | x <- [1..4]]
                                   blackPoses = [(x*2 - 1, 8) | x <- [1..4]]

makeTurn :: Board -> Turn -> Board
makeTurn brd = checkForPawnKingChange . moveFig brd

makeTurnLast :: Board -> Turn -> Board
makeTurnLast brd = checkForPawnKingChange . moveFigLast brd

moveFigLast :: Board -> Turn -> Board
moveFigLast brd (Turn True moves _) = let figure = fromJust $ Board.lookup (getFrom (last moves)) brd
                                          makeSingleMove brd' (Move from to) = Board.insert to figure $ Board.delete from $ foldr Board.delete brd' $ beatenPoses from to
                                      in  makeSingleMove brd (last moves)

moveFig :: Board -> Turn -> Board
moveFig brd (Turn False [move] _) = Board.insert (getTo move) figure $ Board.delete (getFrom move) brd
                                     where figure = fromJust $ Board.lookup (getFrom move) brd
moveFig _ (Turn False _ _) = undefined
moveFig brd (Turn True moves _)  = let figure = fromJust $ Board.lookup (getFrom (head moves)) brd
                                       makeSingleMove brd' (Move from to) = Board.insert to figure $ Board.delete from $ foldr Board.delete brd' $ beatenPoses from to
                                   in  foldl makeSingleMove brd moves

generateKingMovesPos :: Pos -> Direction -> [Pos]
generateKingMovesPos (x, y) NE
    | maxval == 8 = []
    | otherwise = [(x + z, y + z) | z <- [1..8-maxval]]
    where maxval = max x y
generateKingMovesPos (x, y) SE
    | maxval == 8 = []
    | otherwise = [(x + z, y - z) | z <- [1..8-maxval]]
    where maxval = max x (9-y)
generateKingMovesPos (x, y) SW
    | maxval == 8 = []
    | otherwise = [(x - z, y - z) | z <- [1..8-maxval]]
    where maxval = max (9-x) (9-y)
generateKingMovesPos (x, y) NW
    | maxval == 8 = []
    | otherwise = [(x + z, y - z) | z <- [1..8-maxval]]
    where maxval = max x (9-y)


addKingMultiJumps :: Color -> Board -> Turn -> [Turn]
addKingMultiJumps color brd turn
    | null aviableJumps = [turn]
    | otherwise = aviableJumps
    where dirs = [NE, SE, SW, NW]
          from = getTo $ getLastMove turn
          addMove (Turn jump moves _) pos = Turn jump (moves ++ [Move from pos]) Nothing
          addMoves = map (addMove turn)
          brd' = makeTurn brd turn
          aviableJumps = concatMap ((removeInvalidKingMoves True color brd'). addMoves . (generateKingMovesPos from)) dirs

getLastMove :: Turn -> Move
getLastMove (Turn _ moves _) = last moves

removeInvalidKingMoves :: Bool -> Color -> Board -> [Turn] -> [Turn]
removeInvalidKingMoves _ _ _ [] = []
removeInvalidKingMoves reqJump color brd (pos:rest)
    | isNothing fig && reqJump = [] -- req jump and nothin to jump
    | isNothing fig = [pos] ++ removeInvalidKingMoves reqJump color brd rest -- standard jump
    | (getColor $ fromJust fig) == color = removeInvalidKingMoves reqJump color brd rest -- field occupied
    | otherwise = if isNothing $ Board.lookup (getLastTarget (head rest)) brd then addKingMultiJumps color brd $ setJump $ head rest else [] -- fig to jump over
    where fig = Board.lookup (getLastTarget pos) brd
          getLastTarget turn = getTo $ getLastMove turn
          setJump (Turn _ moves brd') = Turn True moves brd'

generateKingMoves :: Color -> Board -> Pos -> [Turn]
generateKingMoves color brd pos = concatMap ((removeInvalidKingMoves False color brd) . createTurns . (generateKingMovesPos pos)) dirs
                                  where dirs = [NE, SE, SW, NW]
                                        createTurns poses = map (createTurn pos) poses

createTurn :: Pos -> Pos -> Turn
createTurn from to  = Turn False [Move from to] Nothing

generateMoves :: Pos -> Color -> [Turn]
generateMoves from@(fromX, fromY) color = let yMod = if color == Black then 1 else -1
                                              positions = [(fromX + xAdd, fromY + yMod ) | xAdd <- [-1,1]]
                                          in  map (\to -> Turn False [(Move from to)] Nothing) positions

generateJumpsPos :: Pos -> [Pos]
generateJumpsPos (fromX, fromY) = [(fromX + xAdd, fromY + yAdd ) | xAdd <- [-2,2], yAdd <- [-2,2]]

generateJumps :: Pos -> [Turn]
generateJumps from = map (\to -> Turn True [Move from to] Nothing) $ generateJumpsPos from

removeOffBoard :: [Turn] -> [Turn]
removeOffBoard turns = let isMoveOnBoard (Move _ (x, y)) = x > 0 && x <= 8 && y > 0 && y <= 8
                           allOnBoard (Turn _ moves _) = all isMoveOnBoard moves
                       in filter allOnBoard turns

removeOccupied :: Board -> [Turn] -> [Turn]
removeOccupied board turns = let isSquareAvailable (Move _ pos) = Board.lookup pos board == Nothing
                                 lastSquaresAvailable (Turn _ moves _) = isSquareAvailable $ last moves
                             in  filter lastSquaresAvailable turns

isInvalidJump :: Board -> Color -> Move -> Bool
isInvalidJump brd color (Move (fromX, fromY) (toX, toY)) = let isJump = abs (toX - fromX) == 2
                                                               yMod = signum(toY - fromY)
                                                               xMod = signum(toX - fromX)
                                                               beatPos = (toX - xMod, toY - yMod )
                                                               unableToJump = case (Board.lookup beatPos brd) of Nothing -> True
                                                                                                                 Just fig -> snd fig == color
                                                           in (isJump && unableToJump)

removeReqBeating :: Board -> Color  -> [Turn] -> [Turn]
removeReqBeating brd color turns = let keep = not . isInvalidJump brd color . getLastMove
                                   in  filter keep turns


removeInvalidMoves :: Board -> Color -> [Turn] -> [Turn]
removeInvalidMoves board color turns = removeReqBeating board color $ removeOccupied board $ removeOffBoard turns

addMultiJumps :: Turn -> [Turn]
addMultiJumps (Turn j moves brd) = let lastPos = getTo $ last moves
                                       availableMoves = generateJumpsPos lastPos
                                       updateBoard = updateAndSetBoard (fromJust brd)
                                   in  map (\to -> updateBoard $ Turn j (moves ++ [Move lastPos to]) brd) availableMoves

addAllMultiJumps :: Color -> Turn -> [Turn]
addAllMultiJumps color turn
    | null availableJumps = [turn]
    | otherwise = concatMap (addAllMultiJumps color) availableJumps
        where availableJumps = removeInvalidMoves (getBoard turn) color $ addMultiJumps turn

generateValidJumps :: Board -> Pos -> Color -> [Turn]
generateValidJumps board from color = let singleJumps = removeInvalidMoves board color $ generateJumps from
                                      in concatMap (addAllMultiJumps color) $ map (generateAndSetBoard board) singleJumps

generateValidMoves :: Board -> Pos -> Color -> [Turn]
generateValidMoves board from color = let fig = Board.lookup from board
                                       in  if isJust fig && snd (fromJust fig) == color
                                           then if fst (fromJust fig) == Pawn
                                                then (removeInvalidMoves board color $ generateMoves from color)
                                                     ++ generateValidJumps board from color
                                                else generateKingMoves color board from
                                           else []

filterLongestJump :: [Turn] -> [Turn]
filterLongestJump turns = filter (\x -> turnLength x >= maxJumps) turns
                         where maxJumps = maximum $ map turnLength turns
                               turnLength (Turn _ moves _) = length moves


filterForceJumps :: [Turn] -> [Turn]
filterForceJumps turns
    | forceJump = filter isJump turns
    | otherwise = turns
    where forceJump = any isJump turns
          isJump (Turn jump _ _) = jump

filterJumpsRequirements :: [Turn] -> [Turn]
filterJumpsRequirements = filterLongestJump . filterForceJumps


generateAllValidMoves :: Board -> Color -> [Turn]
generateAllValidMoves board color = let positions = [(x,y) | x <- [1..8], y <- [1..8]]
                                        appendMoves acc pos = acc ++ (generateValidMoves board pos color)
                                    in  filterJumpsRequirements $ Prelude.foldl appendMoves [] positions

setBoard :: Turn -> Board -> Turn
setBoard (Turn beats move _) board = Turn beats move $ Just board

updateAndSetBoard :: Board -> Turn -> Turn
updateAndSetBoard board turn = setBoard turn $ makeTurnLast board turn

generateAndSetBoard :: Board -> Turn -> Turn
generateAndSetBoard board turn = setBoard turn $ makeTurn board turn

getBoard :: Turn -> Board
getBoard (Turn _ _ board) = fromJust board
