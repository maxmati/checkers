module Moves where

import qualified Data.Map as Map
import Data.Maybe
import Board

data Move = Move {from:: Pos, to:: Pos} deriving Show
data Turn = Turn Bool [Move] (Maybe Board) deriving Show

beatenPoses :: Pos -> Pos -> [Pos]
beatenPoses (fromX, fromY) (toX, toY) = let endX   = toX - fromX
                                            startX = signum endX
                                            xMod   = if endX > startX then [startX..endX - 1] else [endX..startX + 1]
                                            endY   = toY - fromY
                                            startY = signum endY
                                            yMod   = if endY > startY then [startY..endY - 1] else [endY..startY + 1]
                                        in  [(fromX + x, fromY + y) | x <- xMod, y <- yMod, abs x == abs y ]

makeMove :: Board -> [Move] -> Board
makeMove brd moves = let figure = fromJust $ Board.lookup (from (head moves)) brd
                         makeSingleMove (Move from to) brd = Board.insert to figure $ Board.delete from $ foldr Board.delete brd $ beatenPoses from to
                     in  foldr makeSingleMove brd moves

generateMoves :: Pos -> Color -> [Turn]
generateMoves from@(fromX, fromY) color = let yMod = if color == Black then 1 else -1
                                              positions = [(fromX + xAdd, fromY + yMod ) | xAdd <- [-1,1]]
                                          in  map (\to -> Turn True [(Move from to)] Nothing) positions

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
removeReqBeating brd color turns = let getLastMove (Turn _ moves _) = last moves
                                       keep = not . isInvalidJump brd color . getLastMove
                                   in  filter keep turns


removeInvalidMoves :: Board -> Color -> [Turn] -> [Turn]
removeInvalidMoves board color turns = removeReqBeating board color $ removeOccupied board $ removeOffBoard turns

addMultiJumps :: Turn -> [Turn]
addMultiJumps (Turn b moves brd) = let lastPos = to $ last moves
                                       availableMoves = generateJumpsPos lastPos
                                   in  map (\to -> Turn b (moves ++ [Move lastPos to]) brd) availableMoves

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
                                          then  (removeInvalidMoves board color $ generateMoves from color)
                                                ++ generateValidJumps board from color
                                          else []

generateAllValidMoves :: Board -> Color -> [Turn]
generateAllValidMoves board color = let positions = [(x,y) | x <- [1..8], y <- [1..8]]
                                        appendMoves acc pos = acc ++ (generateValidMoves board pos color)
                                    in  Prelude.foldl appendMoves [] positions

setBoard :: Turn -> Board -> Turn
setBoard (Turn beats move _) board = Turn beats move $ Just board

generateAndSetBoard :: Board -> Turn -> Turn
generateAndSetBoard board turn@(Turn _ moves _) =  setBoard turn $ makeMove board moves

getBoard :: Turn -> Board
getBoard (Turn _ _ board) = fromJust board
