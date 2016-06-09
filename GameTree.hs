module GameTree where

import Board
import Moves

import Data.Tree
import Data.Ord
import Data.Foldable

getBestTurn :: Color -> Board -> Turn
getBestTurn color brd = snd bestMove
                        where movesForest = getForestNLevels 5 $ generateMovesTree color brd
                              posibleMoves = forestToList movesForest
                              movesScores = map (minimize (rateTurn color)) movesForest
                              bestMove = maximumBy (comparing fst) $ zip movesScores posibleMoves

forestToList :: Forest a -> [a]
forestToList forest = map (\(Node a _) -> a) forest

generateMovesTree :: Color -> Board -> [Tree Turn]
generateMovesTree color board = let makeTurn move = generateAndSetBoard board move
                                    nextColor = if color == Black then White else Black
                                    nextTurn = map makeTurn $ generateAllValidMoves board color
                                    createNode move = Node move (generateMovesTree nextColor $ getBoard move)
                                in  map createNode nextTurn

maximize :: (a -> Int) -> Tree a -> Int
maximize mapper = maximize' mapper (minBound, maxBound)

maximize' :: (a -> Int) -> (Int, Int) -> Tree a -> Int
maximize' mapper _ (Node n []) = mapper n
maximize' mapper (_, beta) (Node _ sub) = mapmax mapper beta sub

minimize :: (a -> Int) -> Tree a -> Int
minimize mapper = maximize' mapper (minBound, maxBound)

minimize' :: (a -> Int) -> (Int, Int) -> Tree a -> Int
minimize' mapper _ (Node n []) = mapper n
minimize' mapper (alpha, _) (Node _ sub) = mapmin mapper alpha sub

mapmax :: (a -> Int) -> Int -> Forest a -> Int
mapmax = mapmax' minBound

mapmax' :: Int -> (a -> Int) -> Int -> Forest a -> Int
mapmax' cmax _ _ [] = cmax
mapmax' cmax mapper beta forest
    | current <= beta = current
    | otherwise = mapmax' (max current cmax) mapper beta (tail forest)
    where current = minimize' mapper (cmax, beta) (head forest)

mapmin :: (a -> Int) -> Int -> Forest a -> Int
mapmin = mapmin' maxBound

mapmin' :: Int -> (a -> Int) -> Int -> Forest a -> Int
mapmin' cmin _ _ [] = cmin
mapmin' cmin mapper alpha forest
    | current >= alpha = current
    | otherwise = mapmin' (min current cmin) mapper alpha (tail forest)
    where current = maximize' mapper (alpha, cmin) (head forest)


countColorFields :: Color -> Int -> Figure -> Int
countColorFields color acc (_, figureColor) = if color == figureColor then acc + 1 else acc

rateTurn :: Color -> Turn -> Int
rateTurn color (Turn _ _ brd) = maybe 0 (rateBoard color) brd

rateBoard :: Color -> Board -> Int
rateBoard color brd = let myCount = Board.foldl (countColorFields color) 0 brd
                          enemyCount = Board.foldl (countColorFields $ opositeColor color) 0 brd
                      in  2 * myCount - enemyCount

getForestNLevels :: Int -> Forest a -> Forest a
getForestNLevels n forest = map (getNLevels (n)) forest

getNLevels :: Int -> Tree a -> Tree a
getNLevels 1 (Node a _) = Node a []
getNLevels n (Node a subtrees) = Node a $ map (getNLevels (n - 1)) subtrees

