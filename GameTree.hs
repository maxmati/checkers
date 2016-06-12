module GameTree (getBestTurn, rateBoard) where

import Board
import Moves
import Utils

import Data.Tree
import qualified Data.Map as Map
import Data.Ord
import Data.Maybe
import Data.Foldable


listBlackPawn :: [[Float]]
listBlackPawn = [[0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7],
                 [0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7],
                 [0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7],
                 [0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8],
                 [0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9],
                 [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0],
                 [1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5],
                 [1.7, 1.7, 1.7, 1.7, 1.7, 1.7, 1.7, 1.7]]
listWhitePawn :: [[Float]]
listWhitePawn = reverse listBlackPawn
listBlackKing :: [[Float]]
listBlackKing = [[2 | x <- [0..7]] | y <- [0..7]]
listWhiteKing :: [[Float]]
listWhiteKing = [[2 | x <- [0..7]] | y <- [0..7]]

convertList :: Figure -> [[Float]] -> [((Figure, Pos), Float)]
convertList fig list = map toItem [(x,y) | x <- [1..8], y <- [1..8]]
                       where toItem pos@(x,y) = ((fig, pos), list!!(y-1)!!(x-1))

positionsRates :: Map.Map (Figure, Pos) Float
positionsRates = Map.fromList $ (convertList (Pawn, Black) listBlackPawn)
                             ++ (convertList (Pawn, White) listWhitePawn)
                             ++ (convertList (King, Black) listBlackKing)
                             ++ (convertList (King, White) listWhiteKing)


getBestTurn :: Color -> Board -> Turn
getBestTurn color brd = snd bestMove
                        where movesForest = getForestNLevels 15 $ generateMovesTree color brd
                              posibleMoves = forestToList movesForest
                              movesScores = map (minimize (rateTurn color)) movesForest
                              bestMove = maximumBy (comparing fst) $ zip movesScores posibleMoves

forestToList :: Forest a -> [a]
forestToList = map (\(Node a _) -> a)

generateMovesTree :: Color -> Board -> [Tree Turn]
generateMovesTree color board = let updateBoard = generateAndSetBoard board
                                    nextColor = if color == Black then White else Black
                                    nextTurn = map updateBoard $ generateAllValidMoves board color
                                    createNode move = Node move (generateMovesTree nextColor $ getBoard move)
                                in  map createNode nextTurn

maximize :: (a -> Float) -> Tree a -> Float
maximize mapper = maximize' mapper (-inf, inf)

maximize' :: (a -> Float) -> (Float, Float) -> Tree a -> Float
maximize' mapper _ (Node n []) = mapper n
maximize' mapper (_, beta) (Node _ sub) = mapmax mapper beta sub

minimize :: (a -> Float) -> Tree a -> Float
minimize mapper = maximize' mapper (-inf, inf)

minimize' :: (a -> Float) -> (Float, Float) -> Tree a -> Float
minimize' mapper _ (Node n []) = mapper n
minimize' mapper (alpha, _) (Node _ sub) = mapmin mapper alpha sub

mapmax :: (a -> Float) -> Float-> Forest a -> Float
mapmax = mapmax' $ -inf

mapmax' :: Float -> (a -> Float) -> Float -> Forest a -> Float
mapmax' cmax _ _ [] = cmax
mapmax' cmax mapper beta forest
    | current <= beta = current
    | otherwise = mapmax' (max current cmax) mapper beta (tail forest)
    where current = minimize' mapper (cmax, beta) (head forest)

mapmin :: (a -> Float) -> Float -> Forest a -> Float
mapmin = mapmin' inf

mapmin' :: Float -> (a -> Float) -> Float -> Forest a -> Float
mapmin' cmin _ _ [] = cmin
mapmin' cmin mapper alpha forest
    | current >= alpha = current
    | otherwise = mapmin' (min current cmin) mapper alpha (tail forest)
    where current = maximize' mapper (alpha, cmin) (head forest)


countColorFields :: Color -> Int -> Figure -> Int
countColorFields color acc (_, figureColor) = if color == figureColor then acc + 1 else acc

rateTurn :: Color -> Turn -> Float
rateTurn color (Turn _ _ brd) = maybe 0 (rateBoard color) brd

rateBoard :: Color -> Board -> Float
rateBoard color brd = Board.foldlWithKey addRate 0 brd
                      where addRate acc pos fig@(_, currentColor)
                                | currentColor == color = acc + (getPoints (fig,pos))
                                | otherwise = acc - (getPoints (fig,pos))
                            getPoints k = fromMaybe 0 $ Map.lookup k positionsRates

getForestNLevels :: Int -> Forest a -> Forest a
getForestNLevels n forest = map (getNLevels (n)) forest

getNLevels :: Int -> Tree a -> Tree a
getNLevels 1 (Node a _) = Node a []
getNLevels n (Node a subtrees) = Node a $ map (getNLevels (n - 1)) subtrees

