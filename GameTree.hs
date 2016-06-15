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
listBlackPawn = [[4, 4, 4, 4, 4, 4, 4, 4],
                 [4, 3, 3, 3, 3, 3, 3, 4],
                 [4, 3, 2, 2, 2, 2, 3, 4],
                 [4, 3, 2, 1, 1, 2, 3, 4],
                 [4, 3, 2, 1, 1, 2, 3, 4],
                 [4, 3, 2, 2, 2, 2, 3, 4],
                 [4, 3, 3, 3, 3, 3, 3, 4],
                 [4, 4, 4, 4, 4, 4, 4, 4]]
listWhitePawn :: [[Float]]
listWhitePawn = reverse listBlackPawn
listBlackKing :: [[Float]]
listBlackKing = fmap (fmap (3*)) listBlackPawn
listWhiteKing :: [[Float]]
listWhiteKing = fmap (fmap (3*)) listWhitePawn

convertList :: Figure -> [[Float]] -> [((Figure, Pos), Float)]
convertList fig list = map toItem [(x,y) | x <- [1..8], y <- [1..8]]
                       where toItem pos@(x,y) = ((fig, pos), list!!(y-1)!!(x-1))

positionsRates :: Map.Map (Figure, Pos) Float
positionsRates = Map.fromList $ (convertList (Pawn, Black) listBlackPawn)
                             ++ (convertList (Pawn, White) listWhitePawn)
                             ++ (convertList (King, Black) listBlackKing)
                             ++ (convertList (King, White) listWhiteKing)


getBestTurn :: Color -> Board -> Maybe Turn
getBestTurn color brd
    | null movesForest = Nothing
    | otherwise = Just $ snd bestMove
    where movesForest = getForestNLevels 7 $ generateMovesTree color brd
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

maximise :: (a -> Float) -> Tree a -> Float
maximise mapper (Node element []) = mapper element
maximise mapper (Node _ sub) = maximum $ map (minimise mapper) sub

minimise :: (a -> Float) -> Tree a -> Float
minimise mapper (Node element []) = mapper element
minimise mapper (Node _ sub) = minimum $ map (maximise mapper) sub

maximize :: (a -> Float) -> Tree a -> Float
maximize mapper = maximum . (maximize' mapper)

maximize' :: (a -> Float) -> Tree a -> [Float]
maximize' mapper (Node a [])= [mapper a]
maximize' mapper (Node _ sub) = mapmin $ map (minimize' mapper) sub
                                where mapmin [] = []
                                      mapmin (nums:rest) = [minVal] ++ omit (minVal) rest
                                                           where minVal = minimum nums
                                      omit _ [] = []
                                      omit pot (nums:rest)
                                          | minVal <= pot = omit pot rest
                                          | otherwise = [minVal] ++ omit (minVal) rest
                                          where minVal = minimum nums

minimize :: (a -> Float) -> Tree a  -> Float
minimize mapper = minimum . (minimize' mapper)


minimize' :: (a -> Float) -> Tree a -> [Float]
minimize' mapper (Node a [])= [mapper a]
minimize' mapper (Node _ sub) = mapmax $ map (maximize' mapper) sub
                                where mapmax [] = []
                                      mapmax (nums:rest) = [maxVal] ++ omit (maxVal) rest
                                                           where maxVal = maximum nums
                                      omit _ [] = []
                                      omit pot (nums:rest)
                                          | maxVal >= pot = omit pot rest
                                          | otherwise = [maxVal] ++ omit (maxVal) rest
                                          where maxVal = maximum nums


rateTurn :: Color -> Turn -> Float
rateTurn color (Turn _ _ brd) = maybe 0 (rateBoard color) brd

rateBoard :: Color -> Board -> Float
rateBoard color brd = Board.foldlWithKey addRate 0 brd
                      where addRate acc pos fig@(_, currentColor)
                                | currentColor == color = acc + (getPoints (fig,pos)) + 200
                                | otherwise = acc - (getPoints (fig,pos)) - 200
                            getPoints k = fromMaybe 0 $ Map.lookup k positionsRates

getForestNLevels :: Int -> Forest a -> Forest a
getForestNLevels n forest = map (getNLevels (n)) forest

getNLevels :: Int -> Tree a -> Tree a
getNLevels 1 (Node a _) = Node a []
getNLevels n (Node a subtrees) = Node a $ map (getNLevels (n - 1)) subtrees

