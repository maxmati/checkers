module GamePlay where

import Board
import Moves
import GameTree

data PDN =   Move (Int,Int) -- pozycja startowa i koncowa
           | Kill [Int]  -- pozycja startowa to glowa, pozniej kolejne pozycje
           deriving Eq

instance Show PDN where
   show (GamePlay.Move (a,c)) = (show a)++ "-"++ (show c)
   show (Kill [p]) = (show p)
   show (Kill (p:ps)) = (show p)++ "x" ++ show (Kill ps)
   show (Kill []) = undefined

makeBestTurn :: Color -> Board -> (PDN, Board)
makeBestTurn color brd = turnToIdBoard $ getBestTurn color brd

turnToIdBoard :: Turn -> (PDN, Board)
turnToIdBoard (Turn isJump moves (Just brd))
    | isJump = (jumpToPDN moves, brd)
    | otherwise = (moveToPDN moves, brd)
turnToIdBoard (Turn _ _ Nothing) = undefined

jumpToPDN :: [Move] -> PDN
jumpToPDN moves = Kill $ [(posToId . getFrom . head) moves] ++ map (posToId . getTo) moves

moveToPDN :: [Move] -> PDN
moveToPDN [move] = GamePlay.Move ((posToId . getFrom) move, (posToId . getTo) move)
moveToPDN _ = undefined

makeMove :: Board -> PDN -> Board
makeMove brd (GamePlay.Move (from, to)) = makeTurn brd $ Turn False (idsToMoves [from, to]) Nothing
makeMove brd (Kill ids) = makeTurn brd $ Turn True (idsToMoves ids) Nothing

idsToMoves :: [Int] -> [Move]
idsToMoves (from:to:t) = [Moves.Move (idToPos from) (idToPos to)] ++ (idsToMoves (to:t))
idsToMoves [_] = []
idsToMoves [] = []

idToPos :: Int -> Pos
idToPos id
    | odd row = (col', row)
    | otherwise = (col' - 1, row)
    where row = ( (id - 1) `div` 4 ) + 1
          col' = ( ( (id - 1) `mod` 4 ) + 1 ) * 2

posToId :: Pos -> Int
posToId (x,y)
    | odd y = (y-1) * 4 + x `div` 2
    | otherwise = (y-1) * 4 + (x+1) `div` 2