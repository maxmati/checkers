import Board
import Moves

import Data.Tree

--TODO: king jumps

generateMovesTree :: Color -> Board -> [Tree Turn]
generateMovesTree color board = let makeTurn move = generateAndSetBoard board move
                                    nextColor = if color == Black then White else Black
                                    nextTurn = map makeTurn $ generateAllValidMoves board color
                                    createNode move = Node move (generateMovesTree nextColor $ getBoard move)
                                in  map createNode nextTurn


nodesAtLevel :: Int -> Tree a -> [a]
nodesAtLevel 0 (Node a _) = [a]
nodesAtLevel n (Node _ subtrees) = concatMap (nodesAtLevel (n-1)) subtrees

b = stringToBoard initBoard
testBoard1 = ".b.b.b.b\nb.b.b.b.\n.b.b.w.b\n........\n........\nw.w.w.w.\n.w.w.w.w\nw.w.w.w."
tb1 = stringToBoard testBoard1

testBoard2 = unlines [".b.b.b.b",
                      "..w.b.b.",
                      ".....w.b",
                      "..w.w...",
                      "........",
                      "w.w.w.w.",
                      ".w.w.w.w",
                      "w.w.w.w."]
tb2 :: Board
tb2 = stringToBoard testBoard2

main :: IO()
main = putBoard tb2