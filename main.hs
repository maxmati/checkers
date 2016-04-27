data Color = Black | White deriving Show
data Type = Pawn | King deriving Show
newtype Figure = Maybe (Type, Color) deriving Show
type Row = [Figure]
type Board = [Row]

initBoard = ".b.b.b.b\nb.b.b.b.\n.b.b.b.b\n........\n........\nw.w.w.w.\n.w.w.w.w\nw.w.w.w."

charToField 'b' = Just (Pawn, Black)
charToField 'w' = Just (Pawn, White)
charToField a = Nothing

stringToRow string = map charToField string
stringToBoard string = map stringToRow (lines string)

fieldToChar (Just (Pawn, Black)) = '♟'
fieldToChar (Just (King, Black)) = '♛'
fieldToChar (Just (Pawn, White)) = '♙'
fieldToChar (Just (King, White)) = '♕'
fieldToChar Nothing = '.'

rowToString row = map fieldToChar row
boardToString board = unlines (map rowToString board)

putBoard board = putStr $  boardToString board
