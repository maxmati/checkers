import System.Environment
import Control.Monad.Trans.State.Lazy
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number
import Text.ParserCombinators.Parsec.Error
import System.IO
import Data.Maybe
import Control.Monad.IO.Class
import Debug.Trace

import Board
import Data.Foldable
import GamePlay
--- TODO przetestowac
-- uproszczony "parser" PDN
-- ruch Int-Int
-- bicie [Intx]Int

b = stringToBoard initBoard


-- instance Show PDN where
--   show (Move (a,c)) = (show a)++ "-"++ (show c)
--   show (Kill [p]) = (show p)
--   show (Kill (p:ps)) = (show p)++ "x" ++ show (Kill ps)


parsePos :: Parser Int
parsePos = do
            x <- int
            if (x<1 || x>32) then
              unexpected "Tylko liczby od 1-32"
            else
              return x

parseMove :: Parser PDN
parseMove = do
            x1 <- parsePos
            (char '-')
            x2 <- parsePos
            eof
            return $ Move (x1,x2)

parseKill :: Parser PDN
parseKill = do
            x1 <- sepBy (parsePos) (char 'x')
            eof
            if (length x1) > 1 then
              return $ Kill x1
            else
              unexpected "start i koniec minimum"

parsePDN :: Parser PDN
parsePDN =  try parseMove <|> parseKill

type Game a = IO a

play :: Color -> Board -> String -> Game Board
play color brd i
    |trace i False = undefined
    |otherwise  = do
  brd' <- case parse parsePDN "sPDN err" i of
    Right move -> return $ makeMove brd move
    Left x -> fail $ show x
  putStr $ show $ brd'
  (move, brd'') <- return $ makeBestTurn color brd'
  putStrLn $ show move
  hFlush stdout -- konkretny ruch trzeba wygenerowac
  return brd''


doPlay :: Color -> Board -> Game Board
doPlay color brd = getLine >>= (play color brd) >>= (doPlay color)


main :: IO Board
main = do
  args <- getArgs
  progName <- getProgName
  mapM_ putStrLn args
  putStrLn progName
--  let args = [""] -- do zakomentowania w programmie
  case (listToMaybe args) of
    Just "b" -> doPlay Black b
    Just "w" -> do
          putStr $ show $ b
          (move, brd') <- return $ makeBestTurn White b
          putStr $ show $ brd'
          putStrLn $ show $ move
          hFlush stdout
          doPlay White brd'
    Nothing -> doPlay Black b