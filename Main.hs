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
import GameTree
-- uproszczony "parser" PDN
-- ruch Int-Int
-- bicie [Intx]Int

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

play :: Color -> Board -> String -> Game (Maybe Board)
play color brd i = do
  brd' <- case parse parsePDN "sPDN err" i of
    Right move -> return $ makeMove brd move
    Left x -> fail $ show x

  hPutStr stderr $ "Rate: \n" ++ (show (rateBoard color brd'))
  hPutStr stderr $ show $ brd'

  turn <- return $ makeBestTurn color brd'
  move <- return $ fst <$> turn
  brd'' <- return $ snd <$> turn

  rateStr <- return $ ("Rate: \n" ++) .  show . (rateBoard color) <$> brd''
  boardStr <- return $ show <$> brd''
  mapM_ (hPutStr stderr) $ (\a b -> a ++ "\n" ++ b) <$> rateStr <*> boardStr

  mapM_ putStrLn $ show <$> move
  hFlush stdout
  return brd''


doPlay :: Color -> Board -> Game ()
doPlay color brd = do
    line <- getLine
    brd' <- play color brd line
    mapM_ (doPlay color) brd'

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
--  mapM_ putStrLn args
--  putStrLn progName
--  let args = ["w"] -- do zakomentowania w programmie
  case (listToMaybe args) of
    Just "b" -> doPlay Black b
    Just "w" -> do
--          putStr $ show b
          (move, brd') <- return $ fromJust $ makeBestTurn White b
--          putStr $ show brd'
          putStrLn $ show $ move
          hFlush stdout
          doPlay White brd'
    Nothing -> doPlay Black b
  return ()