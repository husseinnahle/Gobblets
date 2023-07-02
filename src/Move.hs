module Move (Move, Drop, OnBoard, getMove, showMove, parseMoves) where

import Board (Position, showPosition)
import Deck (Size(Big, Medium, Small, Tiny))

import Data.Char (digitToInt)
import System.IO
import Text.Parsec
import Text.Parsec.String (Parser)


type Drop = (Size, Position)

showDrop :: Drop -> String
showDrop (size, p) = "drop(" ++ sizeStr size ++ ", " ++ showPosition p ++ ")"
  where
    sizeStr Big = "B"
    sizeStr Medium = "M"
    sizeStr Small = "S"
    sizeStr Tiny = "T"


type OnBoard = (Position, Position)

showOnBoard :: OnBoard -> String
showOnBoard (src, dst) = "onBoard(" ++ showPosition src ++ ", " ++ showPosition dst ++ ")"


type Move = Either Drop OnBoard

showMove :: Move -> String
showMove (Left dropMove) = showDrop dropMove
showMove (Right onboardMove) = showOnBoard onboardMove

positionParser :: Parser Position
positionParser = 
  do _ <- char '('
     skipMany space
     x <- digitToInt <$> oneOf "0123"
     skipMany space
     _ <- char ','
     skipMany space
     y <- digitToInt <$> oneOf "0123"
     skipMany space
     _ <- char ')'
     skipMany space
     return (x, y)

dropParser :: Parser Drop
dropParser = 
  do _ <- char '('
     skipMany space
     size <- do
       c <- oneOf "BMST"
       case c of
         'B' -> return Big
         'M' -> return Medium
         'S' -> return Small
         'T' -> return Tiny
         _ -> fail "Invalid size"
     skipMany space
     _ <- char ','
     skipMany space
     postition <- positionParser
     _ <- char ')'
     skipMany space
     return (size, postition)

onBoardParser :: Parser OnBoard
onBoardParser =
  do _ <- char '('
     skipMany space
     postition1 <- positionParser
     skipMany space
     _ <- char ','
     skipMany space
     postition2 <- positionParser
     _ <- char ')'
     skipMany space
     return (postition1, postition2)

-- Main move parser
moveParser :: Parser Move
moveParser = 
  do moveType <- try (string "drop") <|> string "onboard"
     skipMany space
     case moveType of
      "drop" -> Left <$> dropParser
      "onboard" -> Right <$> onBoardParser
      _ -> fail "Invalid move"

getMove :: IO (Maybe Move)
getMove = 
  do putStr "> "
     hFlush stdout
     input <- getLine
     let result = parse moveParser "" input
     case result of
        Left _ -> return Nothing
        Right move -> return (Just move)

parseMoves :: [String] -> [Move]
parseMoves = map parseMove
  where
    parseMove :: String -> Move
    parseMove str = case parse moveParser "" str of
      Left _ -> error "Invalid move"
      Right move -> move
