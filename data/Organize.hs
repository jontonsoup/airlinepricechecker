module Organize where

import Text.Parsec hiding ((<|>), many)
import Control.Applicative
import Data.Function

type Parser = Parsec String ()
data Browser = Firefox | Tor deriving (Show, Read, Eq, Ord)
data Flight = Flight { airlineName :: String
                     , airLineCode :: String
                     , num :: String
                     } deriving (Show, Read, Eq)
              
data Item = Item { date :: String
                 , time :: String
                 , flight :: Flight
                 , price :: Int -- Times $100
                 } deriving (Show, Read, Eq)

data Run = Run { browser :: Browser
               , ip :: String
               , runs :: [Item]
               } deriving (Show, Read, Eq)

scraperRunsP :: Parser [Run]
scraperRunsP = (scraperRunP `sepEndBy` (many (char '_') <* newline)) <* eof

scraperRunP :: Parser Run
scraperRunP = Run <$> browserP <* newline
                  <*> ipP <* newline
                  <*> many1 (itemP <* newline)
  where itemP = Item <$> dateP <* space
                     <*> timeP <* (space >> middle >> space)
                     <*> flightP <* string " | "
                     <*> priceP

browserP :: Parser Browser
browserP = read <$> (string "Firefox" <|> string "Tor")

ipP :: Parser String
ipP = ds <.> ds <.> ds <.> ds
  where ds = try (count 3 digit) <|> try (count 2 digit) <|> try (count 1 digit)
        p1 <.> p2 = do c1 <- p1
                       char '.'
                       c2 <- p2
                       return $ c1 ++ "." ++ c2

dateP :: Parser String
dateP = do y <- count 4 digit
           char '-'
           m <- count 2 digit
           char '-'
           d <- count 2 digit
           return $ y ++ m ++ d

timeP :: Parser String
timeP = do h <- two
           char ':'
           m <- two
           char ':'
           s <- two
           return $ h ++ ":" ++ m ++ ":" ++ s
  where two = count 2 digit

middle :: Parser String
middle = string "-0600 |"

flightP :: Parser Flight
flightP = Flight <$> many1 (letter <|> space)
                 <*> parens (count 2 alphaNum) <* space
                 <*> many1 digit
  where parens p = do char '('
                      s <- p
                      char ')'
                      return s

priceP :: Parser Int
priceP = read <$> many1 digit
