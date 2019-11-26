module Lib where

import           Text.ParserCombinators.Parsec

csvFile :: GenParser Char st [[String]]
csvFile = do
  result <- many line
  _      <- eof
  return result

line :: GenParser Char st [String]
line = do
  result <- cells
  _      <- eol
  return result

cells :: GenParser Char st [String]
cells = do
  first <- cellContent
  next  <- remainingCells
  return (first : next)

remainingCells :: GenParser Char st [String]
remainingCells = (char ',' >> cells) <|> return [] 
-- Note that shortcutting can occur with choice, goes left to right

cellContent :: GenParser Char st String
cellContent = many (noneOf ",\n")

eol :: GenParser Char st Char
eol = char '\n'

parseCsv :: String -> Either ParseError [[String]]
parseCsv = parse csvFile "(unknown)"
