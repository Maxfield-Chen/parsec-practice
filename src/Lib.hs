module Lib where

import           Text.ParserCombinators.Parsec

csvFile = endBy line eol
line = sepBy cell (char ',')
cell = many (noneOf ",\n")
eol = char '\n'

parseCsv :: String -> Either ParseError [[String]]
parseCsv = parse csvFile "(unknown)"

someFunc :: IO ()
someFunc = print "hello"
