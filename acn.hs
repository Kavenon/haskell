module Acn where
import Text.ParserCombinators.Parsec
import Data.Char
import DataTypes
import Chessboard

moveToAcn :: (Int, Int) -> (Int, Int) -> ACN
moveToAcn (row, col) (movR, movC) = ACN (colToAcn col, rowToAcn row, colToAcn ( movC), rowToAcn ( movR ))

colToAcn col = chr (ord 'a' + col)
rowToAcn row = chr (ord '0' + (-row + 8))

colFromAcn col = ( ord col -ord 'a')
rowFromAcn row = -(ord row - ord '0') + 8


parsePosC :: Parser Char
parsePosC = oneOf "abcdefgh"

parsePosN :: Parser Char
parsePosN = oneOf "12345678"

parseACN :: Parser ACN
parseACN = do
      x1 <- parsePosC
      y1 <- parsePosN
      x2 <- parsePosC
      y2 <- parsePosN
      return $ ACN (x1,y1,x2,y2)
