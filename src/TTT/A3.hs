module TTT.A3 where

import Data.List (transpose)
import TTT.A1
import TTT.A2

-- Q#01
showInts :: [Int] -> [String]
showInts []       = []
showInts (x : xs) = show x : showInts xs

--_HEADER_ :: String
_HEADER_ = ' ' : formatLine (showInts _RANGE_)

-- Q#02
showSquares :: [Square] -> [String] 
showSquares []       = []
showSquares (x : xs) = showSquare x : showSquares xs 

-- Q#03
formatRows :: [Row] -> [String] 
formatRows []       = []
formatRows (x : xs) = formatLine (showSquares x) : formatRows xs 

-- Q#04
isColEmpty :: Row -> Int -> Bool
isColEmpty [] _       = False
isColEmpty (x:_) 0    = x == Empty 
isColEmpty (_:xs) col = isColEmpty xs (col -1)

-- Q#05
dropFirstCol :: Board -> Board
dropFirstCol []    = []
dropFirstCol board = map dropFirst board

dropFirst :: Row -> Row
dropFirst []     = []
dropFirst (_:xs) = xs

dropLastCol :: Board -> Board
dropLastCol []    = []
dropLastCol board = map dropLast board 

dropLast :: Row -> Row
dropLast []   = [] 
dropLast elem = init elem

-- Q#06

getDiag1 = undefined

getDiag2 = undefined

getAllLines = undefined

-- Q#07

putSquare = undefined

-- Q#08

prependRowIndices = undefined

-- Q#09

isWinningLine_ = undefined

-- Q#10

isValidMove = undefined