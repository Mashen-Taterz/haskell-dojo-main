module TTT.A3 where

import Data.List (transpose)
import TTT.A1
import TTT.A2
import Data.List.NonEmpty (prependList)

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
isColEmpty (x:_) 0    = x == E 
isColEmpty (_:xs) col = isColEmpty xs (col -1)

-- Q#05
dropFirstCol :: Board -> Board
dropFirstCol []    = []
dropFirstCol board = map dropFirst board

dropFirst :: Row -> Row
dropFirst []       = []
dropFirst (_ : xs) = xs

dropLastCol :: Board -> Board
dropLastCol []    = []
dropLastCol board = map dropLast board 

dropLast :: Row -> Row
dropLast []  = [] 
dropLast row = init row

-- Q#06
getDiag1 :: Board -> Line
getDiag1 []              = []
getDiag1 ([] : _)        = []
getDiag1 ((x : _): rows) = x : getDiag1 (map tail rows) 

getDiag2 :: Board -> Line
getDiag2 []       = []
getDiag2 ([] : _) = []
getDiag2 rows     = reverse (getDiag1 (reverse rows))

getAllLines :: Board -> [Line]
getAllLines board = concat [board, transpose board, [getDiag1 board, getDiag2 board]]

-- Q#07
putSquare :: Player -> Board -> Move -> Board
putSquare _ [] _ = []
putSquare player (row:rows) (moveRow, moveCol)
    | moveRow == 0 = replaceSquareInRow player moveCol row : rows
    | otherwise = row : putSquare player rows (moveRow - 1, moveCol)

-- Q#08
prependRowIndices :: [String] -> [String]
prependRowIndices inputList = worker (indexRowStrings inputList)
    where
        worker :: [(Char, String)] -> [String]
        worker [] = []
        worker ((indexChar, str) : pairs) =
            (indexChar : str) : worker pairs 


-- Q#09

isWinningLine_ = undefined

-- Q#10

isValidMove = undefined