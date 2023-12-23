module TTT.A2 where

import Data.List (intercalate)
import TTT.A1
import Data.Char (digitToInt)

-- Q#01
promptPlayer :: Player -> String 
promptPlayer player = 
    concat ["Player ", show player, "'s turn: enter a row and column position."]

-- Q#02
_RANGE_ :: [Int]
_RANGE_ = [0 .. 2]

-- Q#03
isDigit :: Char -> Bool
isDigit digit = digit `elem` ['0' .. '9']

readDigit :: Char -> Int
readDigit digit
    | isDigit digit = read [digit] :: Int 
    | otherwise = -1

-- Q#04

_EMPTY_ROW_ = undefined

_EMPTY_BOARD_ = undefined

-- Q#05

isTied = undefined

_TIED_BOARD_ = undefined

-- Q#06

indexRowStrings = undefined

-- Q#07

formatLine = undefined

-- Q#08

isMoveInBounds = undefined

-- Q#09

stringToMove = undefined

-- Q#10

replaceSquareInRow = undefined