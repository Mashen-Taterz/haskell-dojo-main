module TTT.A2 where

import Data.List (intercalate)
import TTT.A1
import Data.Char (digitToInt, readLitChar)
import Control.Applicative (Alternative(empty))
import Data.Foldable (Foldable(toList))

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
    | otherwise     = -1

-- Q#04
_EMPTY_ROW_ :: [Square]
_EMPTY_ROW_ = replicate _SIZE_ E

_EMPTY_BOARD_:: [[Square]]
_EMPTY_BOARD_ = replicate _SIZE_ _EMPTY_ROW_ 

-- Q#05
isTied :: Board -> Bool
isTied board = E `notElem` concat board    

_TIED_BOARD_ :: Board 
_TIED_BOARD_ = [
    [X, O, O]
  , [O, X, X]
  , [O, X, O]
  ]

-- Q#06
indexRowStrings :: [String] -> [(Char, String)]
indexRowStrings strings = zip ['A' ..] strings 
 

-- Q#07
formatLine :: [String] -> String
formatLine line = _SEP_ ++ intercalate _SEP_ line ++ _SEP_

-- Q#08
isMoveInBounds :: Move -> Bool
isMoveInBounds (row , col) =
    inBounds row && inBounds col
    where 
        inBounds index = index >= 0 && index < _SIZE_

-- Q#09
stringToMove :: String -> Move
stringToMove input
    | length input == 2 = convert input
    | otherwise         = _INVALID_MOVE_
        where
            convert [row, col] = (convertRowIndex row, readDigit col)
            convert          _ = _INVALID_MOVE_

-- Q#10
replaceSquareInRow :: Player -> Int -> Row -> Row
replaceSquareInRow player col row =
  let (before, after) = splitAt col row
      newSegment      = if col < 0 || col >= length row then after else replaceElement player after
  in before ++ newSegment

replaceElement :: Player -> Row -> Row
replaceElement          _ [] = []
replaceElement player (_:xs) = player : xs

rsX :: Int -> Row -> Row
rsX = replaceSquareInRow X

rsO :: Int -> Row -> Row
rsO = replaceSquareInRow O
