module ReadPUtil where

import Data.Char (isDigit)

import Text.ParserCombinators.ReadP (ReadP,satisfy,(<++))


maxMany :: ReadP a -> ReadP [a]
maxMany p = maxMany1 p <++ return []
-- common usage from my AP class '15-'16
maxMany1 :: ReadP a -> ReadP [a]
maxMany1 p = (:) <$> p <*> maxMany p


int :: ReadP Int
int = read <$> maxMany1 (satisfy isDigit)

restofline :: ReadP String
restofline = maxMany1 $ satisfy (/='\n')
