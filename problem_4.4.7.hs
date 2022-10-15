module Problem_4'4'7 where

import Probelm_4'4'6

findDigit :: [Char] -> Maybe Char

findDigitOrX :: [Char] -> Char
findDigitOrX xs = case findDigit xs of
    Just x  -> x
    Nothing -> 'X'