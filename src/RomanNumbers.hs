{-|
Module      : RomanNumbers
Description : Convert Roman numbers to integers and the other way around.
Copyright   : STUDENT NAME 1 (ID)
              STUDENT NAME 2 (ID)

RomanNumbers is a module to convert Roman numbers to integers and vice versa. Roman numbers are represented by the characters 'M', 'D', 'C', 'L', 'X', 'V', and 'I'

-}

module RomanNumbers
    ( -- * Convert Roman numbers to integers 
      r2i
      -- * Convert integers to Roman numbers
    , i2r
    ) where


-- Implement and document r2i
r2i rs = 1
texttoinput :: Char -> Int
texttoinput 'M' = 1000
texttoinput 'D' = 500
texttoinput 'C' = 100
texttoinput 'L' = 50
texttoinput 'X' = 10
texttoinput 'V' = 5
texttoinput 'I' = 1

-- Implement and document i2r
i2r n  = "I"
