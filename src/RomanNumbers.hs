{-|
Module      : RomanNumbers
Description : Convert Roman numbers to integers and the other way around.
Copyright   : Matt Verhoeven (1728342)
              David Chen (1742477)

RomanNumbers is a module to convert Roman numbers to integers and vice versa. Roman numbers are represented by the characters 'M', 'D', 'C', 'L', 'X', 'V', and 'I'

-}

module RomanNumbers
    ( -- * Convert Roman numbers to integers 
      r2i
      -- * Convert integers to Roman numbers
    , i2r
    ) where


-- Implement and document r2i
-- | r2i converts a Roman numeral to the respective (arabic) numeral.
-- | returns an error if it is not a valid Roman numeral.

r2i :: String -> Int
r2i rs = sum  (map texttoinput rs)

texttoinput :: Char -> Int
texttoinput 'M' = 1000
texttoinput 'D' = 500
texttoinput 'C' = 100
texttoinput 'L' = 50
texttoinput 'X' = 10
texttoinput 'V' = 5
texttoinput 'I' = 1
texttoinput x = error ("Unexpected character; " ++ [x] ++" is not a Roman numeral.")

-- Implement and document i2r
i2r n  = "I"
