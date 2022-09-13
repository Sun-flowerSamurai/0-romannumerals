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
-- Returns an error if it is not a valid Roman numeral.

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
i2r :: Int -> String
-- i2r n = 1
i2r n = strmult (n `div` 1000) "M" ++ strmult ((n `mod` 1000) `div` 500) "D" ++ strmult ((n `mod` 500) `div` 100) "C" ++ strmult ((n `mod` 100) `div` 50) "L" ++ strmult ((n `mod` 50) `div` 10) "X" ++ strmult((n `mod` 10) `div` 5) "V" ++ strmult(n `mod` 5) "I"




-- | Implements repeating a string (or any list) as would be expected from e.g. python. 
-- example: strmult 3 "abc" = "abcabcabc"
strmult :: Int -> [a] -> [a]
strmult 0 zs = []
strmult n xs = xs ++ (strmult (n-1) xs)