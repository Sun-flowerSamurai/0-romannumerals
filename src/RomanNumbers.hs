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
r2i rs = sum (map rho rs)

rho :: Char -> Int
rho 'M' = 1000
rho 'D' = 500
rho 'C' = 100
rho 'L' = 50
rho 'X' = 10
rho 'V' = 5
rho 'I' = 1
rho x = error ("Unexpected character; '" ++ [x] ++ "' is not a Roman numeral.")

-- Implement and document i2r
-- | i2r converts a number to an (additive) roman numeral.
--  
i2r :: Int -> String
i2r n = if (n < 0) then error "There are no negative Roman numbers" else strmult (n `div` 1000) "M" ++ strmult ((n `mod` 1000) `div` 500) "D" ++ strmult ((n `mod` 500) `div` 100) "C" ++ strmult ((n `mod` 100) `div` 50) "L" ++ strmult ((n `mod` 50) `div` 10) "X" ++ strmult((n `mod` 10) `div` 5) "V" ++ strmult(n `mod` 5) "I"

-- i2r n = strmult (n `div` 1000) "M" ++ strmult ((n `mod` 1000) `div` 500) "D" ++ strmult ((n `mod` 500) `div` 100) "C" ++ strmult ((n `mod` 100) `div` 50) "L" ++ strmult ((n `mod` 50) `div` 10) "X" ++ strmult((n `mod` 10) `div` 5) "V" ++ strmult(n `mod` 5) "I"

i2r' :: Int -> String
i2r' i 
  | i >= 0  = gi2r 1 i
  | i < 0  = error "There are no negative Roman numbers."


gi2r :: (Eq a, Num a) => a -> Int -> [Char]
gi2r z 0 = ""
gi2r z i = strmult q a ++ gi2r (z + 1) r
  where (a,b) = cnt z
        (q,r) = divMod i b

cnt :: (Eq a, Num a, Num b) => a -> ([Char], b)
cnt 1 = ("M", 1000)
cnt 2 = ("D", 500)
cnt 3 = ("C", 100)
cnt 4 = ("L", 50)
cnt 5 = ("X", 10)
cnt 6 = ("V", 5)
cnt 7 = ("I", 1)
cnt _ = error "This should not happen?!"


-- | Implements repeating a string (or any list) as would be expected from e.g. python. 
-- example: strmult 3 "abc" = "abcabcabc"
strmult :: Int -> [a] -> [a]
strmult 0 zs = []
strmult n xs = xs ++ (strmult (n-1) xs)