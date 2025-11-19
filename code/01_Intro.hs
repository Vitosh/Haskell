import Data.Char    (toLower, isAlpha)
import Data.List    (sort, group, groupBy, sortOn)
import Data.Function (on)
import Text.Read    (readMaybe)
import Data.Unique 

isPalindrome :: String -> Bool
isPalindrome s = clean == reverse clean
  where
    clean = [toLower c | c <- s, isAlpha c]

fizzbuzz :: Int -> [String]
fizzbuzz n = [fb k | k <- [1..n]]
  where
    fb m | m `mod` 15 == 0 = "FizzBuzz"
         | m `mod` 3  == 0 = "Fizz"
         | m `mod` 5  == 0 = "Buzz"
         | otherwise       = show m

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

wordFreq :: String -> [(String, Int)]
wordFreq txt =
  let cleaned  = [if isAlpha c then toLower c else ' ' | c <- txt]
      ws       = words cleaned
      grouped  = group (sort ws)
  in [(head g, length g) | g <- grouped]

factorial :: Integer -> Integer
factorial n
  | n < 0     = 0
  | n == 0    = 1
  | otherwise = n * factorial (n - 1)

triangleTriples :: [(Integer, Integer, Integer)]
triangleTriples = [(a, b, c) | c <- [1..], b <- [1..c], a <- [1..b], a*a + b*b == c*c]


