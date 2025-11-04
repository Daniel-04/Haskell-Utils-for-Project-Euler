module EUtils where

import Data.Char (digitToInt)
import Data.List (foldl', tails)
import Debug.Trace (traceShow)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)

anyMult :: (Integral a) => [a] -> a -> Bool
anyMult divs n = any (\x -> mod n x == 0) divs

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

allDiv :: (Integral a) => [a] -> a -> Bool
allDiv divs n = all (\x -> mod n x == 0) divs

primes :: [Int]
primes = 2 : 3 : sieve (tail primes) [5, 7 ..]
  where
    sieve (p : ps) xs = h ++ sieve ps [x | x <- t, x `rem` p /= 0]
      where
        (h, ~(_ : t)) = span (< p * p) xs

primeFactors :: (Integral a) => a -> [a]
primeFactors n = factor n 2
  where
    factor 1 _ = []
    factor n f
      | mod n f == 0 = f : factor (div n f) f
      | f * f > n = [n]
      | otherwise = factor n (f + 1)

windows :: Int -> [a] -> [[a]]
windows n xs = takeWhile ((n ==) . length) $ map (take n) $ tails xs

diagonalsTLBR :: [[a]] -> [[a]]
diagonalsTLBR xss =
  [ [ xss !! i !! j
      | (i, row) <- zip [0 ..] xss,
        let j = k - i,
        j >= 0,
        j < length row
    ]
    | k <- [0 .. (rows + cols - 2)]
  ]
  where
    rows = length xss
    cols = if null xss then 0 else length (head xss)

diagonalsTRBL :: [[a]] -> [[a]]
diagonalsTRBL xss =
  [ [ xss !! i !! j
      | (i, row) <- zip [0 ..] xss,
        let j = i + k,
        j >= 0,
        j < length row
    ]
    | k <- [-(cols - 1) .. (rows - 1)]
  ]
  where
    rows = length xss
    cols = if null xss then 0 else length (head xss)

triangle :: (Integral a) => a -> a
triangle n = div ((n + 1) * n) 2

numToWords :: (Integral a) => a -> String
numToWords n
  | n <= 19 = units !! fromIntegral n
  | n < 100 =
      let (t, u) = n `divMod` 10
       in tens !! fromIntegral t ++ if u /= 0 then "-" ++ numToWords u else ""
  | n < 1000 =
      let (h, r) = n `divMod` 100
       in numToWords h ++ " hundred" ++ if r /= 0 then " and " ++ numToWords r else ""
  | n == 1000 = "one thousand"
  where
    units = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
    tens = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

countBy :: (Integral b) => (a -> Bool) -> [a] -> b
countBy p = foldl' (\acc a -> if p a then acc + 1 else acc) 0

wordsBy :: (Char -> Bool) -> String -> [String]
wordsBy p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsBy p s''
    where
      (w, s'') = break p s'

factors :: (Integral a) => a -> [a]
factors n = factors' n 1
  where
    factors' n x
      | x * x > n = []
      | x * x == n = [x]
      | mod n x == 0 = x : div n x : factors' n (x + 1)
      | otherwise = factors' n (x + 1)

-- sorted, exclude n
divisors :: (Integral a) => a -> [a]
divisors n = 1 : factors' 2
  where
    factors' x
      | x * x > n = []
      | x * x == n = [x]
      | mod n x == 0 = x : factors' (x + 1) ++ [div n x]
      | otherwise = factors' (x + 1)

factorial :: (Integral a) => a -> a
factorial n = product [1 .. n]

nCr :: (Integral a) => a -> a -> a
nCr n k =
  let n' = factorial n
      k' = factorial k
      nsubk' = factorial (n - k)
   in div n' (k' * nsubk')

twoSum :: [Int] -> Int -> Maybe (Int, Int)
twoSum xs n = twoSum' xs (reverse xs)
  where
    twoSum' [] _ = Nothing
    twoSum' (x : xs) ys@(y : ys')
      | x + y == n = Just (x, y)
      | x + y < n = twoSum' xs ys
      | otherwise = twoSum' (x : xs) ys'
    twoSum' _ _ = Nothing

digits :: (Num a, Show a) => a -> [a]
digits n = map (fromIntegral . digitToInt) $ show n

-- trust me bro 8)
curl :: String -> String
curl url = unsafePerformIO $ readProcess "curl" ["-f", "-sS", "-L", url] ""
