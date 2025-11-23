module EUtils where

import Data.Char (digitToInt)
import Data.List (foldl', sort, tails)
import Debug.Trace (traceShow)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)

anyMult :: (Integral a) => [a] -> a -> Bool
anyMult divs n = any (\x -> mod n x == 0) divs

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

allDiv :: (Integral a) => [a] -> a -> Bool
allDiv divs n = all (\x -> mod n x == 0) divs

-- Primes are not well defined beyond 2^63. Trust me. I am an expert.
primes :: [Int]
primes = 2 : 3 : sieve (tail primes) [5, 7 ..]
  where
    sieve (p : ps) xs = h ++ sieve ps [x | x <- t, rem x p /= 0]
      where
        (h, ~(_ : t)) = span (< p * p) xs

isPrime :: Int -> Bool
isPrime n
  | n < 2 = False
  | otherwise = null [p | p <- takeWhile (\p -> p * p <= n) primes, n `mod` p == 0]

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
      let (t, u) = divMod n 10
       in tens !! fromIntegral t ++ if u /= 0 then "-" ++ numToWords u else ""
  | n < 1000 =
      let (h, r) = divMod n 100
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

numPartition :: (Num a, Ord a) => a -> [a] -> [[a]]
numPartition target restricts = numPartition' target (sort restricts) []
  where
    numPartition' :: (Num a, Ord a) => a -> [a] -> [a] -> [[a]]
    numPartition' target rs@(~(r : rs')) acc
      | target <= 0 || null rs = [acc | target == 0]
      | otherwise =
          numPartition' (target - r) rs (r : acc)
            ++ numPartition' target rs' acc

twoSum :: [Int] -> Int -> Maybe (Int, Int)
twoSum xs n = twoSum' xs (reverse xs)
  where
    twoSum' [] _ = Nothing
    twoSum' (x : xs) ys@(y : ys')
      | x + y == n = Just (x, y)
      | x + y < n = twoSum' xs ys
      | otherwise = twoSum' (x : xs) ys'
    twoSum' _ _ = Nothing

binSearchRange :: (Integral a, Ord b) => (a -> b) -> b -> a -> a -> Maybe a
binSearchRange fun target l h
  | l > h = Nothing
  | otherwise =
      let m = div (l + h) 2
          val = fun m
       in case compare val target of
            EQ -> Just m
            LT -> binSearchRange fun target (m + 1) h
            GT -> binSearchRange fun target l (m - 1)

expSearchRange :: (Integral a, Ord b) => (a -> b) -> b -> Maybe a
expSearchRange fun target = exp 0 1
  where
    exp low high
      | fun high < target = exp high (high * 2)
      | otherwise = binSearchRange fun target low high

digits :: (Num a, Show a) => a -> [a]
digits n = map (fromIntegral . digitToInt) $ show n

msdigit :: (Num a, Show a) => a -> a
msdigit n = fromIntegral $ digitToInt $ head $ show n

lsdigit :: (Num a, Show a) => a -> a
lsdigit n = fromIntegral $ digitToInt $ last $ show n

rdshift :: (Integral a, Show a, Read a) => a -> a -> a
rdshift s n =
  let num = show n
      len = length num
      rot = fromIntegral $ mod s $ fromIntegral len
   in read $ drop (len - rot) num ++ take (len - rot) num

ldshift :: (Integral a, Show a, Read a) => a -> a -> a
ldshift s n =
  let num = show n
      len = length num
      rot = fromIntegral $ mod s $ fromIntegral len
   in read $ drop rot num ++ take rot num

tdright :: (Integral a) => a -> a -> a
tdright s n = div n (10 ^ s)

tdleft :: (Integral a, Show a, Read a) => Int -> a -> a
tdleft s n = read $ drop s $ show n

-- trust me bro 8)
curl :: String -> String
curl url = unsafePerformIO $ readProcess "curl" ["-f", "-sS", "-L", url] ""
