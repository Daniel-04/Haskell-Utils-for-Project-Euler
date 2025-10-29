module EUtils where
import Data.Char (digitToInt)
import System.Process (readProcess)
import System.IO.Unsafe (unsafePerformIO)

anyMult :: [Int] -> Int -> Bool
anyMult divs n = any (\x -> mod n x == 0) divs

fibs :: [Integer]
fibs = 1 : 1 : zipWith(+) fibs (tail fibs)

allDiv :: [Int] -> Int -> Bool
allDiv divs n = all (\x -> mod n x == 0) divs

primes :: [Int]
primes = 2 : 3 : sieve (tail primes) [5,7..]
  where
    sieve (p:ps) xs = h ++ sieve ps [x | x <- t, x `rem` p /= 0]
      where
        (h,~(_:t)) = span (< p*p) xs

windows :: Int -> [a] -> [[a]]
windows n xs
  | n <= 0 = []
  | length xs < n = []
  | otherwise = take n xs : windows n (tail xs)

triangle :: Int -> Int
triangle n = div ((n+1) * n) 2

factors :: Int -> [Int]
factors n = factors' n 1
  where
    factors' n x
      | x * x > n = []
      | x * x == n = [x]
      | mod n x == 0 = x : div n x : factors' n (x + 1)
      | otherwise = factors' n (x + 1)

-- sorted, exclude n
divisors :: Int -> [Int]
divisors n = 1 : factors' 2
  where
    factors' x
      | x * x > n = []
      | x * x == n = [x]
      | mod n x == 0 = x : factors' (x + 1) ++ [div n x]
      | otherwise = factors' (x + 1)

factorial :: Integer -> Integer
factorial n = product [1..n]

nCr :: Integer -> Integer -> Integer
nCr n k =
  let
    n' = factorial n
    k' = factorial k
    nsubk' = factorial (n-k)
  in
    div n' (k' * nsubk')

twoSum :: [Int] -> Int -> Maybe (Int, Int)
twoSum xs n = twoSum' xs (reverse xs)
  where
    twoSum' [] _ = Nothing
    twoSum' (x:xs) ys@(y:ys')
      | x + y == n = Just (x, y)
      | x + y < n = twoSum' xs ys
      | otherwise = twoSum' (x:xs) ys'
    twoSum' _ _ = Nothing

digits :: Integer -> [Integer]
digits n = map (fromIntegral.digitToInt) $ show n

-- trust me bro 8)
curl :: String -> String
curl url = unsafePerformIO $ readProcess "curl" ["-f", "-sS", "-L", url] ""
