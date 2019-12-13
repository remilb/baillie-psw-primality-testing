module PrimalityBPSW
    ( isPrimeBPSW
    )
where

import           Data.List

smallPrimes :: [Integer]
smallPrimes = filter isPrimeTrialDiv [1 .. 1000]

-- |Trial division primality testing for precomputing small primes
isPrimeTrialDiv :: (Integral a) => a -> Bool
isPrimeTrialDiv 0 = False
isPrimeTrialDiv 1 = False
isPrimeTrialDiv n = primes !! fromIntegral n
  where
    primes = [False, False] ++ map primetest [2 ..]
    primetest m =
        not . any (== 0) $ map (rem m) [2 .. floor . sqrt $ fromIntegral m]

-- |Baillie-PSW probable prime test
isPrimeBPSW :: Integer -> Bool
isPrimeBPSW 0 = False
isPrimeBPSW 1 = False
isPrimeBPSW 2 = True
isPrimeBPSW n
    | n < 0
    = False
    | isSmallPrime
    = True
    | otherwise
    = not (divisibleBySmallPrime n)
        && fermatStrongProbablePrime n
        && lucasStrongProbablePrime n
  where
    divisibleBySmallPrime n =
        any ((== 0) . (n `rem`)) $ takeWhile (< n) smallPrimes
    isSmallPrime  = n <= maxSmallPrime && elem n smallPrimes
    maxSmallPrime = maximum smallPrimes

-- |A strong base-2 Fermat probable prime test, see Fermat's little theorem
fermatStrongProbablePrime :: Integer -> Bool
fermatStrongProbablePrime n = pred1 || pred2
  where
    (d, s) = halveUntilOdd (n - 1)
    rs     = [0 .. s - 1]
    pred2  = any (== n - 1) . map (\r -> modExp 2 (d * 2 ^ r) n) $ rs
    pred1  = modExp 2 d n == 1


lucasStrongProbablePrime :: Integer -> Bool
lucasStrongProbablePrime n = case selfredgeParams n of
    Just (d, p, q) -> pred1 d p q || pred2 d p q
    Nothing        -> False
  where
    pred1 d p q = let (u, _) = lucasNumber d' n (d, p, q) in u == 0
    pred2 d p q = any (\k -> snd (lucasNumber k n (d, p, q)) == 0)
        $ map (\r -> d' * 2 ^ r) rs
    dn      = n + 1
    (d', s) = halveUntilOdd dn
    rs      = [0 .. s - 1]

-- |Uses method proposed by Selfredge to select parameters D, P, and Q for a Lucas prp test.
-- In selecting these parameters, this method may determine that the given integer is composite,
-- in which case it returns Nothing
selfredgeParams :: Integer -> Maybe (Integer, Integer, Integer)
selfredgeParams n = go n [5, -7, 9]
  where
    go n ds = case find ((< 1) . snd) dsAndJacobis of
        Just (d, 0 ) -> Nothing
        Just (d, -1) -> Just (d, 1, (1 - d) `div` 4)
        Nothing      -> if isSquare n
            then Nothing
            else go n (zipWith (*) [11, 13 ..] $ cycle [-1, 1])
        where dsAndJacobis = zip ds $ map (\a -> jacobiSymbol a n) ds


lucasNumber
    :: Integer -> Integer -> (Integer, Integer, Integer) -> (Integer, Integer)
lucasNumber n m (d, p, q) = foldr ($) (1, 1) indices
  where
    indices = unfoldr doubleOrAddOne n
    doubleOrAddOne m | m == 1    = Nothing
                     | even m    = Just (double, (m `div` 2))
                     | otherwise = Just (addOne, (m - 1))
    double (u, v) = (u * v `mod` m, half (v * v + d * u * u))
    addOne (u, v) = (half (p * u + v), half (d * u + p * v))
    half k = if even k then k `div` 2 `mod` m else (k + m) `div` 2 `mod` m


jacobiSymbol :: Integer -> Integer -> Integer
jacobiSymbol 0 n = 0
jacobiSymbol 1 n = 1
jacobiSymbol a n
    | a >= n || a < 0 = jacobiSymbol (a `mod` n) n
    | even a = if n `mod` 8 == 1 || n `mod` 8 == 7
        then jacobiSymbol (a `div` 2) n
        else -jacobiSymbol (a `div` 2) n
    | gcd a n /= 1 = 0
    | otherwise = if a `mod` 4 == 3 && n `mod` 4 == 3
        then -jacobiSymbol n a
        else jacobiSymbol n a


halveUntilOdd :: Integer -> (Integer, Integer)
halveUntilOdd n =
    head $ dropWhile (even . fst) [ (n `div` 2 ^ s, s) | s <- [1 ..] ]

-- |Performs fast modular exponentiation
modExp :: Integer -> Integer -> Integer -> Integer
modExp base exp modulo = modExp' (base `mod` modulo) exp modulo 1
  where
    modExp' b 0 m r = r
    modExp' b e m r = modExp' (b * b `mod` m)
                              (e `div` 2)
                              m
                              (if even e then r else (r * b `mod` m))

-- |Tests whether a given positive integer is a perfect square using Newton's method
isSquare :: Integer -> Bool
isSquare n = case isqrt of
    Just (rt, _) -> rt * rt == n
    Nothing      -> False
  where
    guesses = n : map (\x -> (x * x + n) `div` (2 * x)) guesses
    isqrt   = find (\(xn', xn) -> abs (xn' - xn) <= 1)
        $ zip (tail guesses) guesses
