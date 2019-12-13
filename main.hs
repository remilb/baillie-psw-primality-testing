import           PrimalityBPSW

-- Count the number of primes <= 1000000
main = print . length . filter isPrimeBPSW $ [1 .. 1000000]
