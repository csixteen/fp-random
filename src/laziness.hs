-- Generates an infinite sequence of prime numbers
-- using the Sieve of Eratosthenes
primes :: [Int]
primes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]


-- Generates an infinite Fibonacci sequence
fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
