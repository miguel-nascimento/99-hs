-- 31. Determine whether a given integer number is prime.
-- SLOW CODE WARNING!
-- https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes

import Data.List ((\\))

primes :: Int -> [Int]
primes n = sieve [2 .. n]
  where
    sieve (x : xs) = x : sieve (xs \\ [x, x + x .. n])
    sieve [] = []

isPrime :: Int -> Bool
isPrime n = filter (== n) (primes n) == [n]

-- 32. Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.
myGCD :: Int -> Int -> Int
myGCD a 0 = a
myGCD a b = abs $ myGCD b (a `mod` b)

-- 33. Determine whether two positive integer numbers are coprime.
-- Two numbers are coprime if their greatest common divisor equals 1.
coprime :: Int -> Int -> Bool
coprime a b = myGCD a b == 1

-- 34. Calculate Euler's totient function phi(m).
tothient :: Int -> Int
tothient 1 = 1
tothient n = length $ [x | x <- [1 .. n], coprime x n]