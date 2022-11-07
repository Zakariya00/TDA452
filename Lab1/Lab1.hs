{- Lab 1
   Date: 2022-11-02
   Authors: Zakariya Omar
   Lab group: 18
 -}
--------------------------------------------
import Test.QuickCheck

power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)

-- A ------------------------
-- stepsPower n k gives the number of steps that
-- power n k takes to compute
-- Solution: power n k takes (k+1) steps to compute
stepsPower :: Integer -> Integer -> Integer
stepsPower n k
  | k < 0 = error "stepsPower: negative argument"
stepsPower n k = k + 1

-- B -------------------------
-- power1
power1 :: Integer -> Integer -> Integer
power1 n k
   | k < 0 = error "power: negative argument"
power1 n 0 = 1
power1 n k = product [n | x <- [1..k]]

-- C -------------------------
-- power2
power2 :: Integer -> Integer -> Integer
power2 n k
   | k < 0 = error "power: negative argument"
power2 n 0 = 1
power2 n k
   | even k = power2 (n * n) (div k 2)
power2 n k
   | odd k = n * power2 n (k - 1)

-- D -------------------------
{-
-D.1
   Valid & Legal inputs (testing odd, even and base cases):
    - (n, k) = 8 0  ---> 1
    - (n, k) = 9 0  ---> 1
    - (n, k) = 5 2  ---> 25
    - (n, k) = 3 3  ---> 27
    - (n, k) = 2 2  ---> 4
    - (n, k) = 0 2  ---> 0
    - (n, k) = 0 0  ---> 1

   Invalid & Illegal inputs (testing negative values to invoke an error):
   - (n, k) = 4 -1  ---> error
 -}

-- D.2
prop_powers :: Integer -> Integer -> Bool
prop_powers n k = power n k == power1 n k &&
                  power n k == power2 n k

-- D.3
test_cases = [(prop_powers 8 0), (prop_powers 9 0),
              (prop_powers 5 2), (prop_powers 3 3),
              (prop_powers 2 2), (prop_powers 0 2),
              (prop_powers 0 0)]

powerTest :: Bool
powerTest = and test_cases

-- D.4
-- Quick check fails when it attempts negative values. Using abs remedies that
prop_powers' :: Integer -> Integer -> Bool
prop_powers' n k = prop_powers n (abs k)