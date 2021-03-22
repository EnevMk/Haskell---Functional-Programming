main :: IO()
main = do
    print("First homework")
    print (validate 4736778291034)
    print(specialSum 10 128)

-- task 1
isPrime :: Integer -> Bool
isPrime n = if n < 2 then False else helper 2
    where
        helper d
            | fromIntegral d > sqrt (fromIntegral n) = True
            | mod n d == 0 = False
            | otherwise = helper (d + 1)

-- the smallest prime number that can be presented as
-- primeNum = 2 * otherPrimeNum + 1 is 5. So if a is < 5, we have a == 5.
safePrimesCount :: Integer -> Integer -> Integer
safePrimesCount a b = if (a < 5) then helper 5 0 else helper a 0
    where
        helper n cnt
            | n > b = cnt
            | isPrime n && isPrime (div n 2) = helper (n + 1) (cnt + 1)
            | otherwise = helper (n + 1) cnt

specialSum :: Integer -> Integer -> Integer
specialSum k m = helper 2 0 0
    where 
        helper pow cnt sum
            | cnt == k = sum
            | isPrime pow && 2^pow - 1 > m = helper (pow + 1) (cnt + 1) (sum + 2^pow - 1)
            | otherwise = helper (pow + 1) cnt sum

-- task 2
reverseNum :: Integer -> Integer
reverseNum n = helper n 0
    where
        helper n res
            | n == 0 = res
            | otherwise = helper (div n 10) (res * 10 + mod n 10)

-- the helper function returns the modified card number reversed
-- and initially I called validate n = mod (sumOfDigits (reverseNum (helper n 0 0))) ...
-- but the order of digits doesn't matter when summing them.
validate :: Int -> Bool
validate n = mod (sumOfDigits (helper n 0 0)) 10 == 0
    where
        helper n turn res
            | n == 0 = res
            | turn == 0 = helper (div n 10) 1 (res * 10 + mod n 10)
            | (mod n 10) * 2 >= 10 = helper (div n 10) 0 (res * 10 + sumOfDigits ((mod n 10) * 2))
            | otherwise = helper (div n 10) 0 (res * 10 + ((mod n 10) * 2))

sumOfDigits :: Int -> Int
sumOfDigits n = helper n 0
    where
        helper n res
            | n == 0 = res
            | otherwise = helper (div n 10) (res + mod n 10)