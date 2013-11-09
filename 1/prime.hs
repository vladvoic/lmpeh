divides :: Integer -> Integer -> Bool
divides k n = rem n k == 0

ldf :: Integer -> Integer -> Integer
ldf k n | divides k n = k
		| k^2 >= n = n
		| otherwise = ldf (k+1) n

ld :: Integer -> Integer
ld n = ldf 2 n

prime :: Integer -> Bool
prime n | n == 1 = False
		| n < 0 = error "Negative numbers are tricky"
		| otherwise = ld n == n