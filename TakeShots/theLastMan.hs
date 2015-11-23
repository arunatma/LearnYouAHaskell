-- Puzzle stmt : 
-- N people standing in a circle in an order 0 to (N - 1) 
-- Index 0 has a sword. He kills next person (1 modulo 2) an gives sword to next to next (0 modulo 2). 
-- The game continues till the last man standing.

getLastMan :: Int -> Int
getLastMan n = getMan n 0 [0 .. n-1]

getMan :: Int -> Int -> [Int] -> Int
getMan _ _ [x] = x
getMan n pr xs = getMan qt rm (shorten pr xs)
	where qt = n `quot` 2
	      rm = n `mod` 2
		  
shorten :: Int -> [Int] -> [Int]
shorten 0 xs = dropEvery 2 xs
shorten 1 xs = dropEvery 2 (drop 1 xs)

dropEvery :: Int -> [Int] -> [Int]
dropEvery _ [] = []
dropEvery n xs = take (n - 1) xs ++ dropEvery n (drop n xs)

-- A much shorter solution
getLastManSimple n = (2 * n - p) `mod` p
	where p =  nextHighPower2 n

nextHighPower2 n =  (2 ^ (ceiling $ (log n) / (log 2)))
