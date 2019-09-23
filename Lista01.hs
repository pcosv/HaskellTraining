
-- Questão 2
sublistas :: [a] -> [[a]]
sublistas [] = [[]]
sublistas (x:xs) = [x:sublist | sublist <- sublistas xs] ++ sublistas xs

-- Questão 3
-- (a)
poli :: Int -> Int -> Int -> Int -> Int
poli a b c = (\x -> a*(x*x) + b*x + c)

-- (b)
listaPoli :: [(Int,Int,Int)] -> [Int -> Int]
listaPoli [] = []
listaPoli ((a,b,c):xs) = poli a b c : listaPoli xs
--listaPoli l = [ poli a b c | (a,b,c) <- l ]

-- (c)
appListaPoli :: [Int -> Int] -> [Int] -> [Int]
appListaPoli [] _ = []
appListaPoli (f:fs) (a:as) = f a : appListaPoli fs as

-- Questão 4
-- (a)
isMatrix :: [[a]] -> Bool
isMatrix [] = True
isMatrix [a] = True
isMatrix (a:b:xs) = length a == length b && isMatrix (xs)

-- (b)
