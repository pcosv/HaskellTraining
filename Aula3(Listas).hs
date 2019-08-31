-- AULA 3 - LISTAS EM HASKELL

-- dobrar os elementos de uma lista
double :: [Int] -> [Int]
double [] = []
double (x:xs) = x * 2 : double xs

-- determinar se um valor faz parte de uma lista
member :: [Int] -> Int -> Bool
member [] n = False
member (x:xs) n
 | x == n = True
 | otherwise = member xs n

-- filtrar apenas os números de uma lista
digito :: Char -> Bool
digito ch = ('0' <= ch) && (ch <= '9')

digits :: String -> String
digits [] = []
digits (x:xs)
 | digito x = x : digits xs
 | otherwise = digits xs

-- somar uma lista de pares
sumPairs :: [(Int, Int)] -> [Int]
sumPairs [] = [0]
sumPairs [(a, b)] = [a + b]
sumPairs ((a, b):xs) = a + b : (sumPairs xs)

-- função que devolve uma lista como os n primeiros elementos da lista de entrada
take :: [t] -> Int -> [t]
take [] n = []
