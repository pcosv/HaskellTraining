-- AULA 3 - LISTAS EM HASKELL

double :: [Int] -> [Int]
double [] = []
double (x:xs) = x * 2 : double xs


member :: [Int] -> Int -> Bool
member [] n = False
member (x:xs) n
 | x == n = True
 | otherwise = member xs n

digito :: Char -> Bool
digito ch = ('0' <= ch) && (ch <= '9')

digits :: String -> String
digits [] = []
digits (x:xs)
 | digito x = x : digits xs
 | otherwise = digits xs
