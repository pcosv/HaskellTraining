
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



-- Questão 5
filtrarEInserir :: [[Int]] -> Int -> ([[Int]], Int)
filtrarEInserir [] _ = ([], 0)
filtrarEInserir l n = (filtrarListas l, (maximo l)*n)

maximo :: [[Int]] -> Int
maximo [] = 0
maximo [a] = sum a
maximo (x:xs) = max (sum x) (maximo xs)

filtrarListas :: [[Int]] -> [[Int]]
filtrarListas [] = []
filtrarListas (x:xs) =
  if ((isAValidArray x) == True)
  then x : filtrarListas xs
  else filtrarListas xs

isAValidArray :: [Int] -> Bool
isAValidArray l =
  if ((sum [e | e <- l, e `mod` 2 /= 0]) > (sum [e | e <- l, e `mod` 2 == 0]))
  then True
  else False

-- Questão 6
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap fa fb [] = []
altMap fa fb [a] = [fa a]
altMap fa fb (a:b:xs) = fa a : fb b : altMap fa fb xs

somaDez :: Int -> Int
somaDez n = n + 10

somaCem :: Int -> Int
somaCem n = n + 100

-- Questão 7
type Codigo = Int

data Voto
  = Presidente Codigo
  | Senador Codigo
  | Deputado Codigo
  | Branco deriving (Show, Eq)

type Urna = [Voto]

type Apuracao = [(Voto, Int)]

urna = [(Presidente 5000), (Presidente 5001), (Senador 100), (Senador 101),
  (Deputado 50000), (Deputado 50001), (Deputado 50001), (Deputado 50001)]

-- (a)
totalVotos :: Urna -> Voto -> Int
totalVotos [] v = 0
totalVotos (x:xs) v =
  if x == v
  then 1 + totalVotos xs v
  else totalVotos xs v

-- (b)
apurar :: Urna -> Apuracao
apurar [] = []
apurar (u:us) = (u, totalVotos (u:us) u) : apurar (filter (\x -> x /= u) us)
