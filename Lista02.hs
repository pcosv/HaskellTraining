
-- Questão 1
c :: [Int] -> [Int]
c (x:xs)  =  [ x | (x,y) <- zip (x:xs) xs, x == y ]

d :: [Int] -> [Int]
d [x]       =  []
d (x:y:zs)
  | x==y = x : d (y:zs)
  | otherwise  = d (y:zs)

-- Questão 2
g ::[Int] -> Bool
g [] = True
g l =
  if length (filter naoEhPar (filter (<=100) l)) == 0
    then True
    else False

naoEhPar :: Int -> Bool
naoEhPar n =
  if (n `mod` 2) /= 0
  then True
  else False

-- Questão 3
type Fabricante = String
type Potencia = Float

-- (a)
data Lampada
  = Compacta Fabricante Potencia
  | Incandescente Fabricante Potencia

-- (b)
instance Show Lampada where
  show (Compacta fc pc) = "Compacta" ++ " " ++ fc ++ " " ++ show pc
  show (Incandescente fi poI) = "Incandescente" ++ " " ++ fi ++ " " ++ show poI

-- (c)
instance Eq Lampada where
  (Compacta f1 p1) == (Compacta f2 p2) = f1 == f2 && p1 == p2
  (Incandescente f1 p1) == (Incandescente f2 p2) = f1 == f2 && p1 == p2
  _ == _ = False
