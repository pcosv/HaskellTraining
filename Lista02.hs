
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
