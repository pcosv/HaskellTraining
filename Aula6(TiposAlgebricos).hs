-- AULA 6 - TIPOS ALGÉBRICOS

{-
data Nome_do_Tipo
  = Construtor1 t11 ... t1k1
  | Construtor2 t21 ... t2k2
    ....
  | Construtorn tn1 ... tnkn

1. Um valor do tipo Nome_do_Tipo pode ser criado usando qualquer um dos construtores definidos
2. Quando um tipo algebrico é definido, algumas classes podem ser instanciadas diretamente atraves da palavra reservada deriving para permitir operações básicas
-}

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

{-

Defina as funções:

showExpr :: Expr -> String
toList :: List t -> [t]
fromList :: [t] -> List t
depth :: Tree t -> Int
collapse :: Tree t -> [t]
mapTree :: (t −> u) -> Tree t -> Tree u

-}

-- showExpr :: Expr -> String
