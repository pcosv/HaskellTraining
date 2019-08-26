-- AULA 2 - TIPOS BÁSICOS EM HASKELL

-- Defina a funçãoo addEspacos que produz um string com uma quantidade n de espaços. --
addEspacos :: Int -> String
addEspacos 0 = ""
addEspacos n = " " ++ addEspacos (n - 1)

{-
Defina a função paraDireita utilizando a definição de addEspacos para adiciconar uma quantidade n de espaços à esquerda de um dado
String, movendo o mesmo para a direita.
-}
paraDireita :: Int -> String -> String
paraDireita 0 a = a
paraDireita n a = addEspacos n ++ a

-- Defina a função menorMaior que recebe três inteiros e retorna uma tupla com o menor e o maior deles, respectivamente.
menorMaior :: Int -> Int -> Int -> (Int, Int)
menorMaior a b c
  |(a <= b) && (a <= c) && (b <= c) = (a, c)
  |(a <= b) && (a <= c) && (c <= b) = (a, b)
  |(b <= a) && (b <= c) && (a <= c) = (b, c)
  |(b <= a) && (b <= c) && (c <= a) = (b, a)
  |(c <= a) && (c <= b) && (a <= b) = (c, b)
  |(c <= a) && (c <= b) && (b <= a) = (c, a)

-- Defina a função ordenaTripla que recebe uma tripla de inteiros e ordena a mesma.
ordenaTripla :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTripla (a, b, c)
    |(a <= b) && (a <= c) && (b <= c) = (a, b, c)
    |(a <= b) && (a <= c) && (c <= b) = (a, c, b)
    |(b <= a) && (b <= c) && (a <= c) = (b, a, c)
    |(b <= a) && (b <= c) && (c <= a) = (b, c, a)
    |(c <= a) && (c <= b) && (a <= b) = (c, a, b)
    |(c <= a) && (c <= b) && (b <= a) = (c, b, a)

{-
Defina funções que retornem a primeira coordenada de um ponto, a segunda coordenada de um ponto e indique se uma
reta é vertical ou não (x1 = x2)
-}

type Ponto = (Float, Float)
type Reta = (Ponto, Ponto)

primeiraCoordenada :: Ponto -> Float
primeiraCoordenada (a,b) = a

segundaCoordenada :: Ponto -> Float
segundaCoordenada (a,b) = b

ehVertical :: Reta -> Bool
ehVertical ((a,b),(c,d))
  |a == c = True
  |otherwise = False
