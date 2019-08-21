
-- Defina uma função que calcula o fatorial de um inteiro dado como argumento --
fat :: Int -> Int
fat 0 = 0
fat n = n + fat (n - 1)

-- Defina uma função que compara se quatro valores inteiros são iguais --
all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal a b c d = (a == b) && (a == c) && (a == d) && (b == c) && (b == d) && (c == d)

-- Defina uma função que retorna quantos argumentos são iguais --
equalCount :: Int -> Int -> Int -> Int
equalCount  a b c
 |(a == b) && (b == c) = 3
 |(a == b) || (a == c) || (b == c) = 2
 |otherwise = 0

-- Defina uma função que, dado um valor inteiro s e um número de semanas n, retorna quantas semanas de 0 a n tiveram vendas iguais a s.
