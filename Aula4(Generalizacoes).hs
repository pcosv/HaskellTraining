-- AULA 4 - FUNÇÕES DE ALTA ORDEM

-- Dada uma função, verificar se ela é crescente em um intervalo de 0 a n
isCrescent :: (Int -> Int) -> Int -> Bool
isCrescent f 0 = True
isCrescent f 1 = True
isCrescent f n
  |f n >= f (n-1) = True
  |otherwise = False

square :: Int -> Int
square x = x * x

-- defina a função que eleva os itens de uma lista ao quadrado (mapping)
squareList :: [Int] -> [Int]
squareList x = map square x

-- defina a função que retorna a soma dos quadrados dos itens de uma lista (folding)
sumSquares :: [Int] -> Int
sumSquares x = foldr1 (+) (squareList x)

greaterThanZero :: Int -> Bool
greaterThanZero n
  |n > 0 = True
  |otherwise = False

-- defina a função que mantenha na lista todos os itens maiores que zero (filtering)
gtzList :: [Int] -> [Int]
gtzList x = filter greaterThanZero x

maiorElementoLista :: [Int] -> Int
maiorElementoLista [] = 0
maiorElementoLista (x:xs)
  |x > maiorElementoLista xs = x
  |otherwise = maiorElementoLista xs

-- defina uma função que: dada uma lista de listas de inteiros, devolve uma lista contendo o maior elemento de cada sub-lista da entrada
maiores :: [[Int]] -> [Int]
maiores [[]] = []
maiores [x] = [maiorElementoLista x]
maiores (x:xs) = maiorElementoLista x : maiores xs
