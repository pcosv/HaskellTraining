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
myTake :: [t] -> Int -> [t]
myTake [] n = []
myTake (x:xs) 0 = []
myTake (x:xs) n = x : myTake xs (n-1)

-- função que devolve uma lista contendo os elementos da lista de entrada, exceto pelos n primeiros
myDrop :: [t] -> Int -> [t]
myDrop [] n = []
myDrop (x:xs) 0 = (x:xs)
myDrop (x:xs) n = myDrop xs (n-1)

-- função que devolve uma lista contendo todos os elementos da lista de entrada que antecedem o primeiro para o qual a função predicado produz valor False
predicado :: Int -> Bool
predicado n
  |n >= 10 = True
  |otherwise = False

myTakeWhile :: [Int] -> [Int]
myTakeWhile [] = []
myTakeWhile (x:xs)
  |predicado x == True = x : myTakeWhile xs
  |predicado x == False = []

-- função que devolve uma lista contendo todos os elementos da lista de entrada que antecedem o primeiro para o qual a função predicado produz valor True
myDropWhile :: [Int] -> [Int]
myDropWhile [] = []
myDropWhile (x:xs)
  |predicado x == True = []
  |predicado x == False = x : myDropWhile xs


type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa,Livro)]

baseExemplo :: BancoDados
baseExemplo = [("Sergio","O Senhor dos Aneis"), ("Andre", "Duna"), ("Fernando", "Jonathan Strange & Mr. Norrell"), ("Fernando" ,"Duna")]

-- Funções sobre a base de dados - consultas
livros :: BancoDados -> Pessoa -> [Livro]
livros [] p = []
livros (x:xs) p
  |fst x == p = snd x : (livros xs p)
  |fst x /= p = (livros xs p)

emprestimos :: BancoDados -> Livro -> [Pessoa]
emprestimos [] l = []
emprestimos (x:xs) l
  |snd x == l = fst x : (emprestimos xs l)
  |snd x /= l = (emprestimos xs l)

emprestado :: BancoDados -> Livro -> Bool
emprestado [] l = False
emprestado (x:xs) l
  |snd x == l = True
  |snd x /= l = emprestado xs l

qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos [] p = 0
qtdEmprestimos (x:xs) p
  |fst x == p = 1 + qtdEmprestimos xs p
  |fst x /= p = qtdEmprestimos xs p

-- Funções sobre a base de dados - atualizações
emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados
emprestar [] p l = [(p,l)]
emprestar (x:xs) p l = (p,l):(x:xs)

devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver [] p l = []
devolver (x:xs) p l
  |x == (p,l) = xs
  |x /= (p,l) = x : devolver xs p l
