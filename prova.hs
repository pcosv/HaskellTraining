{-
Paula Crislaine de Oliveira Souza Vaz
Matricula: 09886880422
Login: pcosv
-}

-- Questão 1
-- (map (+2). filter((>5).(+2)))

-- Questão 2

-- (a)
merge :: Ord a => [a] -> [a] -> [a]
merge [] a = a
merge b [] = b
merge (a:as) (b:bs) =
    if a < b
    then a : merge (as) (b:bs)
    else b : merge (bs) (a:as)

-- (b)
msort :: Ord a => [a] -> [a]
msort [] = []
msort [a] = [a]
msort l = merge (msort (fst (metade l))) (msort (snd (metade l)))

-- (c)
metade:: [a] -> ([a], [a])
metade [] = ([],[])
metade [a] = ([a],[])
metade l = (take (div (length l) 2) l, drop (div (length l) 2) l)


-- Questão 3

type Texto = String
type Id = String
type DataHoraPub = Int

data Post = Post (Id, DataHoraPub) Texto deriving (Show, Eq)
data Thread = Nil | T Post (Thread)

testThread = T(Post ("joao",1) "texto1")(T (Post ("maria",2) "texto2") Nil)

-- (a)
instance Show Thread where
    show Nil = ""
    show (T (Post (i, d) txt) (th)) = "(" ++ i ++ " " ++ show d ++ " " ++ txt ++ ")" ++ show th

-- (b)
inserirPost :: Post -> Thread -> Thread
inserirPost p Nil = T p (Nil)
inserirPost p t = T p (t)

-- (c)
threadToList :: Thread -> [Post]
threadToList (Nil) = []
threadToList (T p (thread)) = p : threadToList thread

-- (d)
listToThread :: [Post] -> Thread
listToThread [] = Nil
listToThread (x:xs) = T x (listToThread xs)

-- (e)
--removerPost :: (Id, DataHoraPub) -> Thread -> Thread
--removerPost (a,b) t =
--    if (length filter (==) threadToList t) == 0
--    then t
--    else -- remove elemento
