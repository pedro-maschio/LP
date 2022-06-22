import Data.List
{--
    Aluno: Pedro de Torres Maschio
    Matrícula: 190018763
--}

-- Questão 1
maior4 :: Int -> Int -> Int -> Int -> Int 

maior4 a b c d 
    |   a > b && a > c && a > d = a
    |   b > a && b > c && b > d = b
    |   c > a && c > b && c > d = c
    | otherwise = d


-- Questão 2
converterNotaParaMencao :: Float -> String

converterNotaParaMencao nota 
    | nota >= 9 = "SS"
    | nota >= 7 && nota < 9 = "MS"
    | nota >= 5 && nota < 7 = "MM"
    | nota >= 3 && nota < 5 = "MI"
    | nota >= 0.1 && nota < 3 = "II"
    | otherwise = "SR"

-- Questão 3 (considerei o decrescente como estritamente decrescente)
isDecrescente :: [Int] -> Bool

isDecrescente [] = True
isDecrescente (l:ls)
    | ls == [] = True
    | l <= head(ls) = False 
    | otherwise = isDecrescente ls

-- Questão 4
--histograma :: [String] -> [(String, Int)]


-- Questão 5
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

myZipWith f [] [] = []
myZipWith f (a:as) (b:bs) = (f a b) : (myZipWith f as bs)

-- Questão 6
aprovadosOrdemDeMedia :: [(String, Float, Float)] -> [(String, Float)]

aprovadosOrdemDeMedia [] = []
aprovadosOrdemDeMedia ((a, b, c):alunos) 
    | media >= 5 = sortBy (\(_,a) (_,b) -> compare a b)  ((a, media):(aprovadosOrdemDeMedia alunos))
    | otherwise = aprovadosOrdemDeMedia alunos 
    where 
        media = ((b + c)/2.0)
        