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

-- Questão 3 (considerei o descrescente como estritamente descrescente)
isDecrescente :: [Int] -> Bool

isDecrescente [] = True
isDecrescente (l:ls)
    | length ls == 0 = True
    | l <= head(ls) = False 
    | otherwise = isDecrescente ls

-- Questao 4
histograma :: [String] -> [(String, Int)]

conta :: String -> [String] -> Int
conta string [] = 0
conta string (s:ss) 
    | string == s = 1 + conta string ss 
    | otherwise = conta string ss 

histograma [] = []
histograma string:strings
    | conta string 