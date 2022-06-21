-- Pedro de Torres Maschio
-- 190018763 


allEqual :: Int -> Int -> Int -> Bool
allEqual n m p = (n == m) && (m == p)

all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal a b c d = (allEqual a b c) && (allEqual b c d)


howManyEqual :: Int -> Int -> Int -> Int 
howManyEqual a b c | allEqual a b c = 3
                   | (a == b || a == c || b == c) = 2
                   | otherwise = 0

