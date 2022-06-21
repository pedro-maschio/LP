idade :: Int 
idade = 17

maiorIdade :: Bool
maiorIdade = (idade >= 18)

teste :: Int -> Int
teste a = idade * 3

(***) :: Int -> Int -> Int 
a *** b  | a < b = a 
         | otherwise = b

ouEx :: Bool -> Bool -> Bool 
ouEx x y = (x || y) && not(x && y)
