somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (x:xs) = x + somaLista xs


multDois :: [Int] -> [Int]
multDois [] = []
multDois (x:xs) = 2*x : multDois xs

multInt :: Int -> [Int] -> [Int]
multInt _ []     = []  
multInt y (x:xs) = y * x : multInt y xs

elemento :: Int -> [Int] -> Bool
elemento _ [] = False
elemento n (x:xs) = n == x || elemento n xs

conta :: Int -> [Int] -> Int
conta _ [] = 0
conta n (x:xs)
    | n == x = 1 + conta n xs
    | otherwise = conta n xs

contaMaior :: Int -> [Int] -> Int
contaMaior _ [] = 0
contaMaior n (x:xs)
    | n < x = 1 + contaMaior n xs
    | otherwise = contaMaior n xs

maiores :: Int -> [Int] -> [Int] 
maiores _ [] = []
maiores n (x:xs)
    | n < x = x : maiores n xs
    | otherwise = maiores n xs

geraLista :: Int -> Int -> [Int]
geraLista m n
    | m <= 0 = []
    | otherwise = n : geraLista (m-1) n

addFim :: Int -> [Int] -> [Int]
addFim n [] = [n]
addFim n (x:xs) = x : addFim n xs

join :: [Int] -> [Int] -> [Int]
join lista1 lista2 = lista1 ++ lista2

inverte :: [Int] -> [Int]
inverte []     = []     
inverte (x:xs) = inverte xs ++ [x]
