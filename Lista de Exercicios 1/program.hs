idade :: Int
idade = 17

testeIdade :: Bool
testeIdade = idade >= 18

quadrado :: Int -> Int
quadrado x = x * x

mini :: Int -> Int -> Int
mini a b 
    | a <= b = a
    | otherwise = b

maiorDeIdade :: Int -> Bool
maiorDeIdade x = x >= 18

igual :: Int -> Int -> Bool
igual x y = x == y

tresIguais :: Int -> Int -> Int -> Bool
tresIguais x y z = (x == y) && (y == z)

palindromo :: String -> Bool
palindromo x = (reverse(x) == x)

verificaTriangulo :: Int -> Int -> Int -> Bool
verificaTriangulo x y z = ((x + y) > z) && ((x + z) > y) && ((z + y) > x)

sinal :: Int -> Int
sinal x
    | x > 0     = 1
    | x < 0     = -1
    | otherwise = 0

menorTres :: Int -> Int -> Int -> Int
menorTres x y z
    | (x > z) && (y > z) = z
    | (x > y) && (y < z) = y
    | (x < z) && (y > x) = x

fat :: Int -> Int 
fat 0 = 1
fat 1 = 1
fat n = n * (fat (n-1))

osQuatroSaoIguais :: Int -> Int -> Int -> Int -> Bool
osQuatroSaoIguais x y z w = (x == y) && (x == z) && (x == w) 

quantosSaoIguais :: Int -> Int -> Int -> Int
quantosSaoIguais x y z 
    | (x == y) && (x == z) = 3
    | (x == y) || (x == z) || (z == y) = 2
    | otherwise = 0

todosDiferentes :: Int -> Int -> Int -> Bool
todosDiferentes x y z = x /= y && y /= z && x /= z


-- Função principal
quantosSaoIguaisVDois :: Int -> Int -> Int -> Int
quantosSaoIguaisVDois x y z
    | todosDiferentes x y z = 0
    | x == y && y == z = 3
    | x == y || x == z || z == y = 2
    | otherwise = 0 -- Caso não seja nenhum dos casos acima, retorna 0

elevadoDois :: Int -> Int 
elevadoDois n = n * n

elevadoQuatro :: Int -> Int
elevadoQuatro n = n * n * n * n 
