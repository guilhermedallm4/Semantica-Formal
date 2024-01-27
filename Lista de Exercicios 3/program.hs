data Temperatura = Frio | Calor
    deriving(Eq,Show)

data Estacao = Verao | Outono | Inverno | Primavera
    deriving(Eq, Show)


tempo :: Estacao -> Temperatura
tempo Verao = Calor
tempo _ = Frio

data Forma = Circulo Float | Retangulo Float Float 
    deriving(Eq,Show)

redondo :: Forma -> Bool
redondo (Circulo x) = True
redondo (Retangulo x y) = False

area :: Forma -> Float
area (Circulo r) = pi * r * r
area (Retangulo b a) = b * a

data Arvore = Folha Int | Nodo Int Arvore Arvore
    deriving(Eq,Show)

arv1 :: Arvore 
arv1 = Nodo 10 (Nodo 14 (Nodo 1 ( Folha 4) (Folha 2)) (Folha 6)) (Folha 9)


somaArvore :: Arvore -> Int
somaArvore (Folha n) = n
somaArvore (Nodo n a1 a2) = n + somaArvore a1 + somaArvore a2

multDoisArvore :: Arvore -> Arvore
multDoisArvore (Folha n) = Folha (2*n)
multDoisArvore (Nodo n a1 a2) = Nodo (2*n) (multDoisArvore a1) (multDoisArvore a2)

multArvore :: Int -> Arvore -> Arvore
multArvore x (Folha n) = Folha (n * x)
multArvore x (Nodo n a1 a2) = Nodo (n * x) (multArvore x a1) (multArvore x a2)

contaFolhas :: Arvore -> Int
contaFolhas (Folha _)     = 1  
contaFolhas (Nodo _ a1 a2) = contaFolhas a1 + contaFolhas a2

contaNodo :: Arvore -> Int
contaNodo (Folha _)     = 0 
contaNodo (Nodo _ a1 a2) = 1 + contaNodo a1 + contaNodo a2

quantasVezes :: Int -> Arvore -> Int
quantasVezes x (Folha valor)          = if valor == x then 1 else 0
quantasVezes x (Nodo valor esq dir)
  | x == valor                    = 1 + quantasVezes x esq + quantasVezes x dir
  | otherwise                     = quantasVezes x esq + quantasVezes x dir

maxArvore :: Arvore -> Int
maxArvore (Folha valor)      = valor
maxArvore (Nodo valor esq dir) = max valor (max (maxArvore esq) (maxArvore dir))

refleteArvore :: Arvore -> Arvore
refleteArvore (Folha valor) = Folha valor
refleteArvore (Nodo valor esq dir) = Nodo valor (refleteArvore dir) (refleteArvore esq)

geraLista :: Arvore -> [Int]
geraLista (Folha valor)      = [valor]
geraLista (Nodo valor esq dir) = valor : ((geraLista esq) ++ (geraLista dir))

