
-- Definição das árvore sintática para representação dos programas:

data E = Num Int
      |Var String
      |Soma E E
      |Sub E E
      |Mult E E
   deriving(Eq,Show)

data B = TRUE
      | FALSE
      | Not B
      | And B B
      | Or  B B
      | Leq E E
      | Igual E E  -- verifica se duas expressões aritméticas são iguais
   deriving(Eq,Show)

data C = While B C
    | If B C C
    | Seq C C
    | Atrib E E
    | Skip
    | DoWhile C B  -- Do C while B
    | RepeatUntil C B  -- Repete C até que B seja verdadeiro
    | CondAtrib B E E E  --- Atribuição condicional, recebe uma expressão booleana, uma variável e duas expressões aritméticas: B |- x := E1,E2, Se B é verdade então x := E1 senâo x:= E2
    | Swap E E --- recebe duas variáveis e troca o conteúdo delas
    | DAtrrib E E E E -- dupla atribuição
   deriving(Eq,Show)   

-----------------------------------------------------
-----
----- As próximas funções, servem para manipular a memória (sigma)
-----
------------------------------------------------


--- A próxima linha de código diz que o tipo memória é equivalente a uma lista de tuplas, onde o
--- primeiro elemento da tupla é uma String (nome da variável) e o segundo um Inteiro
--- (conteúdo da variável):


type Memoria = [(String,Int)]

exSigma :: Memoria
exSigma = [ ("x", 10), ("temp",0), ("y",0)]


--- A função procuraVar recebe uma memória, o nome de uma variável e retorna o conteúdo
--- dessa variável na memória. Exemplo:
---
--- *Main> procuraVar exSigma "x"
--- 10


procuraVar :: Memoria -> String -> Int
procuraVar [] s = error ("Variavel " ++ s ++ " nao definida no estado")
procuraVar ((s,i):xs) v
  | s == v     = i
  | otherwise  = procuraVar xs v


--- A função mudaVar, recebe uma memória, o nome de uma variável e um novo conteúdo para essa
--- variável e devolve uma nova memória modificada com a varíável contendo o novo conteúdo. A
--- chamada
---
--- *Main> mudaVar exSigma "temp" 20
--- [("x",10),("temp",20),("y",0)]
---
---
--- essa chamada é equivalente a operação exSigma[temp->20]

mudaVar :: Memoria -> String -> Int -> Memoria
mudaVar [] v n = error ("Variavel " ++ v ++ " nao definida no estado")
mudaVar ((s,i):xs) v n
  | s == v     = ((s,n):xs)
  | otherwise  = (s,i): mudaVar xs v n


-------------------------------------
---
--- Completar os casos comentados das seguintes funções:
---
---------------------------------

smallStepE :: (E, Memoria) -> (E, Memoria)
smallStepE (Var x, s)                  = (Num (procuraVar s x), s)
smallStepE (Soma (Num n1) (Num n2), s) = (Num (n1 + n2), s)
smallStepE (Soma (Num n) e, s)         = let (el,sl) = smallStepE (e, s)
                                         in (Soma (Num n) el, sl)
smallStepE (Soma e1 e2,s)              = let (el,sl) = smallStepE (e1, s)
                                         in (Soma el e2,sl)
smallStepE (Mult (Num n1) (Num n2), s) = (Num (n1 * n2), s)
smallStepE (Mult (Num n) e, s)         = let (el,sl) = smallStepE (e, s)
                                         in (Mult (Num n) el, sl)
smallStepE (Mult e1 e2,s)              = let (el,sl) = smallStepE (e1, s)
                                         in (Mult el e2,sl)
smallStepE (Sub (Num n1) (Num n2),s)   = (Num (n1 - n2), s)
smallStepE (Sub (Num n1) e, s)         = let (el, sl) = smallStepE(e, s) 
                                         in (Sub (Num n1) el, sl)
smallStepE (Sub e1 e2, s)              = let (el, sl) = smallStepE (e1, s)
                                         in (Sub el e2, sl)


smallStepB :: (B,Memoria) -> (B, Memoria)
smallStepB (Not TRUE, s) =  (FALSE, s)
smallStepB (Not FALSE, s) = (TRUE, s)
smallStepB (Not b, s) = let (bl, sl) = smallStepB (b, s) in (Not bl, sl)
smallStepB (And FALSE b, s) = (FALSE, s)
smallStepB (And TRUE b, s) = smallStepB(b, s)
smallStepB (And b1 b2,s )  =  let (bl, sl) = smallStepB (b1, s) in (And bl b2, sl)
smallStepB (Or TRUE b, s) = (TRUE, s)
smallStepB (Or FALSE b, s) = smallStepB(b, s)
smallStepB (Or b1 b2,s )  = let(bl, sl) = smallStepB (b1, s) in (Or bl b2, sl)
smallStepB (Leq (Num n1) (Num n2), s)
    |n1 <= n2 = (TRUE, s)
    |otherwise = (FALSE, s)
smallStepB (Leq (Num n) e, s) = let (el, sl) = smallStepE(e, s) in (Leq (Num n) el, sl)
smallStepB (Leq e1 e2, s) = let (el, sl) = smallStepE(e1, s) in (Leq el e2, sl)
smallStepB (Igual (Num n1) (Num n2), s) 
    | n1 == n2 = (TRUE, s)
    | otherwise = (FALSE, s)
smallStepB (Igual (Num n) e, s) = let (el, sl) = smallStepE(e, s) in (Igual (Num n) el, sl)
smallStepB (Igual e1 e2, s) = let (el, sl) = smallStepE(e1, s) in (Igual el e2, sl) -- recebe duas expressões aritméticas e devolve um valor booleano dizendo se são iguais

smallStepC :: (C,Memoria) -> (C,Memoria)
smallStepC (If TRUE c1 c2,s) = (c1, s)
smallStepC (If FALSE c1 c2, s) = (c2 ,s)
smallStepC (If b c1 c2, s) = let (bl, sl) = smallStepB(b, s) in (If bl c1 c2 , s)
smallStepC (Seq Skip c, s) = (c, s)
smallStepC (Seq c1 c2, s) = let (cl, sl) = smallStepC (c1, s) in (Seq cl c2, sl)
smallStepC (Atrib (Var x) (Num n), s) = (Skip, mudaVar s x n)
smallStepC (Atrib (Var x) e, s) = let (el, sl) = smallStepE (e, s) in (Atrib (Var x) el, sl)
smallStepC (While b c, s) = (If b (Seq c (While b c)) Skip, s) 
smallStepC (DoWhile c b, s) = (Seq c (If b (DoWhile c b) Skip), s)
smallStepC (RepeatUntil c b, s) = (Seq c (If b Skip (RepeatUntil c b)), s)  -- RepeatUntil C B  -- Repete C até que B seja verdadeiro
smallStepC (CondAtrib b (Var x) e1 e2, s) = (If b (Atrib (Var x) e1) (Atrib (Var x) e2), s) -- CondAtrib B V E E  --- Atribuição condicional, recebe uma expressão booleana, uma variável e duas expressões aritméticas: B |- x := E1,E2, Se B é verdade então x := E1 senâo x:= E2

smallStepC (Swap (Var x) (Var y), s) = (DAtrrib (Var x) (Var y) (Var y) (Var x), s) -- Swap E E --- recebe duas variáveis e troca o conteúdo delas

smallStepC (DAtrrib (Var x) (Var y) (Num n1) (Num n2), s) = (Seq (Atrib (Var x) (Num n1)) (Atrib (Var y) (Num n2)), s)
smallStepC (DAtrrib (Var x) (Var y) (Num n1) e2, s) = let (el, sl) = smallStepE(e2, s) in (DAtrrib (Var x) (Var y) (Num n1) el, sl)
smallStepC (DAtrrib (Var x) (Var y) e1 e2, s) = let(el, sl) = smallStepE(e1, s) in (DAtrrib (Var x) (Var y) el e2, sl) 

----------------------
--  INTERPRETADORES
----------------------


--- Interpretador para Expressões Aritméticas:
isFinalE :: E -> Bool
isFinalE (Num n) = True
isFinalE _       = False


interpretadorE :: (E,Memoria) -> (E, Memoria)
interpretadorE (e,s) = if (isFinalE e) then (e,s) else interpretadorE (smallStepE (e,s))

--- Interpretador para expressões booleanas


--- Interpretador para Expressões Booleanas:
isFinalB :: B -> Bool
isFinalB TRUE = True
isFinalB FALSE = True
isFinalB _ = False

interpretadorB :: (B, Memoria) -> (B, Memoria)
interpretadorB (b, s) = if (isFinalB b) then (b, s) else interpretadorB (smallStepB (b, s))


-- Interpretador da Linguagem Imperativa

isFinalC :: C -> Bool
isFinalC Skip    = True
isFinalC _       = False


interpretadorC :: (C,Memoria) -> (C, Memoria)
interpretadorC (c,s) = if (isFinalC c) then (c,s) else interpretadorC (smallStepC (c,s))


--------------------------------------
---
--- Exemplos de programas para teste
---
--- O ALUNO DEVE IMPLEMENTAR EXEMPLOS DE PROGRAMAS QUE USEM:
--- * Loop 
--- * Dupla Atribuição
--- * Do While
-------------------------------------

exSigma2 :: Memoria
exSigma2 = [("x",3), ("y",10), ("z",0)]


---
--- O progExp1 é um programa que usa apenas a semântica das expressões aritméticas. Esse
--- programa já é possível rodar com a implementação que fornecida:

progExp1 :: E
progExp1 = Soma (Num 3) (Soma (Var "x") (Var "y"))

---
--- para rodar:
-- A função smallStepE anda apenas um passo na avaliação da Expressão

-- *Main> smallStepE (progExp1, exSigma)
-- (Soma (Num 3) (Soma (Num 10) (Var "y")),[("x",10),("temp",0),("y",0)])

-- Note que no exemplo anterior, o (Var "x") foi substituido por (Num 10)

-- Para avaliar a expressão até o final, deve-se usar o interpretadorE:

-- *Main> interpretadorE (progExp1 , exSigma)
-- (Num 13,[("x",10),("temp",0),("y",0)])

-- *Main> interpretadorE (progExp1 , exSigma2)
-- (Num 6,[("x",3),("y",0),("z",0)])


--- Para rodar os próximos programas é necessário primeiro implementar as regras que faltam
--- e descomentar os respectivos interpretadores


---
-- Exemplos de Programas Imperativos:

teste1 :: B
teste1 = (Leq (Soma (Num 3) (Num 3))  (Mult (Num 2) (Num 3)))


---
-- Exemplos de Programas Imperativos:

testec1 :: C
testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y"))) 
               (Atrib (Var "y") (Var "z")))

--cbigStep (testec1, exSigma2) 

fatorial :: C
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (RepeatUntil 
                       (Seq (Atrib (Var "y") (Mult (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))
                            ((Igual (Var "x") (Num 1)))))

--cbigStep (fatorial, exSigma2)

--exSigma 
programaExemplo :: B
programaExemplo = (Igual (Var "x") (Num 5))


--bbigStep (programaExemplo, exSigma)

-- exSigma2
programaExemplo2 :: C
programaExemplo2 = (Seq 
                    (CondAtrib (Igual (Var "x") (Num 1)) -- X == 1
                      (Var "z")
                      (Soma (Num 10) (Num 5))
                      (Mult (Num 5) (Num 2)))
                    (Atrib (Var "y") (Num 5))
                    )

-- exSigma2
programaExemplo3 :: C
programaExemplo3 = (DAtrrib 
                    (Var "x") 
                    (Var "y") 
                    (Soma (Num 10) (Num 10))
                    (Soma (Num 20) (Num 20)))

-- exSigma2
programSwap :: C
programSwap = (Swap 
                (Var "x")
                (Var "y"))
