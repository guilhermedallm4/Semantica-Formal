data E = Num Int | Soma E E | Mult E E
   deriving(Eq,Show)

prog1 :: E
prog1 = Soma (Num 1) (Mult (Num 2) (Num 3))

prog2 :: E
prog2 = Soma (Mult (Num 2) (Num 3)) 
             (Mult (Num 4) (Num 5))

bigStepE :: E -> Int
bigStepE (Num n) = n
bigStepE (Soma e1 e2) = bigStepE e1 + bigStepE e2
bigStepE (Mult e1 e2) = bigStepE e1 * bigStepE e2

data B = TRUE | FALSE | Not B | And B B | Or B B
   deriving (Eq,Show)


-- Implementar o prog1 como sendo a expressão: TRUE && FALSE || TRUE

-- prog1 :: B
-- prog1 =


-- Implementar o prog2 como sendo a expressão: TRUE && FALSE || FALSE && TRUE 

-- prog2 :: B
-- prog2 =

-- A função bigStepB recebe como entrada uma expressão booleana na nossa linguagem,
-- e devolve um booleano do Haskell (o resultado da avaliação da expressão de entrada):

bigStepB :: B -> Bool
bigStepB TRUE = True
bigStepB FALSE = False
-- As duas regras do Not, estão nestes dois casos:
bigStepB (Not b)
  | bigStepB b == True    = False
  | otherwise            = True
-- Seguindo o que foi feito nas regras do Not, implementar o And e o Or:
-- bigStepB (And b1 b2) 
--   |
--   | 
-- bigStepB (Or b1 b2) 
--   |
--   |
