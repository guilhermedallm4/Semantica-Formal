## TRABALHO 2: Completar o seguinte script Haskell com a semântica Small-step da linguagem imperativa (+  exemplos de programas) 

## Memórias
```
type Memoria = [(String,Int)]

exSigma :: Memoria
exSigma = [("x", 10), ("temp", 0), ("y", 0)]

exSigma2 :: Memoria
exSigma2 = [("x",3), ("y",0), ("z",0)]
```
## Programas Exemplos
### Programa Exemplo 1
```
-- exSigma 
programaExemplo :: B
programaExemplo = (Igual (Var "x") (Num 5))
```
### Comando para executar Programa Exemplo 1
```
ghci .\trabalhoSmallStep_guilhermeDLima.hs

interpretadorB (programaExemplo, exSigma)
```
### Programa Exemplo 2
```
-- exSigma2
programaExemplo2 :: C
programaExemplo2 = (Seq 
                    (CondAtrib (Igual (Var "x") (Num 1)) -- X == 1
                      (Var "z")
                      (Soma (Num 10) (Num 5))
                      (Mult (Num 5) (Num 2)))
                    (Atrib (Var "y") (Num 5))
                    )
```

### Comando para executar Programa Exemplo 2
```
ghci .\trabalhoSmallStep_guilhermeDLima.hs

interpretadorC (programaExemplo2, exSigma2)
```
### Programa Exemplo 3

```
-- exSigma2
programaExemplo3 :: C
programaExemplo3 = (Seq 
                    (CondAtrib (Igual (Var "x") (Num 1)) -- X == 1
                      (Var "z")
                      (Soma (Num 10) (Num 5))
                      (Mult (Num 5) (Num 2)))
                    (Swap (Var "z") (Var "x")))


```
### Comando para executar Programa Exemplo 3
```
ghci .\trabalhoSmallStep_guilhermeDLima.hs

interpretadorC (programaExemplo3, exSigma2)
```

### Programa Exemplo 4
```
programaExemplo4 :: C
programaExemplo4 = (RepeatUntil 
                      (Seq 
                        (Atrib (Var "y") (Soma (Var "y") (Num 1))) 
                        (Atrib (Var "x") (Sub (Var "x") (Num 1)))) 
                      (Igual (Var "x") (Num 0)))


```
### Comando para executar Programa Exemplo 4
```
ghci .\trabalhoSmallStep_guilhermeDLima.hs

interpretadorC (programaExemplo4, exSigma2)
```
