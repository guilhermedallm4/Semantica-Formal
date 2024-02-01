## TRABALHO 1: Completar o seguinte script Haskell com a semântica big-step da linguagem imperativa (+  exemplos de programas) 

## Programas Exemplos

## Memórias
```
type Memoria = [(String,Int)]

exSigma :: Memoria
exSigma = [("x", 10), ("temp", 0), ("y", 0)]

exSigma2 :: Memoria
exSigma2 = [("x",3), ("y",0), ("z",0)]
```

### Programa Exemplo 1
```
--exSigma 
programaExemplo :: B
programaExemplo = (Igual (Var "x") (Num 5))
```
### Comando para executar Programa Exemplo 1
```
ghci .\trabalho_guilhermeDLima.hs

bbigStep (programaExemplo, exSigma)
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
ghci .\trabalho_guilhermeDLima.hs

cbigStep (programaExemplo2, exSigma2)
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


--  cbigStep (programaExemplo3, exSigma2)   


```
### Comando para executar Programa Exemplo 3
```
ghci .\trabalho_guilhermeDLima.hs

cbigStep (programaExemplo3, exSigma2)
```
