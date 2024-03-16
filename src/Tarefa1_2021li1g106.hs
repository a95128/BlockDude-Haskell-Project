{-# LANGUAGE AllowAmbiguousTypes #-}

{- |
Module      : Tarefa1_2021li1g106
Description : Validação de um potencial mapa
Copyright   : Diogo Silva <a97941@alunos.uminho.pt>;
            : Ana Alves <a95128@alunos.uminho.pt>;

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.

-}
module Tarefa1_2021li1g106 where
import LI12122

-- inicio do ponto 1

{- | A função ’indicacoordendas’ dada uma lista de peças e coordenadas, transforma uma lista de peças e coordenadas em uma lista de coordenadas.

-}

indicacoordenadas :: [(Peca, Coordenadas)] -> [Coordenadas]
indicacoordenadas [] = []
indicacoordenadas [(p1,(x1,y1))] = [(x1,y1)]
indicacoordenadas ((p1,(x1,y1)) :xs) = (x1,y1) : indicacoordenadas xs

{- | A função 'verifpos' dada uma lista de peças e coordenadas, verifica a existência de peças com as mesmas coordenadas.
se existerem retorna False, mas se não existerem retorna Verdadeiro

-}

verifpos :: [(Peca, Coordenadas)] -> Bool
verifpos [] = True
verifpos [(p1,(x1,y1))] = True
verifpos ((p1,(x1,y1)) :xs) | (x1,y1) `elem` indicacoordenadas xs = False
                            | otherwise = verifpos xs

-- fim do ponto 1

-- inicio do ponto 2

{- | A função 'contaportas' dada uma lista de peças e coordenadas, conta o número de portas existentes numa lista de peças e coordenadas.

-}

contaportas :: [(Peca, Coordenadas)] -> Int
contaportas [] = 0
contaportas [(Porta,(x1,y1))] = 1
contaportas ((p,(x1,y1)):xs) | p == Porta = 1 + contaportas xs
                             | otherwise = contaportas xs

{- | A função 'verifporta' dada uma lista de peças e coordenadas, verifica se existe uma única porta numa lista de peças e coordendas.

-}

verifporta ::  [(Peca, Coordenadas)] -> Bool
verifporta [] = False
verifporta [(Porta,(x1,y1))] = True
verifporta ((p,(x1,y1)):xs) = contaportas ((p,(x1,y1)):xs) == 1

-- fim do ponto 2

-- inicio do ponto 3

{- | A função 'funblocos' dada lista de peças e coordendas, retorna as coordenadas de apenas os blocos.

-}

funblocos :: [(Peca, Coordenadas)] -> [Coordenadas]
funblocos [] = []
funblocos ((p,c):t) | p == Bloco = c:funblocos t
                    | otherwise = funblocos t

{- | A função 'funcaixas' dada lista de peças e coordendas, retorna as coordenadas de apenas as caixas.

-}

funcaixas :: [(Peca, Coordenadas)] -> [Coordenadas]
funcaixas [] = []
funcaixas ((p,c):t) | p == Caixa = c:funcaixas t
                    | otherwise = funcaixas t

{- | A função 'analisacaixa' dada uma lista de coordenadas, verifica se essas coordenadas são de uma caixa ou bloco.

-}

analisacaixa :: [Coordenadas] -> [Coordenadas] -> [Coordenadas]-> Bool
analisacaixa [] _ _ = True
analisacaixa (c:t) caixas blocos = (c `elem` blocos || c `elem` caixas) && analisacaixa t caixas blocos

-- fim do ponto 3

-- inicio do ponto 4

{- | A função 'verifvazio' dada uma lista de peças e coordenadas, compara o comprimento da lista e a sua área. Se o comprimento da lista for menor que a sua área, então retorna verdadeiro.

-}

verifvazio ::  [(Peca, Coordenadas)] -> Bool
verifvazio [] = False
verifvazio a  = length a < area a

{- | A função 'comp' dada uma lista de peças e coordenadas,verifica qual o maior x registado e assume-o como o comprimento do mapa.

-}

comp :: [(Peca, Coordenadas)] -> Int 
comp [] = 0
comp ((_, (x, _)):t) = max x $ comp t

{- | A função 'larg' dada uma lista de peças e coordenadas, verifica qual o maior y registado e assume-o como a largura do mapa.

-}

larg :: [(Peca, Coordenadas)] -> Int 
larg [] = 0
larg ((_, (_, y)):t) = max y $ comp t

{- | A função 'area', dada uma lista de peças e coordenadas e sabendo a largura e o comprimento da mesma, calcula a sua área.

-}


area :: [(Peca, Coordenadas)] -> Int 
area [] = 0
area a = comp a * larg a

-- fim do ponto 4

-- inicio do ponto 5

{- | A função 'iniciobase' dada uma lista de peças e coordenadas devolve as coordenadas do ultimo bloco da primeira coluna.

-}

iniciobase :: [(Peca, Coordenadas)] -> Coordenadas
iniciobase [] = (0, 0)
iniciobase ((Bloco,(0,y)):xs) = (0, max y y2) where (_, y2) = iniciobase xs
iniciobase (x:xs) = iniciobase xs

{- | A função 'descobrey' dada uma lista de coordenadas, verifica se existe algum bloco numa lista de peças e coordendas com as coordenadas dadas.

-}

descobrey :: Coordenadas -> [(Peca, Coordenadas)] -> Bool 
descobrey c l = (Bloco, c) `elem` l

{- | A função 'postbloco' dada as coordenadas e uma lista de peças e coordenadas, verifica se o elemento dessas mesmas coordendas é um bloco.


-}

postbloco :: Coordenadas -> [(Peca, Coordenadas)] -> Bool 
postbloco c [] = False 
postbloco (x1,y1) l | x1 >= ultimacoluna l = True --  esta função tem de parar quando chega à ultima peça da última linha
postbloco (x1,y1) l = descobrey (x1 +1, y1)     l && postbloco (x1 +1, y1)     l
                   || descobrey (x1 +1, y1 -1)  l && postbloco (x1 +1, y1 - 1) l 
                   || descobrey (x1 +1, y1 +1)  l && postbloco (x1 +1, y1 + 1) l
                   || descobrey (x1, y1 + 1)    l && postbloco (x1, y1 + 1)    l 
                   || descobrey (x1, y1 - 1)    l && postbloco (x1, y1 - 1)    l


{- | A função 'ultimacoluna' dada uma lista de peças e coordenadas, verifica o maior x1 e descobre a ultima coluna.

-}

ultimacoluna :: [(Peca, Coordenadas)] -> Int
ultimacoluna [] = 0
ultimacoluna ((p1,(x1,y1)):t) = max x1 (ultimacoluna t)

{- | A função 'validaPotencialMapa' dada uma lista de peças e respectivas coordenadas testa se estas definem
correctamente um mapa.

-}

validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa l = verifpos l && verifporta l && analisacaixa (funcaixas l) (funcaixas l) (funblocos l) && verifvazio l && postbloco (iniciobase l) l
