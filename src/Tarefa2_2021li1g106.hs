{- |
Module      : Tarefa2_2021li1g106
Description : Construção/Desconstrução do mapa
Copyright   : Diogo Silva <a97941@alunos.uminho.pt>;
            : Ana Alves <a95128@alunos.uminho.pt>;

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2021/22.
-}
module Tarefa2_2021li1g106 where

import LI12122

{- | A função 'constroiMapa' dada uma lista válida de peças e respectivas coordenadas
constroi um mapa (a grelha propriamente dita).

-}

constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa [] = []
constroiMapa pecas = constroiMapa' pecas (criarListaVazia (dimensoesMapa pecas))

{- | A função 'constroiMapa' dada uma lista válida de peças e respectivas coordenadas
controi um mapa vazio e insere as peças nas devidas coordenadas.
-}

constroiMapa' :: [(Peca, Coordenadas)] -> Mapa -> Mapa
constroiMapa' [] m = m
constroiMapa' (h:t) m = constroiMapa' t (insereNoMapa m h)

{- | A função 'dimensoesMapa' dada uma lista válida de peças e respectivas coordenadas
determina as dimenções do mapa.
-}

dimensoesMapa :: [(Peca, Coordenadas)] -> Coordenadas
dimensoesMapa [] = (0, 0)
dimensoesMapa ((_, (x1, y1)):t) = (max x1 x2, max y1 y2)
    where (x2, y2) = dimensoesMapa t

{- | A função 'criarListaVazia' dada uma lista de coordenadas
cria uma lista de listas de peças vazias (Mapa).
-}

criarListaVazia :: Coordenadas -> Mapa
criarListaVazia (x, y) = [lx | _ <- [0..y]]
    where lx = [Vazio | _ <- [0..x]]

{- | A função 'insereNoMapa' dado um mapa e uma peça com as suas coordenadas 
insere a peça na coordenada correta.
-}

insereNoMapa :: Mapa -> (Peca, Coordenadas) -> Mapa
insereNoMapa [] (p, (x, y)) = []
insereNoMapa (h:t) (p, (x, y)) | y == 0 = insereNoMapa' h (p, x):t
                               | otherwise = h:insereNoMapa t (p, (x, y - 1))

{- | A função 'insereNoMapa'' é uma função auxiliar á 'insereNoMapa' que
corre as colunas do mapa.
-}
insereNoMapa' :: [Peca] -> (Peca, Int) -> [Peca]
insereNoMapa' [] (p, x) = []
insereNoMapa' (h:t) (p, x) | x == 0 = p:t
                           | otherwise = h:insereNoMapa' t (p, x - 1)

{- | A função desconstroi Mapa rece um mapa e 
dá uma lista de peças com as suas respetivas coordenadas.
-}

desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa mapa = desconstroiMapa' mapa 0

{- | A função 'desconstroiMapa'' recebe um mapa e um nº inteiro e 
corre as linhas e colunas do mapa dando uma lista de peças com as respetivas coordenadas.
-}
desconstroiMapa' :: Mapa -> Int -> [(Peca, Coordenadas)]
desconstroiMapa' [] _ = []
desconstroiMapa' (h:t) y = desconstroiMapaX h 0 y ++ desconstroiMapa' t (y + 1)

{- | A função 'descontroiMapaX' recebe uma lista de peças, um nº x e um nº y 
que corre as colunas do mapa e dá uma lista de peças com as respetivas coordenadas.
-}
desconstroiMapaX :: [Peca] -> Int -> Int -> [(Peca, Coordenadas)]
desconstroiMapaX [] _ _ = []
desconstroiMapaX (Vazio:t) x y = desconstroiMapaX t (x + 1) y
desconstroiMapaX (h:t) x y = (h, (x, y)):desconstroiMapaX t (x + 1) y