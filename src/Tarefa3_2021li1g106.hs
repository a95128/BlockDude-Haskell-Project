{- |
Module      : Tarefa3_2021li1g106
Description : Representação textual do jogo
Copyright   : Diogo Silva <a97941@alunos.uminho.pt>;
            : Ana Alves <a95128@alunos.uminho.pt>;

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2021/22.
-}
module Tarefa3_2021li1g106 where

import LI12122
import Tarefa4_2021li1g106 ( gravidade )

{- | A instância show Jogo torna o tipo de dados Jogo numa instancia da
class Show.
-}

instance Show Jogo where
  show j@(Jogo l (Jogador (x, y) d b)) = let a = subj (agrups l) (newplayer j )
                                         in init (unlines a)

{- | A função 'convjotopl' dada um Jogo, retorna o estado do jogador.

-}

convjotopl :: Jogo -> Jogador 
convjotopl (Jogo l (Jogador (x,y) d b)) = Jogador (x,y) d b

{- | A função 'newplayer' dada um Jogo, retorna o novo estado do jogador.

-}

newplayer :: Jogo -> Jogador 
newplayer l = convjotopl (gravidade l)

{- | A função 'representp' dada uma lista de peças, transforma uma lista de peças em Strings.

-}

representp :: [Peca] -> [Char]
representp [] = []
representp (x:xs) | x == Bloco = 'X' : representp xs
                  | x == Porta = 'P' : representp xs
                  | x == Caixa = 'C' : representp xs
                  | x == Vazio = ' ' : representp xs
                  | otherwise  = representp xs

{- | A função 'agrups' dado um mapa, transforma um mapa em uma lista de listas de caracteres. 

-}

agrups :: Mapa -> [[Char]]
agrups [] = []
agrups ([]:_)   = []
agrups [(x:xs)] = [representp (x:xs)]
agrups ((x:xs):xss) = representp (x:xs) : agrups xss

l :: Mapa
l = [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco], [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco], [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco], [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]]

{- | A função 'direcaoj' dado um jogador, indica a orientação do jogador.
-}

direcaoj :: Jogador -> String
direcaoj (Jogador (x1, y1) d b) = if d == Oeste then "<" else ">"

{- | A função 'subj' dada uma lista de listas de caracteres, posiciona o jogador no mapa, através da substituição de outra peça.
-}
subj :: [[Char]] -> Jogador -> [[Char]]
subj m (Jogador (x1, y1) d b) = l1 ++ [c1 ++ direcaoj (Jogador (x1, y1) d b) ++ drop 1 c2] ++ tail l2
  where (l1,l2) = splitAt y1 m
        l = head l2
        (c1,c2) = splitAt x1 l

-- splitAt = (take ..., drop ...)