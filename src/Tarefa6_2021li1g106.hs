{- |
Module      : Tarefa6_2021li1g106
Description : Movimentação do personagem
Copyright   : Diogo Silva <a97941@alunos.uminho.pt>;
            : Ana Alves <a95128@alunos.uminho.pt>;

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2021/22.5

= Introdução

Nesta tarefa encontramos uma forma de resolver o jogo num limitado numero de movimentos.

= Objetivo

O objetivo para esta tarefa foi minimizar a possível perda máxima, encontrando o conjunto de movimentos que
dentro do limite chegava mais rapidamente ao objetivo.

= Estratégias Adotadas 
A função 'resolveJogo' foi construida usando (=<<) que passa o valor produzido pelo segundo argumento para o primeiro.
Também usamos as funções 'fromJust' e 'isJust' como auxilio nos resultados do tipo 'Maybe a'.

= Conclusão
Para concluir, nesta tarefa conseguimos cumprir os objetivos propostos implementando a função 'resolveJogo'.
-}

module Tarefa6_2021li1g106 where

import LI12122 ( Jogo, Movimento(..) )
import Data.Maybe ( fromJust, isJust)
import Tarefa4_2021li1g106


-- Com ajuda do Rui Gonçalves (a101759) grupo 48

{- | Tenta resolver o jogo num número maximo de movimentos.
-}
resolveJogo :: Int -> Jogo -> Maybe [Movimento]
resolveJogo i j = otimizarMapa j =<< resolveJogo' i j

{- | Otimiza a lista de movimentos.
-}
otimizarMapa :: Jogo -> [Movimento] -> Maybe [Movimento]
otimizarMapa _ [] = Just []
otimizarMapa _ [a] = Just [a]
otimizarMapa j (h:m:t)
    | h == m && fim (correrMovimentos j (m:t)) = otimizarMapa j (m:t) 
    | otherwise = Just (h: fromJust(otimizarMapa j (m:t) ))

{- | Procura uma lista de movimentos que resolva o jogo dentro 
do limite de movimentos, caso nao consiga retorna 'Nothing'.
-}
resolveJogo':: Int -> Jogo -> Maybe [Movimento]
resolveJogo' n j | n < 0 = Nothing
resolveJogo' n j
    | fim ad = Just [AndarDireita]
    | fim ae = Just [AndarEsquerda]
    | fim tp = Just [Trepar] 
    | fim it = Just [InterageCaixa]
    | isJust adr = Just (AndarDireita : fromJust adr)
    | isJust aer = Just (AndarEsquerda : fromJust aer)
    | isJust tpr = Just (Trepar : fromJust tpr)
    | isJust itr = Just (InterageCaixa : fromJust itr)
    | otherwise = Nothing
    where
        ad = moveJogador j AndarDireita
        ae = moveJogador j AndarEsquerda
        tp = moveJogador j Trepar
        it = moveJogador j InterageCaixa
        adr = resolveJogo' (n-1) ad
        aer = resolveJogo' (n-1) ae
        tpr = resolveJogo' (n-1) tp
        itr = resolveJogo' (n-1) it 
