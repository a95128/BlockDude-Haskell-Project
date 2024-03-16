{- |
Module      : Tarefa4_2021li1g106
Description : Movimentação do personagem
Copyright   : Diogo Silva <a97941@alunos.uminho.pt>;
            : Ana Alves <a95128@alunos.uminho.pt>;

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2021/22.
-}
module Tarefa4_2021li1g106 where

import LI12122

{-| A função 'movejoagador' recebe um jogo e um movimento e define cada movimento.
-}
moveJogador :: Jogo -> Movimento -> Jogo
moveJogador (Jogo m (Jogador (x,y) d b)) AndarEsquerda = andarEsquerda' (Jogo m (Jogador (x,y) d b))
moveJogador (Jogo m (Jogador (x,y) d b)) AndarDireita = andarDireita' (Jogo m (Jogador (x,y) d b))
moveJogador (Jogo m (Jogador (x,y) d b)) Trepar = trepar' (Jogo m (Jogador (x,y) d b))
moveJogador (Jogo m (Jogador (x,y) d True)) InterageCaixa = largarCaixa (Jogo m (Jogador (x,y) d True))
moveJogador (Jogo m (Jogador (x,y) d False)) InterageCaixa = carregarCaixa (Jogo m (Jogador (x,y) d False))

{-| A função 'correrMovimentos' recebe um jogo e uma lista de movimentos e aplica-os.
-}
correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos = foldl moveJogador

{- | A função 'andarEsquerda' recebe um jogo e um movimento e faz com que o personagem ande para a esquerda.
-}
andarEsquerda' :: Jogo -> Jogo
andarEsquerda' j@(Jogo m (Jogador (x, y) _ b))
    | verificarOOB m (crdLadoC (x, y) Oeste) = Jogo m (Jogador (x, y) Oeste b)
    | b && not (verificarOOB m (x, y - 1)) && esquerda (emCima j) False && esquerda j True= gravidade (Jogo m (Jogador (x - 1, y) Oeste b))
    | esquerda j True = gravidade (Jogo m (Jogador (x - 1, y) Oeste b))
    | otherwise = Jogo m (Jogador (x, y) Oeste b)

{- | A função 'andarDireita'' recebe um jogo e um movimento e faz com que o personagem ande para a direita.
-}
andarDireita' :: Jogo -> Jogo
andarDireita' j@(Jogo m (Jogador (x,y) _ b))
    | verificarOOB m (crdLadoC (x, y) Este) = Jogo m (Jogador (x,y) Este b)
    | b && not (verificarOOB m (x, y - 1)) &&  direita (emCima j) False && direita j True = gravidade (Jogo m (Jogador (x + 1, y) Este b))
    | direita j True = gravidade (Jogo m (Jogador (x + 1, y) Este b))
    | otherwise = Jogo m (Jogador (x,y) Este b)

{- | A função 'esquerda' recebe um jogo e diz se a peca á esquerda do personagem é vazio ou não.
-}
esquerda :: Jogo -> Bool -> Bool
esquerda (Jogo m (Jogador (x,y) d _)) b = coordMapa m (x-1,y) b

{- | A função 'direita' recebe um jogo e diz se a peca á direita do personagem é vazio ou não.
-}
direita :: Jogo -> Bool -> Bool
direita (Jogo m (Jogador (x,y) d _)) b= coordMapa m (x+1,y) b

{- | A função 'emCima' recebe um jogo e dá a peca em cima.
-}
emCima :: Jogo -> Jogo
emCima (Jogo m (Jogador (x, y) d b)) = Jogo m (Jogador (x, y - 1) d b)

{- | A função 'emBaixo' recebe um jogo e dá a peca em baixo.
-}
emBaixo :: Jogo -> Jogo
emBaixo (Jogo m (Jogador (x, y) d b)) = Jogo m (Jogador (x, y + 1) d b)

{- | A função 'emBaixoC' um par de coordenadas e dá a coordenada a baixo.
-}
emBaixoC :: Coordenadas -> Coordenadas
emBaixoC (x, y) = (x, y + 1)

{- | A função 'emCimaC' um par de coordenadas e dá a coordenada em cima.
-}
emCimaC :: Coordenadas -> Coordenadas
emCimaC (x, y) = (x, y - 1)

{- | A função 'coordMapa' recebe um mapa e uma coordenada e indica se a peca na coordenada é vazia ou não.
-}
coordMapa :: Mapa -> Coordenadas -> Bool -> Bool
coordMapa m (x, y) b | b = (((m !! y) !! x) == Vazio) || (((m !! y) !! x) == Porta) 
                     | otherwise = (((m !! y) !! x) == Vazio)
                     
{- | A função 'coordAtualMapa' recebe um jogo e indica True caso seja vazio.
-}
coordAtualMapa :: Jogo -> Bool -> Bool
coordAtualMapa j@(Jogo m (Jogador crd _ _)) b = coordMapa m crd b

{- | A função 'gravidade' recebe um jogo e aplica uma gravidade ao personagem.
-}
gravidade :: Jogo -> Jogo
gravidade j
    | coordAtualMapa mB True = gravidade mB
    | otherwise = j
    where
        mB = emBaixo j

{- | A função 'gravidadeCaixa' recebe um mapa e um par de coordenadas e aplica gravidade à caixa.
-}
gravidadeCaixa :: Mapa -> Coordenadas -> Mapa
gravidadeCaixa m c
    | ((m !! y) !! x) == Vazio = gravidadeCaixa m (x, y)
    | otherwise = adicionarCaixa m c
    where
        (x, y) = emBaixoC c

{-| A função 'crdLado' recebe um jogo e retorna a coordenda á frente do jogador.
-}
crdLado :: Jogo -> Jogo
crdLado (Jogo m (Jogador (x,y) Oeste b)) = Jogo m (Jogador (x - 1,y) Oeste b)
crdLado (Jogo m (Jogador (x,y) Este b)) = Jogo m (Jogador (x + 1,y) Este b)

{-| A função 'crdLadoC' recebe um par de cooordenadas e uma direção e indica as coordenadas da Peça ao lado consoante a direção.
-}
crdLadoC :: Coordenadas -> Direcao -> Coordenadas
crdLadoC (x, y) Oeste = (x - 1, y)
crdLadoC (x, y) Este = (x + 1, y)

{-| A função 'drB' recebe um jogo e indica consoante a direção se a peça á direita ou esquerda é vazio ou não.
-}
drB :: Jogo -> Bool -> Bool
drB j@(Jogo _ (Jogador _ Oeste _)) b = esquerda j b
drB j@(Jogo _ (Jogador _ Este _)) b = direita j b

{- | A função 'trepar' recebe um jogo e faz com que o personagem trepe uma caixa ou bloco nos casos em que o pode fazer.
-}
trepar' :: Jogo -> Jogo
trepar' j@(Jogo m (Jogador (x, y) d b))
    | verificarOOB m (crdLadoC (x, y - 1) d) = j
    | b && not (verificarOOB m (x, y - 2)) && not (drB j False) && coordAtualMapa l True && not (coordAtualMapa (emCima l) False) = l
    | not (drB j True) && coordAtualMapa l True = l
    | otherwise = j
    where
        l = emCima (crdLado j)

{- | A função 'largarCaixa' recebe um jogo e verifica os casos em que personagem pode ou não largar a caixa.
-}
largarCaixa :: Jogo  -> Jogo
largarCaixa j@(Jogo m (Jogador c@(x, y) d _))
    | verificarOOB m (crdLadoC (x, y - 1) d) = j
    | drB j False && coordAtualMapa (emCima (crdLado j)) False = Jogo (gravidadeCaixa m (emCimaC (crdLadoC c d))) (Jogador c d False)
    | coordAtualMapa (emCima (crdLado j)) False = Jogo (adicionarCaixa m (emCimaC (crdLadoC c d))) (Jogador c d False)
    | otherwise = j

{- | A função 'carregarCaixa' recebe um jogo e um movimento e verifica os casos em que personagem pode ou não carregar a caixa.
-}
carregarCaixa :: Jogo -> Jogo
carregarCaixa j@(Jogo m (Jogador c@(x, y) d _))
    | verificarOOB m (crdLadoC (x, y - 1) d) = j
    | eCaixa m (crdLadoC c d) && coordAtualMapa (emCima (crdLado j)) False && coordAtualMapa (emCima j) False = Jogo (retirarCaixa m (crdLadoC c d)) (Jogador c d True)
    | otherwise = j

{-| A função 'eCaixa' recebe um mapa e um par de coordenadas e retorna true se a peça na coordenada for uma caixa.
-}
eCaixa :: Mapa -> Coordenadas -> Bool
eCaixa m (x, y) = ((m !! y) !! x) == Caixa

{-| A função 'verificarOOB' recebe um mapa e um par de coordenadas e verifica os casos em que o personagem sai dos limites do mapa.
-}
verificarOOB :: Mapa -> Coordenadas -> Bool
verificarOOB m (x, y) = x < 0 || y < 0 || x >= c || y >= a
    where
        c = length (head m)
        a = length m

{-| A função 'retirarCaixa' recebe um mapa e um par de coordenadas e substitui a peça na coordenada por vazio.
-}
retirarCaixa :: Mapa -> Coordenadas -> Mapa
retirarCaixa m (x, y) = setAt m y (setAt (m !! y) x Vazio)

{-| A função 'retirarCaixa' recebe um mapa e um par de coordenadas e substitui a peça na coordenada por uma caixa.
-}
adicionarCaixa :: Mapa -> Coordenadas -> Mapa
adicionarCaixa m (x, y) = setAt m y (setAt (m !! y) x Caixa)

{-| A função 'setAt' substitui numa dada coordenada de uma lista o elemtento pelo elemento dado.
-}
setAt :: [a] -> Int -> a -> [a]
setAt xs i x = take i xs ++ [x] ++ drop (i + 1) xs

{-| A função 'fim' retorna True quando o jogador chegar á porta.
-}
fim :: Jogo -> Bool
fim (Jogo m (Jogador (x,y) _ _)) = ((m !! y) !! x) == Porta

-- com ajuda do Rui Gonçalves (a101759 grupo 48)
-- https://hoogle.haskell.org/?hoogle=%5Ba%5D%20-%3E%20Int%20-%3E%20a%20-%3E%20%5Ba%5D
-- https://hackage.haskell.org/package/yjtools-0.9.18/docs/src/Data-List-Tools.html#setAt