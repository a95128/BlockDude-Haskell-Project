{- |
Module      : Tarefa5_2021li1g106
Description : Aplicação Gráfica
Copyright   : Diogo Silva <a97941@alunos.uminho.pt>;
            : Ana Alves <a95128@alunos.uminho.pt>;

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.

= Introdução

Foi nesta tarefa desenvolvemos em si, a partir do código desenvolvido nas tarefas anteriores,
a aplicação gráfica do jogo e demos significado às ações que realizamos no nosso teclado (input/output).

= Objetivo

O nosso principal objetivo foi sempre criar um jogo funcional e de fácil manipulação.

= Estratégias Adotadas

O nosso objetivo era essencialmente criar um jogo simples de manipular e destaca-lo exatamente por isso. Relativamente à estética do jogo, 
concordamos que seria algo cúbico e moderno.
A estrutura do menu foi realizada no 'Paint' e convertida numa imagem em formato bmp.O upload dessa mesma imagem foi bastante simples 
e eficaz naquilo que procuravamos.
Relativamente às peças, estas foram criadas de raiz com mecanismos do gloss. O mapa foi renderizado linha a linha, acabando por adotar
a mesma estratégia em tarefas anteriores, principalmente na tarefa 3.
A maior dificuldade que sentimos foi em unir os vários tipos de dados em cadeia, no entanto procuramos a sua solução no 'Hackage' e
partilhando informações com diversos colegas.

= Conclusão

Em suma, esta tarefa foi uma das mais trabalhosas que tivemos, mas consideramos que foi bem sucedida. 
-}

module Main where -- o module tem de ser main para compilar 

import LI12122
import Tarefa4_2021li1g106
import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.Pure.Game
import GHC.Float

-- | A função principal 'main' fundo junta todas as anterior para fazer o jogo acontecer.

main :: IO ()
main =
    do 
    menu <- loadBMP "imagens/menu.bmp"   
    play
         (InWindow
            "Block Dude (Grupo 106)"
            (512,512)
            (0,0))
         orange
         30
         (GE m1e1 True menu)
         renderizar
         lidaTeclas
         (\f w -> w)

-- m1e1 é um jogo e foi colocado para não ser necessário fazer import de Fixtures.

m1e1 :: Jogo
m1e1 = Jogo m1r (Jogador (6, 0) Oeste False)

-- m1r é um mapa e foi colocado para não ser necessário fazer import de Fixtures.

m1r :: Mapa
m1r =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

-- | Event handling

lidaTeclas :: Event -> Estado -> Estado
lidaTeclas (EventKey (SpecialKey KeyLeft) Down _ _) (GE j False menu ) = if fim (moveJogador j AndarEsquerda) then (GE m1e1 True menu) else GE (moveJogador j AndarEsquerda) False menu
lidaTeclas (EventKey (SpecialKey KeyRight) Down _ _) (GE j False menu ) = GE (moveJogador j AndarDireita) False menu
lidaTeclas (EventKey (SpecialKey KeyUp) Down _ _) (GE j False menu ) = GE (moveJogador j Trepar) False menu
lidaTeclas (EventKey (SpecialKey KeyDown) Down _ _) (GE j False menu ) = GE (moveJogador j InterageCaixa) False menu
lidaTeclas (EventKey (SpecialKey KeyEnter) Down _ _) (GE j True menu) = GE  j False menu
lidaTeclas _ f = f

-- | A função 'renderizar' mostra um mapa e jogador.

renderizar :: Estado -> Picture
renderizar (GE (Jogo m (Jogador c d b)) False _ ) = Pictures (renderJogador d b ++ renderizarMapa m c 0)

renderizar (GE (Jogo m (Jogador c d b)) True menu ) = menu -- meter imagem do menu

-- | A função 'renderJogador' mostra o jogador
renderJogador :: Direcao -> Bool -> [Picture]
renderJogador d b = jogadorDirecao d: renderJogadorCaixa b

-- | A função 'jogadorDirecao' para meter o jogador para virar para o lado certo (não é util, porque o nosso boneco é um triângulo).

jogadorDirecao :: Direcao -> Picture
jogadorDirecao Este = jogador
jogadorDirecao Oeste = Scale (-1) 1 jogador

-- | A função 'renderJogadorCaixa' faz com que se for verdade manda a caixa 64 pixeis para cima.

renderJogadorCaixa :: Bool -> [Picture]
renderJogadorCaixa False = []
renderJogadorCaixa True = [Translate 0 64 caixa]

-- | A 'renderizarMapa' mostra um mapa.
renderizarMapa :: Mapa -> Coordenadas -> Int -> [Picture]
renderizarMapa [] _ _ = []
renderizarMapa (h:t) c y = renderizarLinhaMapa h c 0 y ++ renderizarMapa t c (y + 1)

-- | A 'renderizarLinhaMapa' mostrar uma linha do mapa.

renderizarLinhaMapa :: [Peca] -> Coordenadas -> Int -> Int -> [Picture]
renderizarLinhaMapa [] _ _ _= []
renderizarLinhaMapa (Vazio:t) c x y = renderizarLinhaMapa t c (x + 1) y
renderizarLinhaMapa (Caixa:t) c@(xo, yo) x y = Translate (int2Float ((xo - x) * (-64))) (int2Float ((yo - y) * 64)) caixa: renderizarLinhaMapa t c (x + 1) y
renderizarLinhaMapa (Bloco:t) c@(xo, yo) x y = Translate (int2Float ((xo - x) * (-64))) (int2Float ((yo - y) * 64)) bloco: renderizarLinhaMapa t c (x + 1) y
renderizarLinhaMapa (Porta:t) c@(xo, yo) x y = Translate (int2Float ((xo - x) * (-64))) (int2Float ((yo - y) * 64)) porta: renderizarLinhaMapa t c (x + 1) y

-- | Tiles

-- Representação visual da caixa.

caixa :: Picture
caixa = Translate (-30) (-30) $ Polygon [(0, 0), (60, 0), (60, 60), (0, 60)]

-- Representação visual do bloco.

bloco :: Picture
bloco = Translate (-32) (-32) $ Polygon [(0, 0), (64, 0), (64, 64), (0, 64)]

-- Representação visual da porta.

porta :: Picture
porta = Translate (-15) (-32) $ Polygon [(0, 0), (30, 0), (30, 64), (0, 64)]

-- Representação visual do jogador.

jogador :: Picture
jogador = Translate (-32) (-32) $ Polygon [(0, 0), (64, 0), (32, 64)]