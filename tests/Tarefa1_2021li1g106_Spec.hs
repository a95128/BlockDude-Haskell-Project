module Tarefa1_2021li1g106_Spec where

import Test.HUnit
import LI12122
import Tarefa1_2021li1g106
import Fixtures

-- Tarefa 1
testsT1 =
  test
    [ "Tarefa 1 - Teste Valida Mapa m1r" ~: validaPotencialMapa m1 ~=? True
    , "Tarefa 1 - Teste Valida Mapa vazio" ~: validaPotencialMapa [] ~=? False
    , "Tarefa 1 - Teste Valida Mapa com 2 portas" ~: validaPotencialMapa [(Porta, (0,0)), (Porta, (1,0))] ~=?  False
    , "Tarefa 1 - Teste Valida Mapa com porta a flutuar" ~: validaPotencialMapa [(Porta, (0,0)), (Bloco, (0,2)), (Bloco, (1,2))] ~=? False
    , "Tarefa 1 - Teste Valida Mapa com caixa a flutuar" ~: validaPotencialMapa [(Porta, (0,0)), (Bloco, (0,1)), (Bloco, (0,2)), (Caixa, (1,0)), (Bloco, (1,2))] ~=? False 
    , "Tarefa 1 - Teste Valida Mapa sem vazios" ~: validaPotencialMapa [(Porta, (0,0)), (Bloco, (0,1)), (Bloco, (1,0)), (Bloco, (1,1))] ~=? False  
    , "Tarefa 1 - Teste Valida Mapa com falhas na base" ~: validaPotencialMapa [(Porta, (0,0)), (Bloco, (0,1)), (Bloco, (0,2)), (Bloco, (1,3))] ~=? False  
    ]
