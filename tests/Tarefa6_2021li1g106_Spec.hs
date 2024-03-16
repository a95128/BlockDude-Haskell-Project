module Tarefa6_2021li1g106_Spec where

import Test.HUnit
import LI12122
import Tarefa6_2021li1g106
import Fixtures

testsT6 =
  test
    [ "Tarefa 6 - Teste ResolveMapa" ~: resolveJogo 7 m1e1 ~=?  Just [AndarDireita,AndarEsquerda,Trepar,AndarEsquerda,AndarEsquerda,AndarEsquerda,AndarEsquerda]
  
    ]