module Constants where

import DataTypes


changeBierkaToChar Puste = '.'
changeBierkaToChar Krol = 'K'
changeBierkaToChar Hetman = 'Q'
changeBierkaToChar Pionek = 'P'
changeBierkaToChar Skoczek = 'N'
changeBierkaToChar Goniec = 'B'
changeBierkaToChar Wieza = 'R'

changeCharToBierka 'K' = Krol;
changeCharToBierka 'Q' = Hetman;
changeCharToBierka 'P' = Pionek;
changeCharToBierka 'N' = Skoczek;
changeCharToBierka 'B' = Goniec;
changeCharToBierka 'R' = Wieza;
changeCharToBierka '.' = Puste

{- wartosci -}
bierkaValue :: Bierka -> Int
bierkaValue Pionek = 100
bierkaValue Hetman = 1000
bierkaValue Skoczek = 350
bierkaValue Goniec = 350
bierkaValue Wieza = 525
bierkaValue Krol = 10000
bierkaValue Puste = 0
--bierkaValue Pionek = 1
--bierkaValue Hetman = 9
--bierkaValue Skoczek = 3
--bierkaValue Goniec = 3
--bierkaValue Wieza = 5
--bierkaValue Krol = 10000
--bierkaValue Puste = 0

infinity = 10000::Int
threshold = 9000::Int

treeMinmaxDepth = 3::Int
treeABDepth = 5::Int


wejsciowaPlansza = "rnbqkbnr\npppppppp\n........\n........\n........\n........\nPPPPPPPP\nRNBQKBNR"


