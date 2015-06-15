module Moves where

import Utils
import DataTypes
import Constants
import Chessboard


{- mozliwe ruchy -}
moves :: Bierka -> [(Int, Int)]
moves Puste = []
moves Krol = [(x, y) | x<-[-1..1], y<- [-1..1], (x, y) /=(0, 0)]
moves Hetman = unique (moves Krol ++ moves Goniec ++ moves Wieza) -- [(x, y) | x<-[-7..7], y<-[-7..7], (x, y) /= (0, 0)] --
moves Wieza = [(x, 0) | x<-[-7..7], x/=0] ++ [(0, y) | y<-[-7..7], y/=0]
moves Goniec = [(x, x) | x<- [-7..7], x /= 0] ++ [(x, -x) | x<- [-7..7], x/=0]
moves Skoczek = [(x, y) | x<-[-2, -1, 1, 2], y<-[-2, -1, 1, 2], (abs y) /= (abs x)]
moves Pionek = [(-1, 1), (1, 0), (1, 1), (-1, 0), (-1, -1), (1, -1)]




{-- sprawdzanie ruchow --}
allMoves :: Bierka -> [(Int, Int)]
allMoves bierka
  | bierka == Krol = moves Krol ++ [(2, 0)] ++ [(0, -2)]
  | bierka == Pionek = moves Pionek ++ [(2, 0), (-2, 0)]
  | otherwise = moves bierka

allMovesForPole :: Pole -> [(Int, Int)]
allMovesForPole Empty = allMoves Puste
allMovesForPole (Pole bierka kolor) = allMoves bierka


sprawdzPozycje :: (Int, Int) -> Bool
sprawdzPozycje (x, y)
  | x>=0 && x<=7 && y>=0 && y<=7 = True
  | otherwise = False

sprawdzRuchPionka :: Szachownica -> (Int,Int) -> (Int,Int) -> Bool
sprawdzRuchPionka szachownica (rowFrom,colFrom) (rowTo, colTo)
    | bierka /= Pionek = True
    | move == (1, 0) && kolorBierki == Czarny && isEmptyField szachownica (rowAfter, colAfter)  = True
    | move == (-1, 0) && kolorBierki == Bialy &&  isEmptyField szachownica (rowAfter, colAfter) = True
    {- bicie na skos -}
    | (move == (1, 1) || move == (1, -1)) &&
        kolorBierki == Czarny &&
        isEmptyField szachownica (rowAfter, colAfter) == False &&
        getKolorBierki (pionRowCol szachownica (rowAfter, colAfter)) == Bialy = True
    | (move == (-1, 1) || move == (-1, -1)) &&
        kolorBierki == Bialy &&
        isEmptyField szachownica (rowAfter, colAfter) == False &&
        getKolorBierki (pionRowCol szachownica (rowAfter, colAfter)) == Czarny = True
    | move == (2, 0) || move == (-2,0) = True
    | otherwise = False
    where move = (rowTo, colTo)
          pole = pionRowCol szachownica (rowFrom,colFrom)
          bierka = getBierkaFromPole pole
          kolorBierki = getKolorBierki pole
          rowAfter = rowFrom + rowTo
          colAfter = colFrom + colTo

sprawdzSpecjalneRuchy :: Szachownica -> (Int,Int) -> (Int,Int) -> Bool
sprawdzSpecjalneRuchy szachownica (rowFrom, colFrom) move
    {- o dwa -}
    | (bierka == Pionek && move == (2,0) && kolorBierki == Czarny && rowFrom == 1 &&  isEmptyField szachownica (rowFrom+1, colFrom) &&isEmptyField szachownica (rowAfter, colAfter) ) = True
    | (bierka == Pionek && move == (-2,0) && kolorBierki == Bialy && rowFrom == 6 &&  isEmptyField szachownica (rowFrom-1, colFrom) && isEmptyField szachownica (rowAfter, colAfter) ) = True
    {- roszada -}
    | (
        move == (0,2)  && colFrom == 4 && getBierkaFromPole (pionRowCol szachownica (rowFrom,4)) == Krol && getBierkaFromPole (pionRowCol szachownica (rowFrom,7)) == Wieza &&
        getKolorBierki (pionRowCol szachownica (rowFrom,4)) == getKolorBierki (pionRowCol szachownica (rowFrom,7)) &&
        isEmptyField szachownica (rowFrom,5) &&
        isEmptyField szachownica (rowFrom,6) &&
        (colFrom == 0 || colFrom == 7)
     ) = True
    | (
        move == (0,-2)  && colFrom == 4 && getBierkaFromPole (pionRowCol szachownica (rowFrom,4)) == Krol && getBierkaFromPole (pionRowCol szachownica (rowFrom,0)) == Wieza &&
        getKolorBierki (pionRowCol szachownica (rowFrom,4)) == getKolorBierki (pionRowCol szachownica (rowFrom,0)) &&
        isEmptyField szachownica (rowFrom,1) &&
        isEmptyField szachownica (rowFrom,2) &&
        isEmptyField szachownica (rowFrom,3) &&
        (colFrom == 0 || colFrom == 7)
     ) = True

    | elem move $moves bierka = True -- podstawowy ruch
    | otherwise = False
    where   pole = pionRowCol szachownica (rowFrom,colFrom)
            bierka = getBierkaFromPole pole
            kolorBierki = getKolorBierki pole
            (rowTo,colTo) = move
            rowAfter = rowFrom + rowTo
            colAfter = colFrom + colTo

sprawdzSamobuje :: Szachownica -> (Int,Int) -> (Int,Int) -> Bool
sprawdzSamobuje szachownica (rowFrom, colFrom) (rowTo, colTo)
    | isEmptyField szachownica (rowFrom+rowTo, colFrom+colTo) = True
    | otherwise = (getKolorBierki (pionRowCol szachownica (rowFrom,colFrom)) /= getKolorBierki (pionRowCol szachownica (rowFrom+rowTo, colFrom+colTo)))

sprawdzPoDrodzeRekurencja :: Szachownica -> (Int,Int) -> (Int,Int) -> Bool
sprawdzPoDrodzeRekurencja szachownica (rowFrom, colFrom) (0, 0) = True
sprawdzPoDrodzeRekurencja szachownica (rowFrom, colFrom) (rowTo, colTo)
    | isEmptyField szachownica (rowFrom+rowTo, colFrom+colTo) = sprawdzPoDrodzeRekurencja szachownica (rowFrom, colFrom) (row,col)
    | otherwise = False
    where row = toZero rowTo
          col = toZero colTo

sprawdzPoDrodze :: Szachownica -> (Int,Int) -> (Int,Int) -> Bool
sprawdzPoDrodze szachownica (rowFrom, colFrom) (rowTo, colTo)
    | bierka == Skoczek = True
    | (wolnaDroga && isEmptyField szachownica (rowFrom+rowTo,colFrom+colTo)) ||
      (wolnaDroga && (isEmptyField szachownica (rowFrom+rowTo, colFrom+colTo)) == False) = True
    | otherwise = False
    where   pole = pionRowCol szachownica (rowFrom,colFrom)
            bierka = getBierkaFromPole pole
            kolorBierki = getKolorBierki pole
            wolnaDroga = sprawdzPoDrodzeRekurencja szachownica (rowFrom, colFrom) (toZero $rowTo, toZero $colTo)


{- nie trzeba sprawdzac dla pustego, bo w allMovesForPole da pusta liste -}
sprawdzRuch :: Szachownica ->  (Int, Int) ->  Kolor -> (Int, Int) -> Bool
sprawdzRuch szachownica (rowFrom, colFrom) kolor (rowTo, colTo)
    | (
    getKolorBierki pole == kolor &&
    sprawdzPozycje(rowAfter,colAfter) &&
    sprawdzRuchPionka szachownica (rowFrom, colFrom) (rowTo,colTo) &&
    sprawdzSpecjalneRuchy szachownica (rowFrom, colFrom) (rowTo, colTo) &&
    sprawdzSamobuje szachownica (rowFrom, colFrom) (rowTo, colTo) &&
    sprawdzPoDrodze szachownica (rowFrom, colFrom) (rowTo, colTo)
    )= True
    | otherwise = False
    where   pole = pionRowCol szachownica (rowFrom,colFrom)
            bierka = getBierkaFromPole pole
            rowAfter = rowFrom + rowTo
            colAfter = colFrom + colTo



listaRuchowZPola :: Szachownica -> (Int, Int) -> Kolor -> [((Int,Int),(Int,Int))]
listaRuchowZPola szachownica (row,col) kolor = map (\(rowTo,colTo) -> ((row,col),(row+rowTo,col+colTo))) moves
    where moves = Prelude.filter (sprawdzRuch szachownica (row,col) kolor)  (allMovesForPole  (pionRowCol szachownica (row,col)))


ruchyZPola :: Szachownica -> (Int, Int) -> Kolor -> [((Int,Int),(Int,Int))]
ruchyZPola szachownica (row,col) kolor =  (listaRuchowZPola szachownica (row,col) kolor)


ruchyZRzedu :: Szachownica ->  (Int, Int) -> Kolor -> [((Int,Int),(Int,Int))]
ruchyZRzedu szachownica (row,8) kolor = []
ruchyZRzedu szachownica (row,col) kolor = ruchyZPola szachownica (row,col) kolor ++ (ruchyZRzedu szachownica (row,col+1) kolor)

ruchyZSzachownicyImpl szachownica (8,0) kolor = []
ruchyZSzachownicyImpl szachownica (row,col) kolor = ruchyZRzedu szachownica (row,0) kolor  ++ (ruchyZSzachownicyImpl szachownica (row+1,0) kolor)

ruchyZSzachownicy :: Szachownica -> Kolor -> [((Int,Int),(Int,Int))]
ruchyZSzachownicy szachownica kolor = ruchyZSzachownicyImpl szachownica (0,0) kolor


toZero :: Int -> Int
toZero a
    | a == 0 = 0
    | otherwise = if a > 0 then a-1 else a+1
