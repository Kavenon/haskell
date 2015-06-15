module Chessboard where

import Data.Sequence
import qualified Data.Foldable as Foldable
import DataTypes
import Data.Char
import Constants
import Utils

{--- poruszanie ---}
nthRowFromSzachownica :: Szachownica -> Int -> SzachownicaRow
nthRowFromSzachownica (Szachownica a) nth = a !! nth

nthPoleFromRow :: SzachownicaRow -> Int -> Pole
nthPoleFromRow (SzachownicaRow a) nth = a !! nth

pionRowCol :: Szachownica -> (Int, Int) -> Pole
pionRowCol szachownica (row,col) = nthPoleFromRow (nthRowFromSzachownica szachownica row) col


updateSingleRow :: SzachownicaRow -> Int ->  Pole -> SzachownicaRow
updateSingleRow (SzachownicaRow lst) col pole = SzachownicaRow ( Foldable.toList ( update col pole $ (fromList lst)))

updateRows :: [SzachownicaRow] -> (Int,Int) -> Pole -> [SzachownicaRow]
updateRows (x:xs) (0,col) pole = (updateSingleRow x col pole):xs
updateRows (x:xs) (row,col) pole = x:(updateRows xs (row-1, col) pole)

{- umiesc pion na polu -}
umiescPionNaPolu :: Szachownica -> (Int,Int) -> Pole -> Szachownica
umiescPionNaPolu (Szachownica rowLst) (row, col) pole = Szachownica $ updateRows rowLst (row,col) pole

{- usuwanie pionka (wstawienie kropki) -}
usunPionka :: Szachownica -> (Int,Int) -> Szachownica
usunPionka szachownica (row, col) = umiescPionNaPolu szachownica (row,col) Empty

{- postaw kropke na starym miejscu, pionek na nowym -}
przesunPionekNaPole :: Szachownica -> (Int,Int) ->  (Int,Int) -> Szachownica
przesunPionekNaPole szachownica (rowFrom, colFrom) (rowTo, colTo)  = umiescPionNaPolu (usunPionka szachownica (rowFrom, colFrom)) (rowTo,colTo) (nthPoleFromRow (nthRowFromSzachownica szachownica rowFrom) colFrom)

--
--doMove :: Szachownica ->  (Int, Int) -> (Int, Int) -> Szachownica
--doMove szachownica (row,col) (movR, movC) = przesunPionekNaPole szachownica (row,col) (row + movR, col + movC)


finalStan::Stan->Bool
finalStan (Stan b c m d) = sw < threshold || c > -threshold
    where sw = wartoscPlanszy b Bialy d
          c = wartoscPlanszy b Czarny d

{-- kto wygral --}
whoWin :: Stan -> Kolor
whoWin (Stan b c m d)
    | sw < threshold = Czarny
    | c > -threshold = Bialy
    where sw = wartoscPlanszy b Bialy d
          c = wartoscPlanszy b Czarny d


wartoscPlanszy :: Szachownica -> Kolor -> Int -> Int
wartoscPlanszy szachownica kolor movCount =  (((mnoznik kolor) * movCount) `div` 5) + foldl (+) 0 wartosciKolor
    where pozycje = [(x, y) | x<-[0..7], y<-[0..7]]
          pionki = map (pionRowCol szachownica) pozycje
          bierki = map getBierkaFromPole $ Prelude.filter (maKolor kolor) pionki
          wartosci = map bierkaValue bierki
          wartosciKolor = map (mnoznik kolor *) wartosci


maKolor :: Kolor -> Pole -> Bool
maKolor kolor Empty = False
maKolor kolor (Pole bierka kolorb) = kolorb == kolor


toggleKolor kolor
 | kolor == Bialy = Czarny
 | otherwise = Bialy

mnoznik Czarny = -1
mnoznik Bialy = 1


isEmpty :: Pole -> Bool
isEmpty Empty = True
isEmpty (Pole bierka kolor)
    | bierka == Puste = True
    | otherwise = False

isEmptyField szachownica (row,col) = isEmpty pion
    where pion = pionRowCol szachownica (row,col)

getBierkaFromPole :: Pole -> Bierka
getBierkaFromPole Empty = Puste
getBierkaFromPole (Pole bierka kolor) = bierka

getKolorBierki :: Pole -> Kolor
getKolorBierki (Pole bierka kolor) = kolor

poleToChar (Empty) = '.'
poleToChar (Pole bierka kolor)
	| kolor == Bialy = toUpper bierkaChar
	| kolor == Czarny = toLower bierkaChar
	where bierkaChar = changeBierkaToChar bierka

{- tworzenie szachownicy -}
przetworzListeRzedow lst = Szachownica $ map utworzRzad lst
utworzRzad lst = SzachownicaRow $map utworzPole lst
utworzPole a
	| isUpper a = (Pole (changeCharToBierka(bierka)) Bialy)
	| otherwise = (Pole (changeCharToBierka(bierka)) Czarny)
	where bierka = toUpper a

utworzSzachownice plansza = przetworzListeRzedow listaRzedow
			where listaRzedow = lines plansza


{- wyswietlanie -}
wyswietlPole a = charToString(poleToChar a)

wyswietlRow (SzachownicaRow []) = "\n"
wyswietlRow (SzachownicaRow (a:as)) =  wyswietlPole a ++ (wyswietlRow (SzachownicaRow as))

wyswietlSzachownica (Szachownica []) = ""
wyswietlSzachownica (Szachownica (a:as)) = wyswietlRow a ++ (wyswietlSzachownica(Szachownica as))


instance Show Pole where
                show Empty = "."
                show a = show (poleToChar(a))

instance Show Szachownica where
		show a = wyswietlSzachownica a


instance Show Stan where
     show (Stan b c m d) = wyswietlSzachownica b ++ "\nPoprzedni ruch: " ++ show c ++ "\nZrobiony ruch: " ++ show m ++ "\n eval: " ++ (show $ wartoscPlanszy b c d)

instance Ord Stan where
    compare (Stan a c m d) (Stan b c2 m2 d2) = randomCompare (wartoscPlanszy a c d) (wartoscPlanszy b c2 d2)

