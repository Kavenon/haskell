module Cheesboard where

import DataTypes

poleToChar (Empty) = '.'
poleToChar (Pole bierka kolor)
	| kolor == Bialy = toUpper bierkaChar
	| kolor == Czarny = toLower bierkaChar
	where bierkaChar = changeBierkaToChar bierka

isEmpty :: Pole -> Bool
isEmpty Empty = True
isEmpty (Pole bierka kolor)
    | bierka == Puste = True
    | otherwise = False


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


{- domyslna szachownica-}
szachownica = utworzSzachownice wejsciowaPlansza


instance Show Pole where
                show Empty = "."
                show a = show (poleToChar(a))

instance Show Szachownica where
		show a = wyswietlSzachownica a

nthRowFromSzachownica :: Szachownica -> Int -> SzachownicaRow
nthRowFromSzachownica (Szachownica a) nth = a !! nth

nthPoleFromRow :: SzachownicaRow -> Int -> Pole
nthPoleFromRow (SzachownicaRow a) nth = a !! nth

pionRowCol :: Szachownica -> (Int, Int) -> Pole
pionRowCol szachownica (row,col) = nthPoleFromRow (nthRowFromSzachownica szachownica row) col


isEmptyField szachownica (row,col) = isEmpty pion
    where pion = pionRowCol szachownica (row,col)

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
