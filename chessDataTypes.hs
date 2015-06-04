import Data.Char
import System.Environment
import System.IO  

import Data.Sequence
import qualified Data.Foldable as Foldable

data Kolor = Bialy | Czarny deriving (Eq,Show)
data Bierka = Puste | Krol | Hetman | Pionek | Skoczek | Goniec | Wieza  deriving (Eq,Show)
data Pole = Empty | Pole Bierka Kolor deriving (Eq)

data SzachownicaRow = SzachownicaRow [Pole] deriving (Eq)
data Szachownica = Szachownica [SzachownicaRow]  deriving (Eq)

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


poleToChar (Empty) = '.' 
poleToChar (Pole bierka kolor) 
	| kolor == Bialy = toUpper bierkaChar
	| kolor == Czarny = toLower bierkaChar
	where bierkaChar = changeBierkaToChar bierka



{- wczytywanie -}
wejsciowaPlansza = "rnbqkbnr\npppppppp\n........\n........\n........\n........\nPPPPPPPP\nRNBQKBNR"

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


{- zmiana do listy list -}
{-}
wP a = charToString(poleToChar a)
wR (SzachownicaRow lst) = map wP lst
wS (Szachownica lst) =  map wR lst-}

{- domyslna szachownica-}
szachownica = utworzSzachownice wejsciowaPlansza


instance Show Pole where
                show Empty = "."
                show a = show (poleToChar(a))
               
instance Show Szachownica where
		show a = wyswietlSzachownica a
nthRowFromSzachownica :: Szachownica -> Int -> SzachownicaRow
nthRowFromSzachownica (Szachownica a) nth = a !! nth

{- usuwanie pionka (wstawienie kropki) -}
updateRow :: SzachownicaRow -> Int ->  Pole -> SzachownicaRow
updateRow (SzachownicaRow lst) col pole = SzachownicaRow ( Foldable.toList ( update col pole $ (fromList lst)))

changeElemInRow :: [SzachownicaRow] -> (Int,Int) -> Pole -> [SzachownicaRow]
changeElemInRow (x:xs) (0,col) pole = (updateRow x col pole):xs
changeElemInRow (x:xs) (row,col) pole = x:(changeElemInRow xs (row-1, col) pole)

updateSzachownica :: Szachownica -> (Int,Int) -> Pole -> Szachownica
updateSzachownica (Szachownica rowLst) (row, col) pole = Szachownica $ changeElemInRow rowLst (row,col) pole


usunPionka :: Szachownica -> (Int,Int) -> Szachownica
usunPionka szachownica (row, col) = updateSzachownica szachownica (row,col) Empty

{- TOOLS -}
charToString :: Char -> String
charToString = (:[])

printElements :: [String] -> IO()
printElements = mapM_ putStrLn
