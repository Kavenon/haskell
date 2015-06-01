import Data.Char
import System.Environment
import System.IO  
import Control.Monad
import Data.List.Split

data Kolor = Bialy | Czarny deriving (Eq)
data Bierka = Puste | Krol | Hetman | Pionek | Skoczek | Goniec | Wieza
data Pole = Empty | Pole Bierka Kolor

--data SzachownicaCol = SzachownicaCol Pole
data SzachownicaRow = SzachownicaRow [Pole]
data Szachownica = Szachownica [SzachownicaRow]

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

instance Show Pole where
                show Empty = "Empty"
                show (Pole bierka kolor) = "Figura: " ++ show (changeBierkaToChar(bierka))
               
instance Show Szachownica where
		show a = wyswietlSzachownica a
{- TOOLS -}
charToString :: Char -> String
charToString = (:[])


