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

{- przykladowe dane 
c = SzachownicaCol (Pole Krol Bialy )
a = SzachownicaCol (Pole Krol Czarny )
b = SzachownicaRow [c,a]
szach = Szachownica [b]-}

{- wyswietlanie -}
wyswietlCol a = charToString(poleToChar a)

wyswietlRow (SzachownicaRow []) = "\n"
wyswietlRow (SzachownicaRow (a:as)) =  wyswietlCol a ++ (wyswietlRow (SzachownicaRow as))

wyswietlSzachownica (Szachownica []) = "\nkoniec szachownicy \n"
wyswietlSzachownica (Szachownica (a:as)) = wyswietlRow a ++ (wyswietlSzachownica(Szachownica as))

{- wczytywanie -}
wejsciowaPlansza = "rnbqkbnr\npppppppp\n........\n........\n........\n........\nPPPPPPPP\nRNBQKBNR"


przetworzCol a 
	| isUpper a = (Pole (changeCharToBierka(bierka)) Bialy)
	| otherwise = (Pole (changeCharToBierka(bierka)) Czarny)
	where bierka = toUpper a




listaRzedow = lines wejsciowaPlansza;

--
przetworzListeRzedow lst = Szachownica $ map utworzRzad lst
utworzRzad lst = SzachownicaRow $map utworzPole lst
utworzPole a = przetworzCol a
--










instance Show Pole where
                show Empty = "Empty"
                show (Pole bierka kolor) = "Figura: " ++ show (changeBierkaToChar(bierka))
               

{- TOOLS -}
charToString :: Char -> String
charToString = (:[])


