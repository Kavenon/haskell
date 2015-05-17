import Data.Char
import System.Environment
import Control.Monad
import Data.Set
import System.IO  
import Control.Monad

data Kolor = Bialy | Czarny deriving (Eq)
data Bierka = Krol | Hetman | Pionek | Skoczek | Goniec | Wieza
data Pole = Empty | Pole Bierka Kolor

data SzachownicaCol = SzachownicaCol Pole
data SzachownicaRow = SzachownicaRow [SzachownicaCol]
data Szachownica = Szachownica [SzachownicaRow]

changeBierkaToChar Krol = 'K'
changeBierkaToChar Hetman = 'Q'
changeBierkaToChar Pionek = 'P'
changeBierkaToChar Skoczek = 'N'
changeBierkaToChar Goniec = 'B'
changeBierkaToChar Wieza = 'R'

poleToChar (Empty) = '.' 
poleToChar (Pole bierka kolor) 
	| kolor == Bialy = toUpper bierkaChar
	| kolor == Czarny = toLower bierkaChar
	where bierkaChar = changeBierkaToChar bierka

c = SzachownicaCol (Pole Krol Bialy )
a = SzachownicaCol (Pole Krol Czarny )
b = SzachownicaRow [c,a]

szach = Szachownica [b]

wyswietlCol (SzachownicaCol a) = charToString(poleToChar a);

wyswietlRow (SzachownicaRow []) = "\n"
wyswietlRow (SzachownicaRow (a:as)) =  wyswietlCol a ++ (wyswietlRow (SzachownicaRow as))

wyswietlSzachownica (Szachownica []) = "\nkoniec szachownicy \n"
wyswietlSzachownica (Szachownica (a:as)) = wyswietlRow a ++ (wyswietlSzachownica(Szachownica as))


{- read from file -}

t = readFile "plansza"

{-instance Show SzachownicaCol where
  show (SzachownicaCol (Pole bierka kolor)) = charToString (poleToChar(Pole bierka kolor) )

instance Show SzachownicaRow where
  show (SzachownicaRow a) = show a-}
              
{-  instance Show PionGracza where
                show Empty = "Empty"
                show (Pion bierka kolor) = "Figura: " ++ show bierka -}
               


charToString :: Char -> String
charToString = (:[])




