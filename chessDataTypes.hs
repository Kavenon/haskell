import Data.Char

data Kolor = Bialy | Czarny deriving (Eq)
data Bierka = Krol | Hetman | Pionek | Skoczek | Goniec | Wieza
data Pole = Empty | Pole Bierka Kolor


data Szachownica = Szachownica [[Pole]] -- odradzane, lepiej zrobic osobny podtyp


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





{-
instance Show Bierka where
                show Krol = "Krol"
              
               
instance Show PionGracza where
                show Empty = "Empty"
                show (Pion bierka kolor) = "Figura: " ++ show bierka -}
               

