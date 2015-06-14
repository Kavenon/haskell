module DataTypes where

data Kolor = Bialy | Czarny deriving (Eq,Show)
data Bierka = Puste | Krol | Hetman | Pionek | Skoczek | Goniec | Wieza  deriving (Eq,Show)
data Pole = Empty | Pole Bierka Kolor deriving (Eq)

data SzachownicaRow = SzachownicaRow [Pole] deriving (Eq)
data Szachownica = Szachownica [SzachownicaRow]  deriving (Eq)

{- szachownica, ostatni ruch -}
data Stan = Stan {
    board :: Szachownica, poprzedniRuch :: Kolor, --, wartosc :: Int
    move :: ACN
} deriving (Eq)


newtype ACN = ACN (Char,Char,Char,Char)


instance Show ACN where
  show (ACN (a,b,c,d)) = a:b:c:d:[]


instance Eq ACN where
    ACN (a,b,c,d) == ACN(a2,b2,c2,d2) = a == a2 && b == b2 && c == c2 && d == d2