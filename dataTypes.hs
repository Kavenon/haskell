module DataTypes where
import Control.Monad.State

data Kolor = Bialy | Czarny deriving (Eq,Show)
data Bierka = Puste | Krol | Hetman | Pionek | Skoczek | Goniec | Wieza  deriving (Eq,Show)
data Pole = Empty | Pole Bierka Kolor deriving (Eq)

data SzachownicaRow = SzachownicaRow [Pole] deriving (Eq)
data Szachownica = Szachownica [SzachownicaRow]  deriving (Eq)

{- szachownica, ostatni ruch -}
data Stan = Stan {
    board :: Szachownica, poprzedniRuch :: Kolor, --, wartosc :: Int
    move :: ACN, count :: Int
} deriving (Eq)


newtype ACN = ACN (Char,Char,Char,Char)

data Vars = Vars {
   stan :: [ACN],
   kolor :: Kolor
}


type Game a = StateT Vars IO a
type Selector a = (Game a, a -> Game ())


instance Show ACN where
  show (ACN (a,b,c,d)) = a:b:c:d:[]


instance Eq ACN where
    ACN (a,b,c,d) == ACN(a2,b2,c2,d2) = a == a2 && b == b2 && c == c2 && d == d2

sameButReverse :: ACN -> ACN -> Bool
sameButReverse (ACN(a,b,c,d))  (ACN(a2,b2,c2,d2))
    | a == c2 && b == d2 && c == a2 && d == b2 = True
    | otherwise = False



