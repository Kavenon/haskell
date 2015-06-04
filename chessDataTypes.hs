import Data.Char
import System.Environment
import System.IO
import Data.Tree



import Data.Sequence
import qualified Data.Foldable as Foldable

data Kolor = Bialy | Czarny deriving (Eq,Show)
data Bierka = Puste | Krol | Hetman | Pionek | Skoczek | Goniec | Wieza  deriving (Eq,Show)
data Pole = Empty | Pole Bierka Kolor deriving (Eq)

data SzachownicaRow = SzachownicaRow [Pole] deriving (Eq)
data Szachownica = Szachownica [SzachownicaRow]  deriving (Eq)

{- szachownica, ostatni ruch -}
data Stan = Stan {
    board :: Szachownica, kolor :: Kolor
} deriving (Eq,Show)

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

isEmpty :: Pole -> Bool
isEmpty (Pole bierka kolor)
    | bierka == Puste = True
    | otherwise = False

{- wartosci -}
bierkaValue :: Bierka -> Int
bierkaValue Pionek = 100
bierkaValue Hetman = 1000
bierkaValue Skoczek = 350
bierkaValue Goniec = 350
bierkaValue Wieza = 525
bierkaValue Krol = 100000


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
a = przesunPionekNaPole szachownica (6,2) (4,2)
b = przesunPionekNaPole a (1,1) (3,1)


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

{- mozliwe ruchy -}

moves :: Bierka -> [(Int, Int)]
moves Puste = []
moves Krol = [(x, y) | x<-[-1..1], y<- [-1..1], (x, y) /=(0, 0)]
moves Hetman = [(x, y) | x<-[-7..7], y<-[-7..7], (x, y) /= (0, 0)]
moves Wieza = [(x, 0) | x<-[-7..7], x/=0] ++ [(0, y) | y<-[-7..7], y/=0]
moves Goniec = [(x, x) | x<- [-7..7], x /= 0] ++ [(x, -x) | x<- [-7..7], x/=0]
moves Skoczek = [(x, y) | x<-[-2, -1, 1, 2], y<-[-2, -1, 1, 2], (abs y) /= (abs x)]
moves Pionek = [(-1, 1), (1, 0), (1, 1), (-1, 0), (-1, -1), (1, -1)]

getBierkaFromPole (Pole bierka kolor) = bierka

getKolorBierki :: Pole -> Kolor
getKolorBierki (Pole bierka kolor) = kolor
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
    | move == (1, 0) && kolorBierki == Czarny = True
    | move == (-1, 0) && kolorBierki == Bialy = True
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
    | (bierka == Pionek && move == (2,0) && kolorBierki == Czarny && rowFrom == 1 &&  isEmptyField szachownica (rowFrom+1, colFrom) ) = True
    | (bierka == Pionek && move == (-2,0) && kolorBierki == Bialy && rowFrom == 6 &&  isEmptyField szachownica (rowFrom-1, colFrom) ) = True
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

sprawdzSamobuje :: Szachownica -> (Int,Int) -> (Int,Int) -> Bool
sprawdzSamobuje szachownica (rowFrom, colFrom) (rowTo, colTo)
    | isEmptyField szachownica (rowFrom+rowTo, colFrom+colTo) = True
    | otherwise = (getKolorBierki (pionRowCol szachownica (rowFrom,colFrom)) /= getKolorBierki (pionRowCol szachownica (rowFrom+rowTo, colFrom+colTo)))


{- nie trzeba sprawdzac dla pustego, bo w allMovesForPole da pusta liste -}
sprawdzRuch :: Szachownica ->  (Int, Int) -> (Int, Int) -> Bool
sprawdzRuch szachownica (rowFrom, colFrom) (rowTo, colTo)
    | (
    sprawdzPozycje(rowAfter,colAfter) &&
    sprawdzRuchPionka szachownica (rowFrom, colFrom) (rowTo,colTo) &&
    sprawdzSpecjalneRuchy szachownica (rowFrom, colFrom) (rowTo, colTo) &&
    sprawdzSamobuje szachownica (rowFrom, colFrom) (rowTo, colTo)

    )= True
    | otherwise = False
    where   pole = pionRowCol szachownica (rowFrom,colFrom)
            bierka = getBierkaFromPole pole
            rowAfter = rowFrom + rowTo
            colAfter = colFrom + colTo

{-}allValidMovesForPole :: Szachownica -> (Int,Int) -> [(Int, Int)]
allValidMovesForPole szachownica (row,col) = map mozliweRuchy
    where mozliweRuchy = allMoves bierka
          pole = pionRowCol szachownica (row,col)
          bierka = getBierkaFromPole pole-}

doMove :: Szachownica ->  (Int, Int) -> (Int, Int) -> Szachownica
doMove szachownica (row,col) (movR, movC) = przesunPionekNaPole szachownica (row,col) (row + movR, col + movC)

{- /todo: sprawdzac brzegi planszy przy pobieraniu elementu -}
{- zrefaktorowac listaRuchowZPola ruchyZPola -}
listaRuchowZPola szachownica (row,col) = Prelude.filter (sprawdzRuch szachownica (row,col))  (allMovesForPole  (pionRowCol szachownica (row,col)))

ruchyZPola :: Szachownica -> (Int, Int) -> [Szachownica]
ruchyZPola szachownica (row,col) =  map (doMove szachownica (row,col)) $ Prelude.filter (sprawdzRuch szachownica (row,col)) ruchy
    where ruchy = allMovesForPole (pionRowCol szachownica (row,col))



{-
generujMozliwePlansze :: Szachownica -> Kolor -> Szachownica
generujMozliwePlansze szachownica kolor = -}



{- TOOLS -}
charToString :: Char -> String
charToString = (:[])

printElements :: [String] -> IO()
printElements = mapM_ putStrLn
