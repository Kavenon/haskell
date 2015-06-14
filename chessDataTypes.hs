import Data.Char
import System.Environment
import System.IO
import System.IO.Unsafe
import Data.Tree
import Data.Sequence
import System.Time
import System.Random
import qualified Data.Foldable as Foldable
import Debug.Trace (traceShow)
import qualified Data.List as List (sort,group,nub,filter,map,length)
import Text.ParserCombinators.Parsec
import System.Environment
import Control.Monad.State
import System.Process


import Data.Maybe

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
isEmpty Empty = True
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
bierkaValue Krol = 10000
bierkaValue Puste = 0

infinity = 10000::Int
threshold = 9000::Int

{- wczytywanie -}
wejsciowaPlansza = "rnbqkbnr\npppppppp\n........\n........\n........\n........\nPPPPPPPP\nRNBQKBNR"

{- do testow -}
t = "rnbqkbnr\n" ++
    "p.pppppp\n" ++
    ".P......\n" ++
    "........\n" ++
    "........\n" ++
    "........\n" ++
    "PpPBP.KP\n" ++
    "R....Q.R"
y = "rnbqkbnr\n" ++
    "ppp.pppp\n" ++
    "........\n" ++
    "........\n" ++
    "........\n" ++
    "...p....\n" ++
    "PPPPPPPP\n" ++
    "RNBQKBNR"

z = "rnbq.bnr\n" ++
    "pppppppp\n" ++
    "........\n" ++
    "........\n" ++
    "...k....\n" ++
    "..P.....\n" ++
    "PP.PPPPP\n" ++
    "RNBQKBNR"

sz = utworzSzachownice z
ss = Stan sz Czarny (ACN('0','0','0','0'))
gs = generujDrzewo 3 ss
{- do testow -}

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
moves Hetman = unique (moves Krol ++ moves Goniec ++ moves Wieza) -- [(x, y) | x<-[-7..7], y<-[-7..7], (x, y) /= (0, 0)] --
moves Wieza = [(x, 0) | x<-[-7..7], x/=0] ++ [(0, y) | y<-[-7..7], y/=0]
moves Goniec = [(x, x) | x<- [-7..7], x /= 0] ++ [(x, -x) | x<- [-7..7], x/=0]
moves Skoczek = [(x, y) | x<-[-2, -1, 1, 2], y<-[-2, -1, 1, 2], (abs y) /= (abs x)]
moves Pionek = [(-1, 1), (1, 0), (1, 1), (-1, 0), (-1, -1), (1, -1)]

getBierkaFromPole Empty = Puste
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
    | move == (1, 0) && kolorBierki == Czarny && isEmptyField szachownica (rowAfter, colAfter)  = True
    | move == (-1, 0) && kolorBierki == Bialy &&  isEmptyField szachownica (rowAfter, colAfter) = True
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
    | (bierka == Pionek && move == (2,0) && kolorBierki == Czarny && rowFrom == 1 &&  isEmptyField szachownica (rowFrom+1, colFrom) &&isEmptyField szachownica (rowAfter, colAfter) ) = True
    | (bierka == Pionek && move == (-2,0) && kolorBierki == Bialy && rowFrom == 6 &&  isEmptyField szachownica (rowFrom-1, colFrom) && isEmptyField szachownica (rowAfter, colAfter) ) = True
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
            (rowTo,colTo) = move
            rowAfter = rowFrom + rowTo
            colAfter = colFrom + colTo

sprawdzSamobuje :: Szachownica -> (Int,Int) -> (Int,Int) -> Bool
sprawdzSamobuje szachownica (rowFrom, colFrom) (rowTo, colTo)
    | isEmptyField szachownica (rowFrom+rowTo, colFrom+colTo) = True
    | otherwise = (getKolorBierki (pionRowCol szachownica (rowFrom,colFrom)) /= getKolorBierki (pionRowCol szachownica (rowFrom+rowTo, colFrom+colTo)))

toZero :: Int -> Int
toZero a
    | a == 0 = 0
    | otherwise = if a > 0 then a-1 else a+1

sprawdzPoDrodzeRekurencja :: Szachownica -> (Int,Int) -> (Int,Int) -> Bool
sprawdzPoDrodzeRekurencja szachownica (rowFrom, colFrom) (0, 0) = True
sprawdzPoDrodzeRekurencja szachownica (rowFrom, colFrom) (rowTo, colTo)
    | isEmptyField szachownica (rowFrom+rowTo, colFrom+colTo) = sprawdzPoDrodzeRekurencja szachownica (rowFrom, colFrom) (row,col)
    | otherwise = False
    where row = toZero rowTo
          col = toZero colTo

sprawdzPoDrodze :: Szachownica -> (Int,Int) -> (Int,Int) -> Bool
sprawdzPoDrodze szachownica (rowFrom, colFrom) (rowTo, colTo)
    | bierka == Skoczek = True
    | (wolnaDroga && isEmptyField szachownica (rowFrom+rowTo,colFrom+colTo)) ||
      (wolnaDroga && (isEmptyField szachownica (rowFrom+rowTo, colFrom+colTo)) == False) = True
    | otherwise = False
    where   pole = pionRowCol szachownica (rowFrom,colFrom)
            bierka = getBierkaFromPole pole
            kolorBierki = getKolorBierki pole
            wolnaDroga = sprawdzPoDrodzeRekurencja szachownica (rowFrom, colFrom) (toZero $rowTo, toZero $colTo)

{-
sprawdzBicieKrola :: Szachownica -> (Int,Int) -> (Int,Int) -> Bool
sprawdzBicieKrola szachownica (rowFrom, colFrom) (rowTo, colTo)
  | bierkaDest == Krol = False
  | otherwise = True
  where   poleDest = pionRowCol szachownica (rowFrom+rowTo,colFrom+colTo)
          bierkaDest = getBierkaFromPole poleDest-}

{- nie trzeba sprawdzac dla pustego, bo w allMovesForPole da pusta liste -}
sprawdzRuch :: Szachownica ->  (Int, Int) ->  Kolor -> (Int, Int) -> Bool
sprawdzRuch szachownica (rowFrom, colFrom) kolor (rowTo, colTo)
    | (
    getKolorBierki pole == kolor &&
    sprawdzPozycje(rowAfter,colAfter) &&
    sprawdzRuchPionka szachownica (rowFrom, colFrom) (rowTo,colTo) &&
    sprawdzSpecjalneRuchy szachownica (rowFrom, colFrom) (rowTo, colTo) &&
    sprawdzSamobuje szachownica (rowFrom, colFrom) (rowTo, colTo) &&
    sprawdzPoDrodze szachownica (rowFrom, colFrom) (rowTo, colTo)
    --sprawdzBicieKrola szachownica (rowFrom, colFrom) (rowTo, colTo)
    -- todo: sprawdz mat / pat?

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


listaRuchowZPola :: Szachownica -> (Int, Int) -> Kolor -> [((Int,Int),(Int,Int))]
listaRuchowZPola szachownica (row,col) kolor = map (\(rowTo,colTo) -> ((row,col),(row+rowTo,col+colTo))) moves
    where moves = Prelude.filter (sprawdzRuch szachownica (row,col) kolor)  (allMovesForPole  (pionRowCol szachownica (row,col)))


moveToAcn :: (Int, Int) -> (Int, Int) -> ACN
moveToAcn (row, col) (movR, movC) = ACN (colToAcn col, rowToAcn row, colToAcn ( movC), rowToAcn ( movR ))

colToAcn col = chr (ord 'a' + col)
rowToAcn row = chr (ord '0' + (-row + 8))

colFromAcn col = ( ord col -ord 'a')
rowFromAcn row = -(ord row - ord '0') + 8
{-}

listaRuchowZPolaACN :: Szachownica -> Kolor -> (Int, Int) -> [ACN]
listaRuchowZPolaACN szachownica kolor (row, col)  = map (moveToAcn (row, col)) ruchy
    where ruchy = listaRuchowZPola szachownica (row,col) kolor

listaRuchowZSzachownicyACN :: Szachownica -> Kolor -> [ACN]
listaRuchowZSzachownicyACN szachwnica kolor = foldr (\x xs -> (listaRuchowZPolaACN szachownica kolor x)++xs) [] pola
    where pola = [(x, y) | x<-[0..7], y<-[0..7]]


debugAcnMove :: Szachownica -> [ACN] -> [Szachownica]
debugAcnMove szachownica [] = []
debugAcnMove szachownica (ACN(a,b,c,d):xs) = przesunPionekNaPole szachownica (rowFromAcn b, colFromAcn a) (rowFromAcn d, colFromAcn c) : debugAcnMove szachownica xs
-}


ruchyZPola :: Szachownica -> (Int, Int) -> Kolor -> [((Int,Int),(Int,Int))]
ruchyZPola szachownica (row,col) kolor =  (listaRuchowZPola szachownica (row,col) kolor)


ruchyZRzedu :: Szachownica ->  (Int, Int) -> Kolor -> [((Int,Int),(Int,Int))]
ruchyZRzedu szachownica (row,8) kolor = []
ruchyZRzedu szachownica (row,col) kolor = ruchyZPola szachownica (row,col) kolor ++ (ruchyZRzedu szachownica (row,col+1) kolor)

ruchyZSzachownicyImpl szachownica (8,0) kolor = []
ruchyZSzachownicyImpl szachownica (row,col) kolor = ruchyZRzedu szachownica (row,0) kolor  ++ (ruchyZSzachownicyImpl szachownica (row+1,0) kolor)

ruchyZSzachownicy :: Szachownica -> Kolor -> [((Int,Int),(Int,Int))]
ruchyZSzachownicy szachownica kolor = ruchyZSzachownicyImpl szachownica (0,0) kolor

debugMoves szachownica [] = []
debugMoves szachownica (((r,c),(rT,cT)):xs) = przesunPionekNaPole szachownica (r, c) (rT, cT) : debugMoves szachownica xs

poczatkowyRuch = ACN('0','0','0','0');
poczatkowyStan = Stan szachownica Czarny poczatkowyRuch -- (wartoscPlanszy szachownica Czarny)

drzewoStanow = Node poczatkowyStan []

maKolor :: Kolor -> Pole -> Bool
maKolor kolor Empty = False
maKolor kolor (Pole bierka kolorb) = kolorb == kolor



wartoscPlanszy :: Szachownica -> Kolor -> Int
wartoscPlanszy szachownica kolor = foldl (+) 0 wartosciKolor
    where pozycje = [(x, y) | x<-[0..7], y<-[0..7]]
          pionki = map (pionRowCol szachownica) pozycje
          bierki = map getBierkaFromPole $ Prelude.filter (maKolor kolor) pionki
          wartosci = map bierkaValue bierki
          wartosciKolor = map (mnoznik kolor *) wartosci


{- stany gry -}
utworzStan :: Kolor -> Szachownica -> ((Int,Int),(Int,Int)) -> Stan
utworzStan kolor szachownica ruch@((r,c),(rT,cT)) = Stan (przesunPionekNaPole szachownica (r, c) (rT, cT)) kolor (moveToAcn (r, c) (rT, cT))--(wartoscPlanszy ruch kolor)

ruchyDoStanow :: [((Int,Int),(Int,Int))] -> Kolor -> Szachownica -> [Stan]
ruchyDoStanow ruchy kolor szachownica = map (utworzStan kolor szachownica) ruchy

{- generowanie drzewa gry -}
generujDrzewo :: Int -> Stan -> Tree Stan
generujDrzewo 0 poczatkowyStan = Node poczatkowyStan []
generujDrzewo level poczatkowyStan =  Node poczatkowyStan (map (generujDrzewo $level-1) stanyRuchow)
    where kolor = toggleKolor (poprzedniRuch poczatkowyStan)
          stanyRuchow = ruchyDoStanow (ruchyZSzachownicy (board poczatkowyStan) kolor) kolor (board poczatkowyStan)




minimax :: Tree Stan -> (Int, [Stan])
minimax (Node stan@(Stan b c _) []) = (wartoscPlanszy b c, [stan])
minimax (Node stan@(Stan _ Czarny _) xs) = let (v, lst) = maximum (map minimax xs)
    in (v, stan : lst)
minimax (Node stan@(Stan _ Bialy _) xs) = let (v, lst) = minimum (map minimax xs)
    in (v, stan : lst)


nextMove :: Stan -> Stan
nextMove stan@(Stan b c m)
    | finalStan stan = stan
    | otherwise = nextStan
    where drzewo = generujDrzewo 3 stan
          mm = minimax drzewo
          nextStan = (snd mm)!!1

finalStan::Stan->Bool
finalStan (Stan b c m) = sw < threshold || c > -threshold
    where sw = wartoscPlanszy b Bialy
          c = wartoscPlanszy b Czarny



game::Bool -> Stan -> Stan
game False stan = stan
game play stan = traceShow (stan) (game p nm)
    where p = not $finalStan stan
          nm = nextMove stan


parsePosC :: Parser Char
parsePosC = oneOf "abcdefgh"

parsePosN :: Parser Char
parsePosN = oneOf "12345678"

parseACN :: Parser ACN
parseACN = do
          x1 <- parsePosC
          y1 <- parsePosN
          x2 <- parsePosC
          y2 <- parsePosN
          return $ ACN (x1,y1,x2,y2)


type Game a = StateT ([ACN]) IO a

printHistory :: [ACN] -> IO ()
printHistory h =  do
--  hPutStrLn stderr "Game history"
  hPutStrLn stderr (show $ debugAcnMove szachownica h)



play :: String -> Game ()
play inp = do

        s <- get
        liftIO $ hPutStrLn stderr "Before parse"
        liftIO $ printHistory s

        case parse parseACN "Blad parsowania" inp of
              Left err -> fail("koniec")
              Right acn ->  (liftIO $ hPutStrLn stderr $ "from opponent = " ++ (show acn)) >> put (s++[acn])

        s <- get

        liftIO $ hPutStrLn stderr "After parse"
        liftIO $ printHistory s

        let m = nextAcnMove szachownica s
        let p =  (move m)

        liftIO $ hPutStrLn stderr ("to opponent = " ++ show p)
        liftIO $ hPutStrLn stderr ("Bialy: " ++ (show $ wartoscPlanszy (board m) Bialy) ++ " Czarny: " ++ (show $ wartoscPlanszy (board m) Czarny))

        case finalStan m of
            True -> fail("Game over")
            False -> liftIO $ putStrLn (show $ p)>> hFlush stdout

        --liftIO $ putStrLn (show $ p)>> hFlush stdout
        put (s++[p])

        s <- get


        liftIO $ printHistory s

--        liftIO $ hPutStrLn stderr $ "to opponent = " ++ show p
--        liftIO $ printHistory s

        liftIO $ hPutStrLn stderr "-----------------------------------"




test = [ACN('a','2','a','3'),ACN('a','7','a','5'),ACN('b','7','b','5')]



nextAcnMove szachownica lst = next
    where states = doAcnMove szachownica lst Bialy
          state = last states
          next = nextMove state


doAcnMove :: Szachownica -> [ACN] -> Kolor -> [Stan]
doAcnMove szachownica [] kolor = []
doAcnMove szachownica (ruch@(ACN(a,b,c,d)):xs) kolor = (Stan new kolor ruch) : (doAcnMove  new xs k)
    where new = przesunPionekNaPole szachownica (rowFromAcn b, colFromAcn a) (rowFromAcn d, colFromAcn c)
          k = toggleKolor kolor

debugAcnMove :: Szachownica -> [ACN] -> Szachownica
debugAcnMove szachownica [] = szachownica
debugAcnMove szachownica (ACN(a,b,c,d):xs) =  debugAcnMove new xs
    where new = przesunPionekNaPole szachownica (rowFromAcn b, colFromAcn a) (rowFromAcn d, colFromAcn c)


doPlay :: Game ()
doPlay = liftIO getContents  >>= (mapM_ play) . lines


poczatkowyRuchBialych = nextMove poczatkowyStan

main :: IO ()
main = do
  args <- getArgs
  case (listToMaybe args) of
    Just "b" -> goblack
    Just "w" -> putStrLn ( show (move poczatkowyRuchBialych) ) >> hFlush stdout>> go -- białe wykonują pierwszy ruch
    Nothing -> goblack  -- domyślnie grają czarne
    where go = evalStateT doPlay [( move poczatkowyRuchBialych)]
          goblack = evalStateT doPlay []


instance Show ACN where
  show (ACN (a,b,c,d)) = a:b:c:d:[]

instance Eq ACN where
    ACN (a,b,c,d) == ACN(a2,b2,c2,d2) = a == a2 && b == b2 && c == c2 && d == d2



instance Show Stan where

        show (Stan b c m) = wyswietlSzachownica b ++ "\nPoprzedni ruch: " ++ show c ++ "\nZrobiony ruch: " ++ show m


instance Ord Stan where
    compare (Stan a c m) (Stan b c2 m2) = randomCompare (wartoscPlanszy a c) (wartoscPlanszy b c2)
pick :: [a] ->  a
pick xs = unsafePerformIO(randomRIO (0, Prelude.length xs - 1) >>= return . (xs !!))


randomCompare :: Int -> Int -> Ordering
randomCompare a b
     | a < b    = GT
     | a > b    = LT
     | a==b = pick [LT,GT]
     | otherwise = EQ

{- TOOLS -}
charToString :: Char -> String
charToString = (:[])

toggleKolor kolor
 | kolor == Bialy = Czarny
 | otherwise = Bialy

mnoznik Czarny = -1
mnoznik Bialy = 1

unique :: Ord a => [a] -> [a]
unique = List.nub