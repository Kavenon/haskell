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
import Data.Tree.Game_tree.Game_tree
import Data.Tree.Game_tree.Negascout

import DataTypes
import Testing
import Constants
import Data.Time
import Utils
import Chessboard
import Moves
import Acn

{- domyslna szachownica-}
szachownica = utworzSzachownice wejsciowaPlansza

poczatkowyRuch = ACN('a','1','a','1');

poczatkowyStan = Stan szachownica Czarny poczatkowyRuch 0
poczatkowyRuchBialych = nextMinmaxMove poczatkowyStan




{- stany gry -}
utworzStan :: Kolor -> Szachownica ->Int -> ((Int,Int),(Int,Int)) -> Stan
utworzStan kolor szachownica count ruch@((r,c),(rT,cT)) = Stan (przesunPionekNaPole szachownica (r, c) (rT, cT)) kolor (moveToAcn (r, c) (rT, cT)) count--(wartoscPlanszy ruch kolor)

ruchyDoStanow :: [((Int,Int),(Int,Int))] -> Kolor -> Szachownica -> [Stan]
ruchyDoStanow ruchy kolor szachownica = map (utworzStan kolor szachownica (List.length ruchy)) ruchy

{- generowanie drzewa gry -}
generujDrzewo :: Int -> Stan -> Tree Stan
generujDrzewo 0 poczatkowyStan = Node poczatkowyStan []
generujDrzewo level poczatkowyStan =  Node poczatkowyStan (map (generujDrzewo $level-1) (generujStany poczatkowyStan))


generujStany :: Stan -> [Stan]
generujStany stan = ruchyDoStanow (ruchyZSzachownicy (board stan) kolor) kolor (board stan)
    where kolor = toggleKolor (poprzedniRuch stan)

{- moj minmax -}
minimax :: Tree Stan -> (Int, [Stan])
minimax (Node stan@(Stan b c _ d) []) = (wartoscPlanszy b c d, [stan])
minimax (Node stan@(Stan _ Czarny _ _) xs) = let (v, lst) = maximum (map minimax xs)
    in (v, stan : lst)
minimax (Node stan@(Stan _ Bialy _ _) xs) = let (v, lst) = minimum (map minimax xs)
    in (v, stan : lst)


{- nastepny ruch -}
nextMinmaxMove :: Stan -> Stan
nextMinmaxMove stan@(Stan b c m _)
    | finalStan stan = stan
    | otherwise = nextStan
    where drzewo = generujDrzewo treeMinmaxDepth stan
          mm = minimax drzewo
          nextStan = (snd mm)!!1


{------ AB  Game_tree-----}
instance Game_tree Stan where
    is_terminal = finalStan
    node_value o = wartoscPlanszy (board o) (poprzedniRuch o) (DataTypes.count o)
    children o = generujStany o

lastN :: Int -> [a] -> [a]

lastN n xs = let m = List.length xs in Prelude.drop (m-n) xs


s1 :: Selector [ACN]
s1 = (gets stan, \x -> modify (\vs -> vs {stan = x}))

s2 :: Selector Kolor
s2 = (gets kolor, \x -> modify (\vs -> vs {kolor = x}))

sel :: Selector a -> Game a
sel = fst

{-- game loop --}
play :: String -> Game ()
play inp = do

        {-- pobierz aktualny stan gry --}
        gameState <- sel s1
        kolor <- sel s2

        let localState = lastN 4  gameState
        -- potrzebne do negascout
        --let infiniteLoop = if (List.length localState) == 4 then sameButReverse (localState!!0) (localState!!2) && sameButReverse (localState!!1) (localState!!3) else False


        liftIO $ hPutStrLn stderr $ (show "LAST 4 " ++ show (localState))
        --liftIO $ hPutStrLn stderr $ (show "Infinite loop " ++ show (infiniteLoop))
        liftIO $ hPutStrLn stderr $ (show "Kolor " ++ show (kolor))

        {-- dodaj ruch pobrany od przeciwnika --}
        case parse parseACN "" inp of
              Left err -> if inp == "WIN" then (liftIO $ putStrLn "LOSE">> hFlush stdout >> fail("LOSE")) else (liftIO $ putStrLn "WIN">> hFlush stdout  >> fail("WIN"))
              Right acn ->  (liftIO $ hPutStrLn stderr $ "from opponent = " ++ (show acn)) >> put (Vars (gameState++[acn]) kolor)

        {-- pobierz aktualny stan gry --}
        gameState <-  sel s1


        {-- wybierz odpowiedni algorytm --}
        {-}let nextState = if infiniteLoop == True then nextAcnMoveMinmax szachownica gameState else nextAcnMove szachownica gameState -}
        let nextState =  nextAcnMoveMinmax szachownica gameState
        let nextMove = (move nextState)


        case finalStan nextState of
                True ->  if whoWin nextState == kolor
                            then  liftIO $ putStrLn "WIN">> hFlush stdout >> fail("WIN")
                            else liftIO $ putStrLn "LOSE">> hFlush stdout >> fail("LOSE")
                False -> liftIO $ putStrLn (show $ nextMove)>> hFlush stdout



        {-- dodaj swoj ruch do planszy --}
        put (Vars (gameState++[nextMove]) kolor)

        {-- pobierz aktualny stan gry --}
        gameState <-  sel s1

        {-- plansza na stderr --}
        liftIO $ hPutStrLn stderr ("to opponent = " ++ show nextMove)
        liftIO $ hPutStrLn stderr ("Bialy: " ++ (show $ wartoscPlanszy (board nextState) Bialy (DataTypes.count nextState)) ++ " Czarny: " ++ (show $ wartoscPlanszy (board nextState) Czarny (DataTypes.count nextState)) )

        liftIO $ printCurrentState gameState
        liftIO $ hPutStrLn stderr "-----------------------------------"

doPlay :: Game ()
doPlay = liftIO getContents  >>= (mapM_ play) . lines

main :: IO ()
main = do
  args <- getArgs
  case (listToMaybe args) of
    Just "b" -> goblack
    Just "w" -> putStrLn ( show (move poczatkowyRuchBialych) ) >> hFlush stdout>> go -- białe wykonują pierwszy ruch
    Nothing -> goblack  -- domyślnie grają czarne
    where go = evalStateT doPlay (Vars [( move poczatkowyRuchBialych)] Bialy)
          goblack = evalStateT doPlay (Vars [] Czarny)

nextAcnMove szachownica lst = next
    where states = doAcnMove szachownica lst Bialy
          state = last states
          next = fst(negascout state treeABDepth)!!1


nextAcnMoveMinmax szachownica lst = next
    where states = doAcnMove szachownica lst Bialy
          state = last states
          next = nextMinmaxMove state

doAcnMove :: Szachownica -> [ACN] -> Kolor -> [Stan]
doAcnMove szachownica [] kolor = []
doAcnMove szachownica (ruch@(ACN(a,b,c,d)):xs) kolor = (Stan new kolor ruch (List.length(ruchyZSzachownicy new kolor))) : (doAcnMove  new xs k)
  where new = evalAcnMove szachownica [ruch]
        k = toggleKolor kolor

evalAcnMove :: Szachownica -> [ACN] -> Szachownica
evalAcnMove szachownica [] = szachownica
evalAcnMove szachownica (ACN(a,b,c,d):xs) =  evalAcnMove new xs
  where new = przesunPionekNaPole szachownica (rowFromAcn b, colFromAcn a) (rowFromAcn d, colFromAcn c)

evalMoves :: Szachownica -> [((Int,Int),(Int,Int))] -> [Szachownica]
evalMoves szachownica [] = []
evalMoves szachownica (((r,c),(rT,cT)):xs) = przesunPionekNaPole szachownica (r, c) (rT, cT) : evalMoves szachownica xs


printCurrentState :: [ACN] -> IO ()
printCurrentState h =  do
  hPutStrLn stderr (show $ evalAcnMove szachownica h)

{-- graj sam ze soba --}
game::Bool -> Stan -> Stan
game False stan = stan
game play stan = traceShow (stan) (game p nm)
    where p = not $finalStan stan
          nm = nextMinmaxMove stan
