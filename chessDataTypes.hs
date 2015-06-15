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
-- todo : 0?????
poczatkowyStan = Stan szachownica Czarny poczatkowyRuch 0 -- (wartoscPlanszy szachownica Czarny)
poczatkowyRuchBialych = nextMinmaxMove poczatkowyStan--pick ( children poczatkowyStan)




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

{-- game loop --}
play :: String -> Game ()
play inp = do

        {-- pobierz aktualny stan gry --}
        gameState <- get

        let localState = lastN 4  gameState
        let infiniteLoop = if (List.length localState) == 4 then sameButReverse (localState!!0) (localState!!2) && sameButReverse (localState!!1) (localState!!3) else False


           -- todo : czarne cos za czesto wygrywaja

        liftIO $ hPutStrLn stderr $ (show "LAST 4 " ++ show (localState))
        liftIO $ hPutStrLn stderr $ (show "Infinite loop " ++ show (infiniteLoop))

        {-- dodaj ruch pobrany od przeciwnika --}
        case parse parseACN "" inp of
              Left err -> fail(inp)
              Right acn ->  (liftIO $ hPutStrLn stderr $ "from opponent = " ++ (show acn)) >> put (gameState++[acn])

        {-- pobierz aktualny stan gry --}
        gameState <- get

        {-- pobierz aktualny stan gry --}
        let nextState = if infiniteLoop == True then nextAcnMoveMinmax szachownica gameState else nextAcnMove szachownica gameState


        let nextMove = (move nextState)


        case finalStan nextState of
                True ->  case whoWin nextState of
                            Bialy -> liftIO $ putStrLn "WHITE WIN">> hFlush stdout >> fail("WHITE WIN")
                            Czarny -> liftIO $ putStrLn "BLACK WIN">> hFlush stdout >> fail("BLACK WIN")
                False -> liftIO $ putStrLn (show $ nextMove)>> hFlush stdout



        {-- dodaj swoj ruch do planszy --}
        put (gameState++[nextMove])

        {-- pobierz aktualny stan gry --}
        gameState <- get

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
    where go = evalStateT doPlay [( move poczatkowyRuchBialych)]
          goblack = evalStateT doPlay []

nextAcnMove szachownica lst = next
    where states = doAcnMove szachownica lst Bialy
          state = last states
          next = fst(negascout state treeABDepth)!!1--nextMinmaxMove state --fst(negascout state 3)!!1--



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