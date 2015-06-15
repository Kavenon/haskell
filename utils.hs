module Utils where

import System.IO.Unsafe
import qualified Data.List as List (sort,group,nub,filter,map,length)
import System.Random

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

unique :: Ord a => [a] -> [a]
unique = List.nub