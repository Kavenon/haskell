import Data.Char

cezar [] przesuniecie = []
cezar (x:xs) przesuniecie =
                 chr (((((ord x) - 96) + przesuniecie) `mod` 26) + 96) :
                cezar xs przesuniecie


cezardecode [] przesuniecie = []
cezardecode (x:xs) przesuniecie =
                chr (((((ord x) - 96) - przesuniecie) `mod` 26) + 96) :
                cezardecode xs przesuniecie





