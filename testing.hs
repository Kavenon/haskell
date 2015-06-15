module Testing where

import DataTypes
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
    "pppppppp\n" ++
    "....P...\n" ++
    "........\n" ++
    "........\n" ++
    "........\n" ++
    "PPPP.PPP\n" ++
    "RNBQKBNR"

z = "rnbq.bnr\n" ++
    "pppppppp\n" ++
    "........\n" ++
    "........\n" ++
    "...k....\n" ++
    "..P.....\n" ++
    "PP.PPPPP\n" ++
    "RNBQKBNR"

fin = "...k....\n" ++
      "........\n" ++
      "........\n" ++
      "...p....\n" ++
      "....K...\n" ++
      "........\n" ++
      "........\n" ++
      "........\n"

test = [ACN('a','2','a','3'),ACN('a','7','a','5'),ACN('b','7','b','5')]

{- do testow -}
