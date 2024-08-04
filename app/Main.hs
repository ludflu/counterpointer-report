module Main where

import Euterpea
import Scales
import Counterpoint (counterpointReport)

getNotes :: [Int] -> [Music Pitch] -> [Music Pitch]
getNotes sg scale = map (\x -> scale !! x) sg

mkTriad :: Int -> [Int]
mkTriad n = [n, n+2, n+4]

mkSeventh :: Int -> [Int]
mkSeventh n = [n, n+2, n+4, n+6]



--how to compose 1:1 counterpoint
-- https://www.youtube.com/watch?v=b5PoTBOj7Xc

counterpoint :: [Music Pitch]
counterpoint = [
                    g 4 qn,
                    f 4 qn,
                    g 4 qn,
                    a 4 qn,
                    b 4 qn,
                    d 5 qn,
                    c 5 qn,
                    c 5 qn,
                    d 5 qn,
                    e 5 qn,
                    d 5 qn,
                    a 4 qn,
                    c 5 qn,
                    b 4 qn,
                    c 5 qn
               ]


-- by Fancois-Josepf Fetis (1784-1871)
cantusFirmus :: [Music Pitch]
cantusFirmus = [
                    c 4 qn,
                    d 4 qn,
                    e 4 qn,
                    c 4 qn,
                    g 4 qn,
                    f 4 qn,
                    e 4 qn,
                    a 4 qn,
                    g 4 qn,
                    c 4 qn,
                    d 4 qn,
                    f 4 qn,
                    e 4 qn,
                    d 4 qn,
                    c 4 qn
               ]


p1 :: Pitch
p1  = (C,4)
p2 :: Pitch
p2 = (D,4)

-- i = calculateInterval p1 p2
cpt = map getPitch counterpoint
cfp = map getPitch cantusFirmus

rpt = counterpointReport cpt cfp

main :: IO ()
main = do 
          print "cantus firmus"
          print rpt
         --  play $ line cantusFirmus
         --  play $ line counterpoint
         --  play $ line cantusFirmus :=: line counterpoint
