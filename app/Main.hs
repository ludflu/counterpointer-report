module Main where

import Euterpea
import Scales
-- import Counterpoint
-- import Compose

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
                    g 4 hn,
                    f 4 hn,
                    g 4 hn,
                    a 4 hn,
                    b 4 hn,
                    d 5 hn,
                    c 5 hn,
                    c 5 hn,
                    d 5 hn,
                    e 5 hn,
                    d 5 hn,
                    a 4 hn,
                    c 5 hn,
                    b 4 hn,
                    c 5 hn
               ]


-- by Fancois-Josepf Fetis (1784-1871)
cantusFirmus :: [Music Pitch]
cantusFirmus = [
                    c 4 hn,
                    d 4 hn,
                    e 4 hn,
                    c 4 hn,
                    g 4 hn,
                    f 4 hn,
                    e 4 hn,
                    a 4 hn,
                    g 4 hn,
                    c 4 hn,
                    d 4 hn,
                    f 4 hn,
                    e 4 hn,
                    d 4 hn,
                    c 4 hn
               ]


p1 :: Pitch
p1  = (C,4)
p2 :: Pitch
p2 = (D,4)

i = calculateInterval p1 p2


main :: IO ()
main = do 
          print "cantus firmus"
          play $ line cantusFirmus
          play $ line counterpoint
          play $ line cantusFirmus :=: line counterpoint
