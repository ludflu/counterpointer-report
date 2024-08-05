module Main where

import Euterpea
import Scales
import Counterpoint (counterpointReport, CounterpointReport)




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


report :: CounterpointReport
report = let counterpoint' = map getPitch counterpoint
             cantusFirmus' = map getPitch cantusFirmus
          in counterpointReport counterpoint' cantusFirmus'


main :: IO ()
main = do 
         play $ line cantusFirmus
         play $ line cantusFirmus :=: line counterpoint
         print report

