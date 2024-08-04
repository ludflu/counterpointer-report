
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}



module Scales where


import Data.Modular (ℤ, type (/))


import Euterpea (Music, Pitch, wn, Primitive(Note), PitchClass, Music(Prim), shiftPitches, chord, absPitch, pitch, note)

import Euterpea (PitchClass(C, Cs, D, E, F, G, A, B))

import qualified Data.Set as Set

import Data.Modular (ℤ, toMod)

data Mood = Major | Minor deriving Show

data ScaleDegree = Tonic | Supertonic | Mediant | Subdominant | Dominant | Submediant | Leading
 deriving (Eq, Ord, Enum, Show)

data Degree = I | II | III | IV | V | VI | VII
   deriving (Eq, Ord, Enum, Show)

newtype ScaleFamily = ScaleFamily [Int]
newtype Progression = Progression [(Degree,Mood)]

type TwelveTone = ℤ / 12

whole :: Int
whole = 2
half :: Int
half = 1

major= ScaleFamily [whole, whole, half, whole, whole, whole, half, whole]
minor= ScaleFamily [whole, half, whole, whole, half, whole, whole, whole]

moodToPattern :: Mood -> ScaleFamily
moodToPattern Major = major
moodToPattern Minor = minor

majorPentatonic= ScaleFamily [whole, whole, minorThird, whole, majorThird]
minorPentatonic= ScaleFamily [minorThird, whole, whole, minorThird, whole]

ionian =     ScaleFamily [whole, whole, half, whole, whole, whole, half]
dorian =     ScaleFamily [whole, half, whole, whole, whole, half, whole]
phrygian =   ScaleFamily [half, whole, whole, whole, half, whole,whole]
lydian =     ScaleFamily [whole, whole, whole, half, whole, whole, half]
mixolydian = ScaleFamily [whole, whole, half, whole, whole, half, whole]
aeolian =    ScaleFamily [whole, half, whole, whole, half, whole, whole]
locrian =    ScaleFamily [half, whole, whole, half, whole, whole, whole]

mixolydianPentatonic= ScaleFamily [whole, whole, minorThird, minorThird, whole]
phrygianPentatonic= ScaleFamily [half, whole, minorThird, half, minorThird]
diminishedPentatonic= ScaleFamily [whole, half, minorThird, half, minorThird]

unison :: Int
unison = 0

minorSecond :: Int
minorSecond = 1

majorSecond :: Int
majorSecond = 2

minorThird :: Int
minorThird = 3

majorThird :: Int
majorThird = 4

perfectFourth :: Int
perfectFourth = 5

tritone :: Int  
augmentedFourth = 6
diminishedFifth = 6
tritone = 6

perfectFifth :: Int
perfectFifth = 7

minorSixth :: Int
minorSixth = 8

majorSixth :: Int
majorSixth = 9

minorSeventh :: Int
minorSeventh = 10

majorSeventh :: Int
majorSeventh = 11

octave :: Int
octave  = 12

--leaving out the perfectFourth from the list of perfect and consonant Intervals
perfection = Set.fromList [unison, octave, perfectFifth]
dissonantIntervals = Set.fromList [minorSecond, majorSecond, tritone, minorSeventh, majorSeventh]
consonantIntervals = Set.fromList [ unison, minorThird, majorThird, perfectFourth, perfectFifth, minorSixth, majorSixth, octave]

patternToSemitones :: [Int] -> [Int]
patternToSemitones pat = init $ scanl1 (+) (0:pat)

-- given a scale pattern, a root note, and a duration, return the scale of notes
makeScale :: ScaleFamily -> Pitch -> Rational -> [Music Pitch]
makeScale (ScaleFamily ptn) p d = let f ap = note d (pitch (absPitch p + ap))
                                      semis = patternToSemitones ptn
                                   in map f semis


makeMajorScale :: Pitch -> Rational -> [Music Pitch]
makeMajorScale = makeScale major

makeMinorScale :: Pitch -> Rational -> [Music Pitch]
makeMinorScale = makeScale minor

makeDiatonicScale :: Mood -> Pitch -> Rational -> [Music Pitch]
makeDiatonicScale Major = makeMajorScale
makeDiatonicScale Minor = makeMinorScale


cmajor = makeMajorScale (C,4)
dmajor = makeMajorScale (D,4)
emajor = makeMajorScale (E,4)
fmajor = makeMajorScale (F,4)
gmajor = makeMajorScale (G,4)
amajor = makeMajorScale (A,4)
bmajor = makeMajorScale (B,4)

cminor = makeMinorScale (C,4)
dminor = makeMinorScale (D,4)
eminor = makeMinorScale (E,4)
fminor = makeMinorScale (F,4)
gminor = makeMinorScale (G,4)
aminor = makeMinorScale (A,4)
bminor = makeMinorScale (B,4)

cminorPent = makeScale minorPentatonic (C,4)
cmajorPent = makeScale majorPentatonic (C,4)
cSmajorPent = makeScale majorPentatonic (Cs,4)

getPitch :: Music Pitch -> (PitchClass,Int)
getPitch (Prim (Note d p)) = p

-- an index into the scale
getIndexes :: [Int] -> [Int]
getIndexes = map (`mod` 7)

--how many semitones to raise
getOctaveShift :: [Int] -> [Int]
getOctaveShift = map ((* 12) . (`div` 7)) 


calculateInterval :: Pitch -> Pitch -> TwelveTone
calculateInterval p1  p2 = let i  = abs (absPitch p1 - absPitch p2)
                            in toMod $ fromIntegral i


getNotes :: Pitch -> Mood -> Rational -> [Int] -> [Music Pitch]
getNotes key mood duration degrees = let scale = makeDiatonicScale mood key duration
                                         pindices = getIndexes degrees -- an index into the scale
                                         notes = map (scale !!) pindices
                                         shifterAmounts = getOctaveShift degrees --how many semitones to raise
                                         shifters = map shiftPitches shifterAmounts  --an array of pitch shifter functions
                                         shiftedNotes = zipWith ($) shifters notes
                                      in shiftedNotes

mkTriad :: Int -> [Int]
mkTriad n = [n, n+2, n+4]

mkSeventh :: Int -> [Int]
mkSeventh n = [n, n+2, n+4, n+6]

makeMajorChord :: Music Pitch -> Music Pitch
makeMajorChord p = chord [p, shiftPitches (whole+whole) p, shiftPitches (whole+whole+half+whole) p]

makeMinorChord :: Music Pitch -> Music Pitch
makeMinorChord p = chord [p, shiftPitches (whole+half) p, shiftPitches (whole+half+whole+whole) p]

makeChord Major = makeMajorChord
makeChord Minor = makeMinorChord

fromDegree :: Degree -> Int
fromDegree = fromEnum 

makeChordProgression :: [(Degree,Mood)] -> Pitch -> ScaleFamily -> [Music Pitch]
makeChordProgression progression key scaleFamily = let scale = makeScale scaleFamily key wn 
                                                       degrees = map fst progression
                                                       moods = map snd progression
                                                       notes = map (\degree -> scale !! fromDegree degree) degrees
                                                       prog = zip notes moods
                                                    in map (\(d,m) -> makeChord m d) prog



