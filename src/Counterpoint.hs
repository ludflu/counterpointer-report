{-# LANGUAGE ScopedTypeVariables #-}

module Counterpoint where

import           Data.List as DL
import qualified Data.Set as Set
import Scales
import Euterpea (Pitch, absPitch)
import Data.Modular (toMod)

data MusicMotion = Contrary | Parallel  | Oblique | Similar
    deriving (Show, Eq)

type Note = Int

data Direction = Ascending | Descending | Stationary
    deriving (Show, Eq)

isConsonantInterval :: Interval -> Bool
isConsonantInterval i = Set.member i consonantIntervals

--The counterpoint must begin and end on a perfect consonance.
beginAndEndPerfectly :: [Pitch] -> [Pitch] -> Bool
beginAndEndPerfectly a b = let intervals = zipWith (calculateInterval') a b
                               absIs = map abs intervals
                               begin =  head absIs --TODO this is partial
                               end =  last absIs --TODO this is partial
                               bperfect = Set.member begin perfection
                               eperfect = Set.member end perfection
                            in bperfect && eperfect

windows :: Int -> [a] -> [[a]]
windows n xs = map (take n) (DL.tails xs)

tuple2 :: [a] -> (a,a)
tuple2 ns = (head ns, (head . tail) ns)

diff :: (Int,Int) -> Int
diff (a,b) = b-a

intToDirection :: Int -> Direction
intToDirection i = if i > 0 then Ascending else Descending

--takes a series of notes, specificed in number of half steps (semitones)
-- returns a series of intervals, also in semitones. negative if pitch drops, positive if it rises
direction :: [Pitch] -> [Int]
direction ns = let absNotes = map absPitch ns
                   prs = windows 2 absNotes
                   noEmptyPrs = filter (\x -> length x == 2) prs
                   directions = map (diff . tuple2) noEmptyPrs
                in directions
               
mostlySteps :: [Pitch] -> Bool
mostlySteps ns = let is = direction ns
                     absInts = map abs is
                     steps = filter (<=2) absInts
                     leaps = filter (>2) absInts
                  in length steps > length leaps

motionType :: Int -> Int -> MusicMotion
motionType a b
 | a == b       = Parallel   --same direction and interval
 | a>0 && b<0   = Contrary   --different direction
 | a<0 && b>0   = Contrary   --different direction
 | a==0 || b==0 = Oblique    --one note repeats, the other moves up or down
 | otherwise    = Similar    --same direction, different interval


motions :: [Pitch] -> [Pitch] -> [(Int, Int)]
motions a b = let adir = direction a
                  bdir = direction b
               in zip adir bdir
 
--takes two series of notes, specificed in semitones
--returns the motion type of each note movement with respect to each other
motionTypes :: [Pitch] -> [Pitch] -> [MusicMotion]
motionTypes a b = let pairs = motions a b
                   in map (uncurry motionType) pairs

--Contrary motion should dominate.
mostlyContrary :: [Pitch] -> [Pitch] -> Bool
mostlyContrary a b = let ms = motionTypes a b
                         contrary = filter (== Contrary) ms
                         notContrary = filter (/= Contrary) ms
                      in length contrary > length notContrary


-- relativeIntervals :: [Pitch] -> [Pitch] -> [Int]
-- relativeIntervals = zipWith (-)

-- absIntervals :: [Pitch] -> [Pitch] -> [Int]
-- absIntervals a b = let interval = relativeIntervals a b
--                        absInt = map abs interval
--                     in absInt


--Perfect consonances must be approached by oblique or contrary motion.
approachPerfection :: [Pitch] -> [Pitch] -> Bool
approachPerfection a b = let intervals = calculateInterval a b
                             ms = motionTypes a b
                             ia = zip ms (tail intervals)
                             onlyPerfect = filter (\(m,i) -> Set.member i perfection) ia
                             onlyPerfectMotions = map fst onlyPerfect
                             contraryOrOblique m = (m == Contrary) || (m == Oblique)
                          in all contraryOrOblique onlyPerfectMotions

--TODO this is not quite right - does this mean two adjacent notes in the same voice
-- or two coinciding notes from different voices?
--The interval of a tenth should not be exceeded between two adjacent parts unless by necessity.
limitInterval :: [Pitch] -> [Pitch] -> Bool
limitInterval a b = let intervals = calculateAbsInterval a b
                        lessThanTenth i = i <= 16 -- a Major Tenth is 16 semitones
                     in all lessThanTenth intervals

--Use no unisons except at the beginning or end.
unisonOnlyBeginOrEnd :: [Pitch] -> [Pitch] -> Bool
unisonOnlyBeginOrEnd a b = let intervals = calculateInterval a b
                               absIntervals = calculateAbsInterval a b
                               modularAndAbsIntervals = zip intervals absIntervals
                               middle = (init . tail) modularAndAbsIntervals
                            in all (\(modInterval,abInterval) -> modInterval>0 || abInterval /= 0) middle


isParallelFifthOrOctave :: (Interval, Interval) -> Bool
isParallelFifthOrOctave (x,y) = x == octave || abs x == perfectFifth && x==y

isParallelFifthOrOctave' :: (Int, Int) -> Bool
isParallelFifthOrOctave' (x',y') = let x = intToMod x'
                                       y = intToMod y'  
                                       in isParallelFifthOrOctave (x,y)
   

--TODO this doesn't seem quite right. Violated by example
--Avoid parallel fifths or octaves between any two parts;
avoidParallelFifthsOrOctaves :: [Pitch] -> [Pitch] -> Bool
avoidParallelFifthsOrOctaves a b = let ms = motions a b
                                       isParallelFifthOrOctave (x,y) = abs x == octave || abs x == perfectFifth && x==y
                                       pfo = filter isParallelFifthOrOctave' ms
                                    in null pfo

--and avoid "hidden" parallel fifths or octaves: that is, movement by similar motion to a perfect fifth or octave, unless one part (sometimes restricted to the higher of the parts) moves by step.


--Avoid moving in parallel fourths.
--(In practice Palestrina and others frequently allowed themselves such progressions, especially if they do not involve the lowest of the parts.)
avoidParallelFourths :: [Pitch] -> [Pitch] -> Bool
avoidParallelFourths a b = let ms = motions a b
                               isParallelFourth (x,y) = intToMod x == perfectFourth && x==y
                               pfo = filter isParallelFourth ms
                            in null pfo


allEqualToFirst :: [Interval] -> Bool
allEqualToFirst ns = let alleq x = x == head ns
                      in all alleq ns

notAllEqual :: [Interval] -> Bool
notAllEqual = not . allEqualToFirst

--Do not use an interval more than three times in a row.
lessThan3RepeatedIntervals :: [Pitch] -> [Pitch] -> Bool
lessThan3RepeatedIntervals a b = let intervals = calculateInterval a b
                                     fours = windows 4 intervals
                                     onlyFours = filter (\t -> length t == 4) fours
                                  in all notAllEqual onlyFours

--Avoid having any two parts move in the same direction by leap
noLeapsInSameDirection :: [Pitch] -> [Pitch] -> Bool
noLeapsInSameDirection a b = let ms = motions a b
                                 similarLeaps (x,y) = (abs x>2 && abs y>2) && ((x>0 && y>0) || (x<0 && y<0))
                                 sls = filter similarLeaps ms
                              in null sls

--Avoid dissonant intervals between any two parts: major or minor second, major or minor seventh, any augmented or diminished interval, and perfect fourth (in many contexts).

noDissonantIntervals :: [Pitch] -> [Pitch] -> Bool
noDissonantIntervals a b = let intervals = Set.fromList $ calculateInterval a b
                               inter = Set.intersection intervals dissonantIntervals
                            in Set.size inter == 0

leapOnlyByConsonantInterval :: [Pitch] -> Bool
leapOnlyByConsonantInterval ns = let ms = direction ns
                                     leaps = filter (\x -> abs x > 2) ms
                                     absLeaps = map intToMod leaps
                                     cleaps = filter isConsonantInterval absLeaps
                                  in length cleaps == length absLeaps

climaxIsConsonantWithScaleOne :: [Int] -> Bool
climaxIsConsonantWithScaleOne ns = undefined

--Attempt to use up to three parallel thirds or sixths in a row.
repeatParallelThirdsorSixths :: [Int] -> [Int] -> Bool
repeatParallelThirdsorSixths a b = undefined

isValidFirstSpeciesCounterpoint :: [Pitch] -> [Pitch] -> Bool
isValidFirstSpeciesCounterpoint a b = noDissonantIntervals a b
    && noLeapsInSameDirection a b
    && avoidParallelFourths a b
    && avoidParallelFifthsOrOctaves a b
    && unisonOnlyBeginOrEnd a b
    && approachPerfection a b
    && beginAndEndPerfectly a b
    && mostlyContrary a b
    && limitInterval a b
    && lessThan3RepeatedIntervals a b
    && mostlySteps a
    && mostlySteps b
    && leapOnlyByConsonantInterval a
    && leapOnlyByConsonantInterval b

data CounterpointReport = CounterpointReport {
    isValidFirstSpeciesR :: Bool,
    noDissonantIntervalsR :: Bool,
    noLeapsInSameDirectionR :: Bool,
    avoidParallelFourthsR :: Bool,
    avoidParallelFifthsOrOctavesR :: Bool,
    unisonOnlyBeginOrEndR :: Bool,
    approachPerfectionR :: Bool,
    beginAndEndPerfectlyR :: Bool,
    mostlyContraryR :: Bool,
    limitIntervalR :: Bool,
    lessThan3RepeatedIntervalsR :: Bool,
    mostlyStepsCantusFirmusR :: Bool,
    mostlyStepsCounterpointR :: Bool,
    onlyConsonantLeapsR :: Bool
} deriving (Show)

counterpointReport :: [Pitch] -> [Pitch] -> CounterpointReport
counterpointReport a b = CounterpointReport {
    isValidFirstSpeciesR = isValidFirstSpeciesCounterpoint a b,
    noDissonantIntervalsR = noDissonantIntervals a b,
    noLeapsInSameDirectionR = noLeapsInSameDirection a b,
    avoidParallelFourthsR = avoidParallelFourths a b,
    avoidParallelFifthsOrOctavesR = avoidParallelFifthsOrOctaves a b,
    unisonOnlyBeginOrEndR = unisonOnlyBeginOrEnd a b,
    approachPerfectionR = approachPerfection a b,
    beginAndEndPerfectlyR = beginAndEndPerfectly a b,
    mostlyContraryR = mostlyContrary a b,
    limitIntervalR = limitInterval a b,
    lessThan3RepeatedIntervalsR = lessThan3RepeatedIntervals a b,
    mostlyStepsCantusFirmusR = mostlySteps a,
    mostlyStepsCounterpointR = mostlySteps b,
    onlyConsonantLeapsR = leapOnlyByConsonantInterval a && leapOnlyByConsonantInterval b
}
