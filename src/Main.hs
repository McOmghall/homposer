module Main where

import Euterpea
import qualified Data.List (elem)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

data NonNormalizedProbability a = NonNormalizedTransition (Double, a, a)
data Probability a = Transition (Double, a, a)
data MarkovMap a = NonNormalizedNode [(a, [NonNormalizedProbability a])] | Node [(a, [Probability a])]


upThird :: Mode -> PitchClass -> PitchClass
upThird Major x = 
  let notes = Map.fromList $ zip [C,Cs,D,Ds,E,F,Fs,G,Gs,A,As,B] [E,F,Fs,G,Gs,A,As,B,C,Cs,D,Ds]
  in Maybe.fromJust $ Map.lookup x notes
upThird Minor x = 
  let notes = Map.fromList $ zip [C,Cs,D,Ds,E,F,Fs,G,Gs,A,As,B] [Ds,E,F,Fs,G,Gs,A,As,B,C,Cs,D]
  in Maybe.fromJust $ Map.lookup x notes

allMajorChords :: [Music Pitch]
allMajorChords = map (uncurry transpose) $ zip [0..] $ map (chord) $ repeat $ map (note sn) $ take 3 $ arpeggiate Major (C, 4)

arpeggiate :: Mode -> Pitch -> [Pitch]
arpeggiate Major (pc, octv) =
  let next = (upThird Major pc, octv)
  in next:arpeggiate Minor next
arpeggiate Minor (pc, octv) =
  let next = (upThird Minor pc, octv)
  in next:arpeggiate Major next

exampleArpeggiation :: [Music Pitch]
exampleArpeggiation = map (note sn) $ (C, 4):arpeggiate Major (C, 4)

diminishedChord :: [Music Pitch]
diminishedChord = map (uncurry transpose) $ zip ([0..]) $ repeat $ chord $ map (note sn) $ standardDimChord
  where standardDimChord = take 4 $ zip (iterate (upThird Minor) C) (repeat 4)

main :: IO ()
main = do
  play . line . concat $ [(take 12 diminishedChord), (take 12 allMajorChords), (take 40 exampleArpeggiation)]