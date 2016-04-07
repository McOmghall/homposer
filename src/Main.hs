module Main where

import Euterpea
import qualified Data.List (elem)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

upThird :: Mode -> PitchClass -> PitchClass
upThird Major x = 
  let notes = Map.fromList $ zip [C,Cs,D,Ds,E,F,Fs,G,Gs,A,As,B] [E,F,Fs,G,Gs,A,As,B,C,Cs,D,Ds]
  in Maybe.fromJust $ Map.lookup x notes
upThird Minor x = 
  let notes = Map.fromList $ zip [C,Cs,D,Ds,E,F,Fs,G,Gs,A,As,B] [Ds,E,F,Fs,G,Gs,A,As,B,C,Cs,D]
  in Maybe.fromJust $ Map.lookup x notes

allMajorChords :: [Music Pitch]
allMajorChords = map (uncurry transpose) $ zip [0..12] (repeat ((c 4 qn) :=: (e 4 qn) :=: (g 4 qn)))

arpeggiate :: Mode -> Pitch -> [Pitch]
arpeggiate Major (pc, octv) =
  let next = (upThird Major pc, octv)
  in next:arpeggiate Minor next
arpeggiate Minor (pc, octv) =
  let next = (upThird Minor pc, octv)
  in next:arpeggiate Major next

exampleArpeggiation :: [Music Pitch]
exampleArpeggiation = map (note sn) $ (C, 4):arpeggiate Major (C, 4)

main :: IO ()
main = 
  let arpeggios = take 20 exampleArpeggiation
  in do
  play . line . concat $ [allMajorChords, arpeggios]