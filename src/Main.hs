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

simpleComp :: [Music Pitch]
simpleComp =
  let rithmBass = [wn, wn, wn, wn]
      notesBass = [(C, 3), (F, 3), (B, 3), (G, 3)]
      notesPerBass = 8
      process (dur, (bas, mel)) = chord [note dur bas, transpose 12 $ Euterpea.line $ map (note $ dur/(toRational notesPerBass)) mel]
  in map process $ zip rithmBass [(x, (take notesPerBass . arpeggiate Major) x) | x <- notesBass]


main :: IO ()
main =
  let motif    = c 4 en :+: g 4 en :+: c 5 en :+: g 5 en
      motif2   = motif :+: transpose 3 motif
      motif3   = tempo (1/5) $ transpose (-5) (invert motif)
      motif4   = tempo (1/10) $ transpose (-12) (retro motif)
      miniline = motif :+: motif2 :+: invert motif2 :+: retro motif2
      result = forever miniline :=: forever motif3 :=: forever motif4
  in do play (cut (6 * wn) result)