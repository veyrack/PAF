
module MindEngine where

import Data.Foldable

-- utilitaires de séquences
import Data.Sequence (Seq, Seq (..))
import qualified Data.Sequence as Seq

-- utilitaires d'ensembles
import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.Foldable as F

import Debug.Trace



-- Note: 8 colors because it's the standard ANSI colors
data Peg =
  PEmpty
  | Black
  | Blue
  | Green
  | Yellow
  | Cyan
  | White
  | Magenta
  | Red
  deriving (Show, Eq, Ord)

data FeedbackMark =
  MarkedCorrect
  | MarkedPosition
  | Unmarked
  deriving (Show, Eq)

data Secret = Secret { pegs :: (Seq Peg)
                     , size :: Int }
            deriving (Show, Eq)


-- smart constructor for secrets
mkSecret :: Seq Peg -> Secret
mkSecret pegs = Secret pegs (length pegs)
                
type Guess = Seq Peg
type Feedback = Seq (Peg, FeedbackMark)

data Answer = Answer { correct :: Int, position :: Int }
  deriving (Show, Eq)

-- runtime error if not a good guess
safeGuess :: Secret -> Guess -> Guess
safeGuess secret guess =
  if (size secret) /= (length guess)
  then error "Wrong guess size (please report)"
  else guess

wrongGuess :: Secret -> Guess -> Bool
wrongGuess secret guess = (size secret) /= length guess

initFeedback :: Secret -> Feedback
initFeedback (Secret sec _) =
  fmap (\p -> (p, Unmarked)) sec 

markCorrectOne :: Peg -> (Peg, FeedbackMark) -> (Peg, (Peg, FeedbackMark))
markCorrectOne gpeg (speg, mk) | gpeg == speg = (PEmpty, (speg, MarkedCorrect))
                               | otherwise = (gpeg, (speg, mk))

-- fonction à définir (cf. tests)                      
markCorrect :: Guess -> Feedback -> (Guess, Feedback)
markCorrect Empty Empty = (Empty,Empty) 
markCorrect (h:<|t) (h2:<|t2) = 
  let (peg,pfb) = markCorrectOne h h2 in
    let (g,fb) = markCorrect t t2 in
      (peg :<| g,pfb :<| fb)


markPosOne :: Peg -> Feedback -> Feedback
markPosOne _ Empty = Empty
markPosOne p ((peg,fb):<|t)
 | t == Empty = if p == peg && fb == Unmarked
    then Seq.singleton (peg, MarkedPosition)
    else Seq.singleton (peg,fb)
 | p == peg && fb == Unmarked = (p,MarkedPosition) :<| t
 | otherwise = let res = markPosOne p t in
   (peg,fb) :<| res


-- fonction à définir (cf. tests)
markPosition :: Guess -> Feedback -> Feedback
markPosition Empty Empty = Empty
markPosition Empty _ = Empty
markPosition _ Empty = Empty
markPosition (h:<|t) fb
  | t == Empty = markPosOne h fb
  | otherwise = let res = markPosOne h fb in
    markPosition t res



verify :: Secret -> Guess -> Answer
verify secret guess = 
  let (guess', fb) = markCorrect (safeGuess secret guess) (initFeedback secret)
      fb' = markPosition guess' fb
  in foldr verifyAux (Answer 0 0) (fmap snd fb')
  where verifyAux :: FeedbackMark -> Answer -> Answer
        verifyAux MarkedCorrect (Answer cor pos)  = Answer (cor + 1) pos 
        verifyAux MarkedPosition (Answer cor pos)  = Answer cor (pos + 1)
        verifyAux _ ans = ans

winning :: Secret -> Answer -> Bool
winning (Secret _ size) (Answer cor _) = size == cor 
