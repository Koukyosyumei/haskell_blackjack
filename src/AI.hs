module AI
  ( choiceStandofAi
  , aiblain
  , aiRatioFunc
  , aiChoiceHit
  ) where

import System.Random.Shuffle
import JudgeBJ
import Cards
import Hands

choiceStandofAi :: (Hand, Deck) -> IO (Hand, Deck)
choiceStandofAi (hand, deck) = do
  return (hand, deck)

aiblain :: (Hand, Deck) -> IO (Hand, Deck)
aiblain (ihand, ideck) = do
  case (judge21 (map cardStrongness (fromHand ihand)), ihand) of
      ((a, b), c) | b > 21 -> choiceStandofAi (ihand, ideck)
      ((a, b), c) | b == 21 -> choiceStandofAi (ihand, ideck)
      ((a, b), c) | b < 21 -> aiChoiceHit (ihand, ideck)


aiRatioFunc :: (Hand, Deck) -> IO (Hand, Deck)
aiRatioFunc (hand, deck) = do
  ratiofordeciding <- shuffleM [0, 1, 2]
  case (take 1 ratiofordeciding) of
      --[1] -> aiChoiceHit (hand, deck)
      [2] -> choiceStandofAi (hand, deck)
      [1] -> aiChoice (hand, deck)
      [0] -> choiceStandofAi (hand, deck)

aiChoice :: (Hand, Deck) -> IO (Hand, Deck)
aiChoice (hand, deck) = do
  case (judge21 (map cardStrongness (fromHand hhand)), hhand) of
      ((a, b), c) | b > 17 -> aiRatioFunc (hhand, hdeck)
      ((a, b), c) | b == 17 -> aiChoice (hhand, hdeck)
      ((a, b), c) | b < 17 -> aiChoice (hhand, hdeck)
      where
         (Just hhand, hdeck) = hit (hand, deck)


aiChoiceHit :: (Hand, Deck) -> IO (Hand, Deck)
aiChoiceHit (hand, deck) = do
  case (judge21 (map cardStrongness (fromHand hhand)), hhand) of
      ((a, b), c) | b > 17 -> aiblain (hhand, hdeck)
      ((a, b), c) | b == 17 -> aiblain (hhand, hdeck)
      ((a, b), c) | b < 17 -> aiblain (hhand, hdeck)
      where
         (Just hhand, hdeck) = hit (hand, deck)
