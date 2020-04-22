module JudgeBJ
  ( Judge
  , judge21
  , judgeVictory
  , strongest
  ) where

import Control.Monad.State
import Data.List
import Cards
import Hands

data Judge = Bust | AnyScore | BlackJack deriving (Show, Eq, Ord)

judge21 :: [Int] -> (Judge, Int)
judge21 xs
       | sum xs > 21 = (Bust, (sum xs))
       | sum xs == 21 = (BlackJack, (sum xs))
       | otherwise = (AnyScore, (sum xs))

judgeVictory :: (Judge, Int) -> (Judge, Int) -> IO ()
judgeVictory (pjudge, px) (ejudge, ex) = do
  case (pjudge `compare` ejudge) of
    GT -> putStrLn "あなたの勝ちです"
    EQ -> patternEQ (pjudge, px) (ejudge, ex)
    LT -> putStrLn "あなたの負けです"

patternEQ :: (Judge, Int) -> (Judge, Int) -> IO ()
patternEQ (AnyScore, px) (AnyScore, ex) = do
  case (px `compare` ex) of
    GT -> putStrLn "あなたの勝ちです"
    EQ -> putStrLn "引き分けです"
    LT -> putStrLn "あなたの負けです"
patternEQ _ _ = do
  putStrLn "引き分けです"

biggest :: Ord a => [a] -> a
biggest (x:xs) = foldl big x xs
  where
    big :: Ord a => a -> a -> a
    big a b = if (a > b)
                then a
                else b

strongest :: [((Judge, Int), Hand)] -> (Judge, Int)
strongest (x:xs) = foldl getJudge (take1fromt x) (map take1fromt xs)
  where
    take1fromt :: (a, b) -> a
    take1fromt (a, b) = a

    getJudge :: (Judge, Int) -> (Judge, Int) -> (Judge, Int)
    getJudge (aj, ax) (bj, bx) = case (aj `compare` bj) of
                                  GT -> (aj, ax)
                                  EQ -> getJudgeEQ (aj, ax) (bj, bx)
                                  LT -> (bj, bx)

    getJudgeEQ :: (Judge, Int) -> (Judge, Int) -> (Judge, Int)
    getJudgeEQ (AnyScore, ax) (AnyScore, bx) = case (ax `compare` bx) of
                                  GT -> (AnyScore, ax)
                                  EQ -> (AnyScore, ax)
                                  LT -> (AnyScore, bx)
    getJudgeEQ (aj, ax) (bj, bx) = (aj, 0)
