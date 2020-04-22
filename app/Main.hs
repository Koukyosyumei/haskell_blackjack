module Main where
import AI
import JudgeBJ
import Cards
import Hands
import MaBJ
import System.Random.Shuffle

--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "------------------"
  putStrLn "-- simple black jack --"
  putStrLn "------------------"

  deck <- shuffleM allCards
  case getHand deck of
    Nothing -> error "予期せぬエラー"
    Just (hand, deck) -> matchBJtest (hand, deck)

  putStrLn " "

  ynQuestion "Would you continue to play game ?" main (putStrLn "Good Bye !!")

--------------------------------------------------------------------------------
