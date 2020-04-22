module Hands
  ( Hand
  , toHand
  , fromHand
  , randomHand
  , deckNumbers
  , getHand
  , hit
  , showHand
  , DiscardList
  , Deck
  , drawHand
  , selectByIndexes
  , toIntList
  , getDiscardList
  ) where


import Cards
import System.Random.Shuffle
import Data.List
import Data.Char

newtype Hand = Hand { fromHand :: [Card] } deriving (Show, Eq, Ord)

toHand :: [Card] -> Maybe Hand
toHand l =
  if length l >= 2
    then Just $ Hand (sort l)
    else Nothing

randomHand :: IO (Maybe Hand)
randomHand = do
  shuffled <- shuffleM allCards
  return . toHand . take 2 $ shuffled

deckNumbers :: Hand -> [Int]
deckNumbers (Hand x) = map cardNumber x

--------------------------------------------------------------------------------

type DiscardList = [Card] -- 捨て札
type Deck = [Card]        -- 山札

getHand :: Deck -> Maybe (Hand, Deck)  -- 手札の取得
getHand deck = do
  hand <- toHand . take 2 $ deck
  return (hand, drop 2 deck)

hit :: (Hand, Deck) -> (Maybe Hand, Deck) -- 手札の追加
hit (hand, x:xs) = (toHand (x:(fromHand hand)), xs)

showHand :: (t, t1) -> t
showHand (hand, deck) = hand

drawHand :: Deck -> DiscardList -> Hand -> Maybe (Hand, Deck)
drawHand deck dis h = let　　　　　　　　-- 手札の入れ替え
  nl = filter (flip notElem dis) (fromHand h)
  nr = drop (5 - length nl) deck
  in do
    hand <- toHand . take 5 $ nl ++ deck
    ndeck <- return nr
    return (hand, ndeck)

atMay :: [a] -> Int -> Maybe a
atMay [] _  = Nothing
atMay xs n = Just (xs !! n)

selectByIndexes :: [a] -> [Int] -> Maybe [a]
selectByIndexes l = sequence . map ((atMay l).(subtract 1))


toIntList :: String -> Maybe [Int]
toIntList str = if and $ map isDigit str then Just $ reads str else Nothing
  where
    reads :: String -> [Int]
    reads = map $ read . (:[])

getDiscardList :: Hand -> IO (Maybe DiscardList)
getDiscardList h = do　　　　 -- 捨て札　選択
    input <- getLine
    return $ do
      intList <- toIntList input
      res <- selectByIndexes (fromHand h) intList
      return res
