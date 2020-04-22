module MaBJ
  ( ynQuestion
  , choiceHit
  , choiceHit'
  , choiceStand
  , choiceStand'
  , returnResult
  , hitorStand
  , hitorStand'
  , Player
  , matchBJ
  , matchBJtest
  , playBJ
  ) where

  import AI
  import Cards
  import Hands
  import JudgeBJ

  ynQuestion :: String -> IO a -> IO a -> IO a
  ynQuestion s yes no = do
    putStrLn $ s ++ "(y/n)"
    input <- getLine
    case input of
      "y" -> yes
      "n" -> no
      _ -> do
        putStrLn "-- `y`か`n`で入力してね"
        ynQuestion s yes no

---------------------------------------------------------------------

  choiceHit :: (Hand, Deck) -> IO ()
  choiceHit (shand, sdeck) = do
    print hhand
    case (judge21 (map cardStrongness (fromHand hhand)), hhand) of
        ((a, b), c) | b > 21 -> choiceStand hhand
        ((a, b), c) | b == 21 -> choiceStand hhand
        ((a, b), c) | b < 21 -> ynQuestion "would you hit more ? " (choiceHit (hhand, hdeck)) (choiceStand hhand)
        where
          (Just hhand, hdeck) = hit (shand, sdeck)

  choiceHit' :: (Hand, Deck) -> IO (Hand, Deck)
  choiceHit' (shand, sdeck) = do
    print hhand
    case (judge21 (map cardStrongness (fromHand hhand)), hhand) of
        ((a, b), c) | b > 21 -> choiceStand' (hhand, hdeck)
        ((a, b), c) | b == 21 -> choiceStand' (hhand, hdeck)
        ((a, b), c) | b < 21 -> ynQuestion "would you hit more ? " (choiceHit' (hhand, hdeck)) (choiceStand' (hhand, hdeck))
        where
          (Just hhand, hdeck) = hit (shand, sdeck)

------------------------------------------------------------------------

  choiceStand :: Hand -> IO ()
  choiceStand hand = do
    putStrLn "あなたのハンドは"
    print $ (judge21 (map cardStrongness (fromHand hand)), hand)

  choiceStand' :: (Hand, Deck) -> IO (Hand, Deck)
  choiceStand' (hand, deck) = do
    return (hand, deck)

  returnResult :: Hand -> Player -> IO ((Judge, Int), Hand)
  returnResult hand player = do
    case player of
      Player -> do
        putStrLn " "
        --putStrLn "あなたのハンドは"
        return (judge21 (map cardStrongness (fromHand hand)), hand)
      Enemy -> do
        putStrLn " "
        --putStrLn "あいてのハンドは"
        return (judge21 (map cardStrongness (fromHand hand)), hand)
  --------------------------------------------------------------------

  hitorStand :: (Hand, Deck) -> IO ()
  hitorStand (hand, deck) = do
      print hand
      ynQuestion "Would you hit ?" (choiceHit (hand, deck)) (choiceStand hand)

  hitorStand' :: (Hand, Deck) -> IO (Hand, Deck)
  hitorStand' (hand, deck) = do
      print hand
      ynQuestion "Would you hit ?" (choiceHit' (hand, deck)) (choiceStand' (hand, deck))

  ---------------------------------------------------------------------------

  data Player = Player | Enemy deriving Eq

  showPlayerName :: Player -> String
  showPlayerName Player = "あなた"
  showPlayerName Enemy = "あいて"

  matchBJ :: (Hand, Deck) -> IO ()
  matchBJ (mhand, deck) = do
    (nmhand, ndeck) <- playBJ mhand deck Player
    ((pjudge, px), phand) <- returnResult nmhand Player
    print ((pjudge, px), phand)

    case getHand ndeck of
      Nothing -> error "予期せぬエラー : getHand in matchPoker"
      Just (ehand1, odeck1) -> do
        (enhand1, eodeck1) <- playBJ ehand1 odeck1 Enemy
        ((ejudge1, ex1), ehand1) <- returnResult enhand1 Enemy
        putStrLn "AI_1の手札は"
        print ((ejudge1, ex1), ehand1)
        putStrLn " "

        case getHand eodeck1 of
          Nothing -> error "予期せぬエラー : getHand in matchPoker"
          Just (ehand2, odeck2) -> do
            (enhand2, eodeck2) <- playBJ ehand2 odeck2 Enemy
            ((ejudge2, ex2), ehand2) <- returnResult enhand2 Enemy
            putStrLn "AI_2の手札は"
            print ((ejudge2, ex2), ehand2)
            putStrLn " "

            putStrLn "勝者は"
            print $ strongest [((pjudge, px), phand), ((ejudge1, ex1), ehand1), ((ejudge2, ex2), ehand2)]

  matchBJtest :: (Hand, Deck) -> IO ()
  matchBJtest (mhand, deck) = do
    (nmhand, ndeck) <- playBJ mhand deck Player
    ((pjudge, px), phand) <- returnResult nmhand Player
    print ((pjudge, px), phand)
    (((ejudge1, ex1), ehand1), edeck1) <- makeAIHand (getHand ndeck) 1
    (((ejudge2, ex2), ehand2), edeck2) <- makeAIHand (getHand edeck1) 2
    (((ejudge3, ex3), ehand3), edeck3) <- makeAIHand (getHand edeck2) 3
    (((ejudge4, ex4), ehand4), edeck4) <- makeAIHand (getHand edeck3) 4
    putStrLn "勝者は"
    print $ strongest [((pjudge, px), phand), ((ejudge1, ex1), ehand1), ((ejudge2, ex2), ehand2), ((ejudge3, ex3), ehand3), ((ejudge4, ex4), ehand4)]


  makeAIHand :: Maybe (Hand, Deck) -> Int -> IO (((Judge, Int), Hand), Deck)
  makeAIHand (Just (hand, deck)) x = do
    (enhand1, eodeck1) <- playBJ hand deck Enemy
    ((ejudge1, ex1), ehand1) <- returnResult enhand1 Enemy
    putStrLn ("AI_" ++ show x ++ "の手札は")
    print ((ejudge1, ex1), ehand1)
    putStrLn " "
    return (((ejudge1, ex1), ehand1), eodeck1)
  makeAiHand Nothing x = error "予期せぬエラー : getHand in matchPoker"



  playBJ :: Hand -> Deck -> Player -> IO (Hand, Deck)
  playBJ hand deck player = do
    case player of
      Player -> hitorStand' (hand, deck)
      Enemy -> aiblain (hand, deck)
