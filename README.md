## haskell で ブラックジャック

モジュール等は、cabalを参照

実行コマンド

        stack exec BJ-exe

実行画面の例

        ------------------
        -- simple black jack --
        ------------------
        Hand {fromHand = [H4_,C5_]}
        Would you hit ?(y/n)
        y
        Hand {fromHand = [H4_,C5_,C6_]}
        would you hit more ? (y/n)
        n

        ((AnyScore,15),Hand {fromHand = [H4_,C5_,C6_]})

        AI_1の手札は
        ((Bust,23),Hand {fromHand = [D2_,S10,HA_]})


        AI_2の手札は
        ((Bust,23),Hand {fromHand = [C3_,CJ_,SQ_]})


        AI_3の手札は
        ((BlackJack,21),Hand {fromHand = [H2_,D3_,D6_,C10]})


        AI_4の手札は
        ((Bust,27),Hand {fromHand = [D7_,DQ_,SK_]})

        勝者は
        (BlackJack,21)

        Would you continue to play game ?(y/n)
        n
        Good Bye !!
