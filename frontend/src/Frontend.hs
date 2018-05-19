{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import Control.Monad (forM_)
import Data.List (group, sort)
import Data.Semigroup ((<>))
import qualified Data.Text as T

import Language.Javascript.JSaddle (JSM)
import Reflex.Dom.Core
import Static

import Common.Api

data Card
  = Card_MinusFive | Card_0 | Card_1 | Card_2 | Card_3 | Card_4 | Card_5
  | Card_6 | Card_7 | Card_8 | Card_9 | Card_10 | Card_11 | Card_12
  deriving (Eq, Show, Ord, Bounded, Enum)

cardCount :: Card -> Int
cardCount = \case
  Card_MinusFive -> 4
  _ -> 8

deck :: [Card]
deck = mconcat $ fmap f [minBound .. maxBound]
  where
    f c = replicate (cardCount c) c

frontend :: JSM ()
frontend = mainWidgetWithHead' (const head', const body)
  where
    head'= elAttr "link" ("rel" =: "stylesheet" <> "href" =: static @"semantic.min.css") blank
    body = do
      divClass "ui container" $ divClass "ui segment" $ do
        elClass "h1" "ui header" $ text "Play Nine Golf"
        showProbabilities deck

showProbabilities :: MonadWidget t m => [Card] -> m ()
showProbabilities cards = divClass "ui cards" $ do
  forM_ (group $ sort cards) $ \grp -> divClass "card" $ divClass "content" $ do
    divClass "header" $ text $ T.pack $ show $ head grp
    divClass "description" $ text $ T.pack $ show $ length grp
