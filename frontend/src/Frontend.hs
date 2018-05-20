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

-- TODO: A function to render cards. Take direct scan of the physical cards, but don't put 'em publicly.

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

frontend :: (StaticWidget x (), Widget x ())
frontend = (head', body)
  where
    head'= elAttr "link" ("rel" =: "stylesheet" <> "href" =: static @"semantic.min.css") blank
    body = do
      divClass "ui container" $ divClass "ui segment" $ do
        elClass "h1" "ui header" $ text "Play Nine Golf"
        controlWidget
        showProbabilities deck

controlWidget :: MonadWidget t m => m ()
controlWidget = divClass "ui raised segment" $ do
  -- TODO: Use an increment (number) widget here
  pile <- value <$> textInput def
  -- TODO: Use a list widget here (to which we can append cards)
  turned <- value <$> textInput def
  let probabilities = zipDynWith calcProbabilities
          (parsePile <$> pile)
          (parseCards <$> turned)
  divClass "ui segment" $ do
    divClass "ui header" $ text "Probabilities"
    divClass "ui contenet" $ do
      el "tt" $ dynText $ fmap (T.pack . show) probabilities
  return ()
  where
    parsePile = read . T.unpack
    parseCards _ = []

showProbabilities :: MonadWidget t m => [Card] -> m ()
showProbabilities cards = divClass "ui cards" $ do
  forM_ (group $ sort cards) $ \grp -> divClass "card link" $ divClass "content" $ do
    divClass "header" $ text $ T.pack $ show $ head grp
    divClass "description" $ text $ T.pack $ show $ length grp

-- | Calculate the probablity of a card appearing next in the pile.
calcProbabilities
  :: Int  -- Number of cards on pile (unturned)
  -> [Card] -- Cards on the table that are turned
  -> [(Card, Double)]
calcProbabilities inPile turned = [] -- TODO

-- | Pop one card out of the pipe
popPile
  :: Int  -- Number of cards on pile (unturned)
  -> Int
popPile n = n - 1

turnCard
  :: [Card]  -- ^ Currently turned cards
  -> Card  -- ^ Card that was just turned
  -> [Card]
turnCard xs x = x : xs
