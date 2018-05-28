{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import Control.Monad (forM_)
import Data.List (group, sort)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T

import Language.Javascript.JSaddle (JSM)
import Reflex.Dom.Core
import Static

import Common.Api

-- TODO: A function to render cards. Take direct scan of the physical cards, but don't put 'em publicly.

data Card
  = Card_MinusFive | Card_0 | Card_1 | Card_2 | Card_3 | Card_4 | Card_5
  | Card_6 | Card_7 | Card_8 | Card_9 | Card_10 | Card_11 | Card_12
  deriving (Eq, Show, Ord, Bounded, Enum, Read)

parseCard :: Text -> Card
parseCard = f . read . T.unpack
  where
    f = \case
      0 -> Card_0
      1 -> Card_1
      2 -> Card_2
      3 -> Card_3
      4 -> Card_4
      5 -> Card_5
      6 -> Card_6
      7 -> Card_7
      8 -> Card_8
      9 -> Card_9
      10 -> Card_10
      11 -> Card_11
      12 -> Card_12
      -5 -> Card_MinusFive
      otherwise -> error "bad number" -- XXX

cardCount :: Card -> Int
cardCount = \case
  Card_MinusFive -> 4
  _ -> 8

-- | Entire deck of Play Nine cards
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
  probabilities <- divClass "ui form" $ do
    pile <- divClass "field" $ do
      -- TODO: Use an increment (number) widget here
      el "label" $ text "Pile count"
      value <$> textInput (def & textInputConfig_initialValue .~ "0")
    -- TODO: Use a list widget here (to which we can append cards)
    turned <- divClass "field" $ do
      el "label" $ text "Turned cards"
      value <$> textInput def
    return $ zipDynWith calcProbabilities
      (parsePile <$> pile)
      (parseCards <$> turned)
  divClass "ui segment" $ do
    divClass "ui header" $ text "Probabilities"
    divClass "ui contenet" $ do
      el "tt" $ dynText $ fmap (T.pack . show) probabilities
  return ()
  where
    parsePile = read . T.unpack
    parseCards = fmap parseCard . T.words

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
calcProbabilities inPile turned = map (, 1.0) turned -- TODO

-- | Play a turn
-- playTurn
--   :: Int -- Player
--   ->

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
