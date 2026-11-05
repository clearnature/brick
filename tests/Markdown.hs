{-# LANGUAGE OverloadedStrings #-}

module Markdown (main) where

import Brick.Widgets.Markdown
import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)
import qualified Data.Text as T

-- | An empty document has no rows.
prop_emptyInput :: Bool
prop_emptyInput = renderMarkdownRaw plainStyle 80 "" == []

-- | A single paragraph is one row.
prop_plainParagraph :: Bool
prop_plainParagraph = renderMarkdownRaw plainStyle 80 "hello" == ["hello"]

-- | Two paragraphs are separated by a blank row.
prop_twoParagraphs :: Bool
prop_twoParagraphs =
    renderMarkdownRaw plainStyle 80 "a\n\nb" == ["a", "", "b"]

-- | A soft line break joins into one row (CommonMark: it is not a new block).
prop_softBreakJoins :: Bool
prop_softBreakJoins =
    renderMarkdownRaw plainStyle 80 "a\nb" == ["a b"]

-- | Whatever the input, rendering returns (never throws).
prop_neverCrashes :: String -> Bool
prop_neverCrashes s =
    length (renderMarkdownRaw plainStyle 80 (T.pack s)) >= 0

main :: IO Bool
main = and <$> sequenceA
    [ isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_emptyInput
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_plainParagraph
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_twoParagraphs
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_softBreakJoins
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_neverCrashes
    ]
