{-# LANGUAGE OverloadedStrings #-}

module TextWidth (main) where

import Brick.Widgets.TextWidth
import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)
import qualified Data.Text as T

-- | ASCII occupies one cell.
prop_asciiWidth :: Char -> Property
prop_asciiWidth c = c >= ' ' && c <= '~' ==> charCellWidth c == 1

-- | Combining marks take no cells of their own.
prop_combiningZero :: Bool
prop_combiningZero = charCellWidth '\x0301' == 0

-- | CJK is two cells.
prop_cjkWidth :: Bool
prop_cjkWidth = charCellWidth '中' == 2

-- | Emoji are two cells regardless of the platform's wcwidth.
prop_emojiWidth :: Bool
prop_emojiWidth = charCellWidth '😀' == 2

-- | A ZWJ sequence measures as a single two-cell glyph.
prop_zwjWidth :: Bool
prop_zwjWidth = textCells "\x1F468\x200D\x1F469\x200D\x1F467" == 2

-- | A regional-indicator pair measures as one two-cell flag.
prop_flagWidth :: Bool
prop_flagWidth = textCells "\x1F1E8\x1F1F3" == 2

-- | A combining accent belongs to its base character.
prop_combiningBelongsToBase :: Bool
prop_combiningBelongsToBase = length (graphemes "e\x0301") == 1

-- | Mixed text sums the cell widths.
prop_mixedWidth :: Bool
prop_mixedWidth = textCells "中a😀" == 5

-- | takeCells never exceeds the budget.
prop_takeWithinBudget :: String -> NonNegative Int -> Bool
prop_takeWithinBudget s (NonNegative n) =
    textCells (takeCells n (T.pack s)) <= n

-- | takeCells and dropCells are complementary: nothing is lost or duplicated.
prop_takeDropComplementary :: String -> NonNegative Int -> Bool
prop_takeDropComplementary s (NonNegative n) =
    let t = T.pack s in takeCells n t <> dropCells n t == t

-- | Taking everything yields the whole string.
prop_takeAll :: String -> Bool
prop_takeAll s =
    let t = T.pack s in takeCells (textCells t) t == t

-- | A zero budget yields only zero-width characters (newlines, control
-- characters, combining marks) -- never anything that occupies a cell.
prop_takeZeroWidths :: String -> Bool
prop_takeZeroWidths s = textCells (takeCells 0 (T.pack s)) == 0

main :: IO Bool
main = and <$> sequenceA
    [ isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_asciiWidth
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_combiningZero
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_cjkWidth
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_emojiWidth
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_zwjWidth
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_flagWidth
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_combiningBelongsToBase
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_mixedWidth
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_takeWithinBudget
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_takeDropComplementary
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_takeAll
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_takeZeroWidths
    ]
