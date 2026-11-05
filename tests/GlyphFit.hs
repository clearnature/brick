{-# LANGUAGE OverloadedStrings #-}

module GlyphFit (main) where

import Brick.Widgets.GlyphFit
import Brick.Widgets.TextWidth (charCellWidth, textCells)
import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)
import qualified Data.Text as T

-- | A console that draws nothing beyond ASCII: every non-ASCII character has
-- to be fitted. This is the worst case, and the one that must never move the
-- layout.
strictAscii :: FitPolicy
strictAscii = FitPolicy
  { gfCells    = charCellWidth
  , gfHasGlyph = (< '\x80')
  , gfBestFit  = const Nothing
  }

-- | A console that draws everything: fitting must be a no-op.
trusting :: FitPolicy
trusting = trustAllGlyphs charCellWidth

-- | ASCII passes through untouched.
prop_asciiUnchanged :: String -> Bool
prop_asciiUnchanged s =
    let t = T.pack (filter (< '\x80') s)
    in applyGlyphFit strictAscii t == t

-- | A console that draws everything gets the string back verbatim.
prop_trustingIdentity :: String -> Bool
prop_trustingIdentity s =
    let t = T.pack s in applyGlyphFit trusting t == t

-- | The invariant that matters: fitting never changes the cell count.
prop_widthPreserved :: String -> Bool
prop_widthPreserved s =
    let t = T.pack s
    in textCells (applyGlyphFit strictAscii t) == textCells t

-- | Fitting never makes a string wider.
prop_neverWider :: String -> Bool
prop_neverWider s =
    let t = T.pack s
    in textCells (applyGlyphFit strictAscii t) <= textCells t

-- | Everything left in the output is drawable by the console.
prop_allDrawable :: String -> Bool
prop_allDrawable s =
    let out = applyGlyphFit strictAscii (T.pack s)
    in T.all (< '\x80') out

-- | A known stand-in is used when one exists.
prop_checkmarkUsesStandIn :: Bool
prop_checkmarkUsesStandIn = applyGlyphFit strictAscii "\x2713 done" == "v done"

-- | A wide character with no stand-in becomes blanks of the same width.
prop_emojiBecomesBlanks :: Bool
prop_emojiBecomesBlanks = applyGlyphFit strictAscii "\x1F600" == "  "

-- | A no-op policy reports that nothing needs fitting.
prop_needsFittingFalse :: Bool
prop_needsFittingFalse = not (needsFitting trusting "hello \x2713")

-- | A strict policy reports the need.
prop_needsFittingTrue :: Bool
prop_needsFittingTrue = needsFitting strictAscii "\x2713"

main :: IO Bool
main = and <$> sequenceA
    [ isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_asciiUnchanged
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_trustingIdentity
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_widthPreserved
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_neverWider
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_allDrawable
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_checkmarkUsesStandIn
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_emojiBecomesBlanks
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_needsFittingFalse
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_needsFittingTrue
    ]
