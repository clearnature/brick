{-# LANGUAGE OverloadedStrings #-}
-- | Degrade glyphs the terminal cannot draw, without moving the layout.
--
-- A terminal font does not cover everything Unicode offers.  When a symbol
-- the interface draws (a check mark, a box-drawing corner, a spinner frame)
-- has no glyph, the console renders it as a replacement box, as a blank, or
-- -- worse -- as a different number of cells than the code counted.  Lines
-- then drift out of alignment.
--
-- This module rewrites such characters to something the terminal does draw,
-- preserving /the cell count/: a stand-in that is narrower than the original
-- is padded, so a string keeps its width no matter what gets substituted.
--
-- What the terminal can draw is not knowable from the library, so it is
-- supplied by the caller as three functions:
--
--   * 'gfCells' -- how many cells a character occupies (see
--     "Brick.Widgets.TextWidth");
--   * 'gfHasGlyph' -- whether the console's font has a glyph for it (on
--     Windows this is a GDI font probe; elsewhere a basic coverage set);
--   * 'gfBestFit' -- the platform's own best-fit mapping, if it has one.
--
-- == Usage
--
-- > policy = FitPolicy
-- >   { gfCells    = charCellWidth
-- >   , gfHasGlyph = fontHasGlyph
-- >   , gfBestFit  = const Nothing
-- >   }
-- > applyGlyphFit policy "✓ done"   -- "√ done" on a console without ✓
--
-- The result never contains a character that was rejected by 'gfHasGlyph'
-- and is never wider than the input.
module Brick.Widgets.GlyphFit
  ( -- * Policy
    FitPolicy(..)
  , noBestFit
  , trustAllGlyphs

    -- * Fitting
  , fitChar
  , applyGlyphFit
  , needsFitting

    -- * Stand-ins
  , glyphStandIns
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

-- | What the terminal can show, and how wide it measures things.
data FitPolicy = FitPolicy
  { gfCells    :: Char -> Int
    -- ^ Cells the character occupies.  Must agree with how the rest of the
    -- program measures text, or padding will be wrong.
  , gfHasGlyph :: Char -> Bool
    -- ^ Whether the console font can draw the character.
  , gfBestFit  :: Char -> Maybe Char
    -- ^ The platform's best-fit mapping, if any (Windows GDI supplies one).
  }

-- | A policy whose platform offers no best-fit mapping.
noBestFit :: FitPolicy -> FitPolicy
noBestFit p = p { gfBestFit = const Nothing }

-- | A policy for a terminal that draws everything: fitting becomes a no-op.
-- Useful as the default when font probing is unavailable.
trustAllGlyphs :: (Char -> Int) -> FitPolicy
trustAllGlyphs cells = FitPolicy
  { gfCells    = cells
  , gfHasGlyph = const True
  , gfBestFit  = const Nothing
  }

-- | Symbols of the same meaning, tried in order.  These are the marks an
-- interface actually draws (status icons, borders, spinners, Greek letters);
-- each is paired with ASCII or box-drawing characters that every monospace
-- font covers.
glyphStandIns :: Map Char [Text]
glyphStandIns = M.fromList
  [ ('◆', ["*"]), ('◇', ["*"]), ('●', ["•", "*"]), ('○', ["o"]), ('•', ["*"])
  , ('■', ["#"]), ('□', ["#"]), ('★', ["*"]), ('☆', ["*"])
  , ('✓', ["√", "v"]), ('✔', ["√", "v"]), ('✅', ["√", "OK"]), ('☑', ["x"]), ('☐', ["o"])
  , ('✗', ["×", "x"]), ('✘', ["×", "x"]), ('❌', ["×", "X"]), ('⚠', ["!"])
  , ('⎿', ["└", "L"]), ('↳', ["└", ">"]), ('▸', [">"]), ('▶', [">"]), ('▾', ["v"])
  , ('❯', [">"]), ('›', [">"]), ('→', [">"]), ('←', ["<"]), ('↑', ["^"]), ('↓', ["v"])
  , ('⟲', ["<"]), ('⧗', ["~"]), ('▎', ["│", "|"])
  , ('│', ["|"]), ('─', ["-"]), ('█', ["#"]), ('╭', ["┌", "+"]), ('╮', ["┐", "+"])
  , ('╰', ["└", "+"]), ('╯', ["┘", "+"]), ('┼', ["+"]), ('❤', ["♥", "*"])
  , ('×', ["x"]), ('÷', ["/"]), ('±', ["+"]), ('°', ["o"]), ('≈', ["~"])
  , ('≤', ["<"]), ('≥', [">"]), ('≠', ["!"]), ('∞', ["~"]), ('√', ["v"])
  , ('※', ["*"]), ('…', ["."]), ('·', ["."])
  , ('①', ["1"]), ('②', ["2"]), ('③', ["3"]), ('④', ["4"]), ('⑤', ["5"])
  , ('⑥', ["6"]), ('⑦', ["7"]), ('⑧', ["8"]), ('⑨', ["9"])
  , ('α', ["a"]), ('β', ["b"]), ('γ', ["g"]), ('δ', ["d"]), ('ε', ["e"])
  , ('θ', ["q"]), ('λ', ["l"]), ('μ', ["u"]), ('π', ["p"]), ('σ', ["s"])
  , ('τ', ["t"]), ('φ', ["f"]), ('ω', ["w"]), ('Δ', ["D"]), ('Σ', ["S"]), ('Ω', ["W"])
  , ('⣾', ["|"]), ('⣽', ["/"]), ('⣻', ["-"]), ('⢿', ["\\"]), ('⡿', ["|"])
  , ('⣟', ["/"]), ('⣯', ["-"]), ('⣷', ["\\"])
  ]

-- | Whether the terminal draws the character at the width the policy counts.
draws :: FitPolicy -> Char -> Bool
draws p c = c < '\x80' || gfHasGlyph p c

-- | Whether /any/ character in the string needs fitting.  Callers use this to
-- skip the rewrite (and its allocation) on the common all-ASCII path.
needsFitting :: FitPolicy -> Text -> Bool
needsFitting p = T.any (\c -> c >= '\x80' && fitChar p c /= T.singleton c)

-- | The replacement for one character, padded so the cell count is unchanged.
--
-- Order of attempts: the character itself, a stand-in of the same meaning,
-- the platform best-fit, then blanks.  A candidate is accepted only when it
-- is shorter or equally wide /and/ fully drawable.
fitChar :: FitPolicy -> Char -> Text
fitChar p c
  | c < '\x80' = T.singleton c
  | draws p c = T.singleton c
  | otherwise =
      case [ cand | cand <- candidates, isUsable cand ] of
        (cand : _) -> cand <> T.replicate (counted - candWidth cand) " "
        []         -> T.replicate counted " "
  where
    counted = gfCells p c
    candidates =
      fromMaybe [] (M.lookup c glyphStandIns)
        <> maybe [] (pure . T.singleton) (gfBestFit p c)
    isUsable cand =
      cand /= T.singleton c
        && candWidth cand <= counted
        && T.all (draws p) cand
    candWidth = sum . map (gfCells p) . T.unpack

-- | Rewrite a plain string so every character is drawable and the total cell
-- count is preserved.
--
-- An all-ASCII or fully-drawable string is returned unchanged (same value,
-- no work).  This function assumes plain text: it does not parse escape
-- sequences, because Brick widgets carry structure rather than a styled byte
-- stream.
applyGlyphFit :: FitPolicy -> Text -> Text
applyGlyphFit p t
  | not (needsFitting p t) = t
  | otherwise = T.concat (map (fitChar p) (T.unpack t))
