-- | Terminal cell widths that respect grapheme clusters.
--
-- Brick measures text with 'Brick.Widgets.Core.textWidth', which is
-- implemented in terms of the platform's @wcwidth@.  That is the right
-- answer for a single code point, but it leaves two things to the caller:
--
--   * /grapheme clusters/ -- a base character plus combining marks, a ZWJ
--     emoji sequence, or a regional-indicator flag pair must be measured and
--     truncated as one unit, or truncation leaves half a glyph on screen;
--   * /emoji width/ -- many platforms report emoji as one cell, while
--     terminals render them as two.
--
-- This module answers both from Unicode data alone (East-Asian Width blocks
-- plus general categories), so its results do not depend on the C library
-- the program happens to be linked against.
--
-- == Usage
--
-- > textCells "中a😀"          -- 5
-- > takeCells 3 "中a中"        -- "中a" (never splits a cluster)
-- > dropCells 3 "中a中"        -- "中"
module Brick.Widgets.TextWidth
  ( -- * Single characters
    charCellWidth
  , isWideChar

    -- * Grapheme clusters
  , graphemes
  , clusterWidth

    -- * Strings
  , textCells
  , takeCells
  , dropCells
  ) where

import Data.Char (GeneralCategory (..), generalCategory, ord)
import Data.Text (Text)
import qualified Data.Text as T

-- | Cells occupied by one character in a terminal.
--
-- Combining marks occupy no cells of their own; control characters,
-- surrogates and unassigned code points are treated as zero so they cannot
-- push a line past its budget.
charCellWidth :: Char -> Int
charCellWidth c
  | cat `elem` [NonSpacingMark, SpacingCombiningMark, EnclosingMark] = 0
  | cat `elem` [Control, Format, Surrogate, NotAssigned] = 0
  | isWideChar c = 2
  | otherwise = 1
  where
    cat = generalCategory c

-- | East-Asian wide/fullwidth blocks plus the emoji ranges terminals render
-- double-width.
isWideChar :: Char -> Bool
isWideChar c =
  let n = ord c
  in  (n >= 0x1100 && n <= 0x115F)   -- Hangul Jamo
   || (n >= 0x2E80 && n <= 0x303E)   -- CJK radicals .. Kangxi
   || (n >= 0x3041 && n <= 0x33FF)   -- kana, CJK symbols
   || (n >= 0x3400 && n <= 0x4DBF)   -- CJK ext A
   || (n >= 0x4E00 && n <= 0x9FFF)   -- CJK unified
   || (n >= 0xA000 && n <= 0xA4CF)   -- Yi
   || (n >= 0xAC00 && n <= 0xD7A3)   -- Hangul syllables
   || (n >= 0xF900 && n <= 0xFAFF)   -- CJK compatibility
   || (n >= 0xFE30 && n <= 0xFE4F)   -- CJK compatibility forms
   || (n >= 0xFF00 && n <= 0xFF60)   -- fullwidth forms
   || (n >= 0xFFE0 && n <= 0xFFE6)   -- fullwidth signs
   || (n >= 0x1F300 && n <= 0x1F64F) -- emoji (pictographs)
   || (n >= 0x1F900 && n <= 0x1F9FF) -- supplemental emoji
   || (n >= 0x20000 && n <= 0x3FFFD) -- CJK ext B+

-- | Split into grapheme clusters: base + combining marks / variation
-- selectors, ZWJ-joined continuations, and regional-indicator flag pairs.
graphemes :: Text -> [Text]
graphemes = go . T.unpack
  where
    go [] = []
    go (c : d : cs)
      | isRegional c && isRegional d = T.pack [c, d] : go cs
    go (c : cs) = let (cluster, rest) = spanCluster [c] cs in T.pack cluster : go rest

    spanCluster acc [] = (reverse acc, [])
    spanCluster acc (c : cs)
      | isCombining c || c == '\xFE0F' || c == '\xFE0E' = spanCluster (c : acc) cs
      | c == '\x200D' = case cs of
          (n : cs') -> spanCluster (n : c : acc) cs'
          []        -> (reverse acc, [])
      | otherwise = (reverse acc, c : cs)

    isCombining c = generalCategory c `elem` [NonSpacingMark, SpacingCombiningMark, EnclosingMark]
    isRegional c = let n = ord c in n >= 0x1F1E6 && n <= 0x1F1FF

-- | Cells one cluster occupies.  A ZWJ sequence renders as one emoji, and a
-- regional-indicator pair as one flag -- both are two cells regardless of how
-- many code points they contain.
clusterWidth :: Text -> Int
clusterWidth g
  | T.any (== '\x200D') g = 2
  | T.length g >= 2 && T.all isRegional g = 2
  | otherwise = case T.uncons g of
      Just (c, _) -> charCellWidth c
      Nothing     -> 0
  where
    isRegional c = let n = ord c in n >= 0x1F1E6 && n <= 0x1F1FF

-- | Total terminal cells of a string.
textCells :: Text -> Int
textCells = sum . map clusterWidth . graphemes

-- | The longest prefix that fits in @n@ cells (never splits a cluster).
takeCells :: Int -> Text -> Text
takeCells n t
  | n < 0 = T.empty
  -- 注意是 n < 0 而不是 n <= 0：宽度为 0 的字符（换行、控制符、组合标记）
  -- 在 0 列预算下依然放得下，若在这里短路会让 takeCells 0 丢掉它们，
  -- 破坏 takeCells/dropCells 的互补性。
  | otherwise = T.concat (go n (graphemes t))
  where
    go _ [] = []
    go left (g : gs)
      | w <= left = g : go (left - w) gs
      | otherwise = []
      where
        w = clusterWidth g

-- | Drop the prefix that fits in @n@ cells (the complement of 'takeCells').
dropCells :: Int -> Text -> Text
dropCells n t
  | n < 0 = t
  -- 与 takeCells 对称：0 列预算时，宽度为 0 的字符归 take 一侧，drop 一侧
  -- 什么也不丢。两处都用 n < 0 才能保证 takeCells n t <> dropCells n t == t。
  | otherwise = T.concat (go n (graphemes t))
  where
    go _ [] = []
    go left (g : gs)
      | w <= left = go (left - w) gs
      | otherwise = g : gs
      where
        w = clusterWidth g
