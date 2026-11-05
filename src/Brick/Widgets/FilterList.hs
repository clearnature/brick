-- | Selection and filtering helpers for searchable lists.
--
-- "Brick.Widgets.List" handles element movement, but it has no notion of a
-- /query/: it cannot filter elements as the user types, it cannot skip
-- non-selectable rows (section headers), and it cannot compute where the
-- selection should land after a filter shrinks the list.  Applications with
-- a command palette or a picker end up writing those four things locally.
--
-- Everything here is a pure function over lists.  Nothing depends on how an
-- element is represented: the two places that need to know something about
-- elements take it as a function argument instead of a type class, so using
-- this module requires no instances and no imported types.
--
-- == Usage
--
-- > data Item = Item { itName, itDesc :: Text, itHeader :: Bool }
-- >
-- > let searchable i = [itName i, itDesc i]     -- what the query matches against
-- >     selectable i = not (itHeader i)         -- section headers are skipped
-- >
-- > filterBy searchable "git" items             -- filtered view
-- > moveSkipping selectable 1 sel filtered      -- next entry, never a header
-- > firstEnabled selectable filtered            -- where to park after filtering
-- > windowRange (length filtered) sel height    -- visible slice
module Brick.Widgets.FilterList
  ( -- * The query buffer
    queryAppend
  , queryDelete

    -- * Filtering
  , filterBy

    -- * Selection
  , clampSelection
  , moveSkipping
  , firstEnabled

    -- * Scrolling
  , windowRange
  ) where

import Data.Char (toLower)
import Data.List (findIndex)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

-- | Append a character to the query buffer.
queryAppend :: Text -> Char -> Text
queryAppend = T.snoc

-- | Delete the last character of the query buffer (one full Unicode
-- character, not one byte).
queryDelete :: Text -> Text
queryDelete = T.dropEnd 1

-- | Filter elements by a case-insensitive substring match.
--
-- The query is stripped and lowercased once.  Each element is turned into
-- the texts it should be matched against by the supplied projection; the
-- element passes when any of them contains the query.
--
-- An empty (or whitespace-only) query returns the list unchanged, which lets
-- callers apply it unconditionally on every keystroke.
filterBy :: (a -> [Text])
         -- ^ The texts an element is matched against (e.g. its name and
         -- description)
         -> Text
         -- ^ The query string
         -> [a]
         -- ^ The elements to filter
         -> [a]
filterBy searchable query = go
  where
    q = T.toLower (T.strip query)
    go xs
      | T.null q = xs
      | otherwise = filter matches xs
    matches x = any (T.isInfixOf q . T.toLower) (searchable x)

-- | Move the selection by a delta, clamped to the valid range.
--
-- An empty list yields 0, so callers never have to special-case it.
clampSelection :: Int
               -- ^ The delta
               -> Int
               -- ^ The current selection
               -> Int
               -- ^ The total number of elements
               -> Int
clampSelection delta selected total
  | total <= 0 = 0
  | otherwise  = max 0 (min (selected + delta) (total - 1))

-- | Index of the first selectable element.
--
-- Returns 0 for an empty or entirely non-selectable list; callers that care
-- about that case can check 'firstEnabled' against their own emptiness test.
firstEnabled :: (a -> Bool)
             -- ^ Whether an element is selectable
             -> [a]
             -- ^ The elements
             -> Int
firstEnabled selectable = fromMaybe 0 . findIndex selectable

-- | Move the selection by a delta, skipping non-selectable elements.
--
-- This is what keeps a picker's cursor off a section header.  Moving past
-- either end parks the selection on the nearest selectable element rather
-- than wrapping, and a list with nothing selectable keeps the selection.
moveSkipping :: (a -> Bool)
             -- ^ Whether an element is selectable
             -> Int
             -- ^ The delta (+1 next, -1 previous)
             -> Int
             -- ^ The current selection
             -> [a]
             -- ^ The elements
             -> Int
moveSkipping selectable delta selected items
  | null candidates = 0
  | delta > 0 = case [ i | i <- candidates, i > selected ] of
      (i : _) -> i
      []      -> last candidates
  | delta < 0 = case [ i | i <- candidates, i < selected ] of
      []  -> head candidates
      is' -> last is'
  | otherwise = selected
  where
    candidates = [ i | (i, it) <- zip [0 ..] items, selectable it ]

-- | The visible window @(first, last)@ of a scrollable list, keeping the
-- selection roughly centred.
--
-- An empty list yields @(0, -1)@, which is the conventional empty range: it
-- can be fed straight into a @take (last - first + 1) . drop first@ pipeline.
windowRange :: Int
            -- ^ Total number of elements
            -> Int
            -- ^ The selection
            -> Int
            -- ^ The visible height in rows
            -> (Int, Int)
windowRange total selected height
  | total <= 0      = (0, -1)
  | total <= height = (0, total - 1)
  | height <= 0     = (selected, selected - 1)
  | otherwise =
      let start = max 0
            (min (selected - (height - 1) `div` 2) (total - height))
      in (start, start + height - 1)
