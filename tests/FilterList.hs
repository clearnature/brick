{-# LANGUAGE OverloadedStrings #-}
module FilterList (main) where

import Brick.Widgets.FilterList
import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)
import qualified Data.Text as T

-- | An empty query leaves the list alone, so callers can filter on every
-- keystroke without special-casing the empty state.
prop_emptyQueryIdentity :: [String] -> Bool
prop_emptyQueryIdentity xs = filterBy (: []) "" ts == ts
  where ts = map T.pack xs

-- | Whitespace-only queries are also no-ops.
prop_blankQueryIdentity :: [String] -> Bool
prop_blankQueryIdentity xs = filterBy (: []) "   " ts == ts
  where ts = map T.pack xs

-- | A query that matches nothing yields nothing.
prop_noMatchEmpty :: Bool
prop_noMatchEmpty = null (filterBy (: []) "zzzz" ["a" :: T.Text, "bb"])

-- | Filtering is case-insensitive.
prop_caseInsensitive :: String -> Bool
prop_caseInsensitive s =
    let t = T.pack s in filterBy (: []) "ABC" [t] == filterBy (: []) "abc" [t]

-- | Everything returned by filterBy actually matches the query.
prop_allMatched :: String -> [String] -> Bool
prop_allMatched q xs =
    let qq = T.pack q
        ts = map T.pack xs
    in T.null (T.strip qq) || all (T.isInfixOf (T.toLower (T.strip qq)) . T.toLower) (filterBy (: []) qq ts)

-- | clampSelection always lands inside the list.
prop_clampInRange :: Int -> Int -> NonNegative Int -> Bool
prop_clampInRange delta sel (NonNegative n)
    | n <= 0 = clampSelection delta sel n == 0
    | otherwise = clampSelection delta sel n >= 0 && clampSelection delta sel n <= n - 1

-- | firstEnabled points at a selectable element when one exists.
prop_firstEnabledSelectable :: NonEmptyList Bool -> Bool
prop_firstEnabledSelectable (NonEmpty fs) =
    let flags = True : fs          -- ensure at least one selectable entry
        i     = firstEnabled id flags
    in i >= 0 && i < length flags && flags !! i

-- | moveSkipping never parks on a non-selectable element.
prop_moveSkippingSelectable :: NonZero Int -> NonEmptyList Bool -> Bool
prop_moveSkippingSelectable (NonZero delta) (NonEmpty fs) =
    let flags = True : fs          -- ensure at least one selectable entry
        i     = moveSkipping id delta 0 flags
    in i >= 0 && i < length flags && flags !! i

-- | An empty list yields the conventional empty range.
prop_windowEmpty :: Bool
prop_windowEmpty = windowRange 0 0 10 == (0, -1)

-- | When everything fits, the window is the whole list.
prop_windowFits :: Positive Int -> NonNegative Int -> Bool
prop_windowFits (Positive h) (NonNegative total)
    | total <= h = windowRange total 0 h == (0, total - 1)
    | otherwise = True

-- | A non-empty window always contains the selection.
prop_windowContainsSelection :: Positive Int -> NonNegative Int -> NonNegative Int -> Property
prop_windowContainsSelection (Positive h) (NonNegative total) (NonNegative sel) =
    total > 0 && h > 0 ==>
      let s = min sel (total - 1)
          (lo, hi) = windowRange total s h
      in lo <= s && s <= hi && lo >= 0 && hi <= total - 1

main :: IO Bool
main = and <$> sequenceA
    [ isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_emptyQueryIdentity
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_blankQueryIdentity
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_noMatchEmpty
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_caseInsensitive
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_allMatched
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_clampInRange
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_firstEnabledSelectable
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_moveSkippingSelectable
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_windowEmpty
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_windowFits
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_windowContainsSelection
    ]
