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

-- ---------------------------------------------------------------- T1 块级

-- | A level-1 heading renders its level and text.
prop_h1 :: Bool
prop_h1 = renderMarkdownRaw plainStyle 80 "# Title" == ["# Title"]

-- | A level-3 heading keeps its level.
prop_h3 :: Bool
prop_h3 = renderMarkdownRaw plainStyle 80 "### Sub" == ["### Sub"]

-- | Bullet list: one row per item, marker preserved.
prop_unorderedList :: Bool
prop_unorderedList =
    renderMarkdownRaw plainStyle 80 "- a\n- b" == ["- a", "- b"]

-- | Ordered list keeps the numbers.
prop_orderedList :: Bool
prop_orderedList =
    renderMarkdownRaw plainStyle 80 "1. a\n2. b" == ["1. a", "2. b"]

-- | A nested list indents the inner items.
prop_nestedList :: Property
prop_nestedList =
    let got = renderMarkdownRaw plainStyle 80 "- a\n  - b"
    in counterexample (show got) (got == ["- a", "  - b"])

-- | A fenced code block yields its content rows, verbatim.
prop_fencedCode :: Property
prop_fencedCode =
    let got = renderMarkdownRaw plainStyle 80 "```\nx\ny\n```"
    in counterexample (show got) (got == ["x", "y"])

-- | The language tag is not part of the content.
prop_fencedCodeLang :: Bool
prop_fencedCodeLang =
    renderMarkdownRaw plainStyle 80 "```hs\nx\n```" == ["x"]

-- | A blockquote is prefixed with the quote bar.
prop_blockquote :: Property
prop_blockquote =
    let got = renderMarkdownRaw plainStyle 80 "> q"
    in counterexample (show got) (got == ["\x2502 q"])

-- | A thematic break spans the available width.
prop_thematicBreak :: Bool
prop_thematicBreak =
    renderMarkdownRaw plainStyle 80 "---" == [T.replicate 80 "-"]

-- ---------------------------------------------------------------- T2 行内

-- | Strong text keeps its markers under plainStyle.
prop_strong :: Property
prop_strong =
    let got = renderMarkdownRaw plainStyle 80 "**b**"
    in counterexample (show got) (got == ["**b**"])

-- | Emphasis keeps its markers.
prop_emphasis :: Property
prop_emphasis =
    let got = renderMarkdownRaw plainStyle 80 "*i*"
    in counterexample (show got) (got == ["*i*"])

-- | An inline code span keeps its backticks.
prop_codeSpan :: Property
prop_codeSpan =
    let got = renderMarkdownRaw plainStyle 80 "`c`"
    in counterexample (show got) (got == ["`c`"])

-- | A link renders its text and target.
prop_link :: Property
prop_link =
    let got = renderMarkdownRaw plainStyle 80 "[t](u)"
    in counterexample (show got) (got == ["t (u)"])

-- | Mixed inline markup composes.
prop_mixedInline :: Property
prop_mixedInline =
    let got = renderMarkdownRaw plainStyle 80 "**a** *b* `c`"
    in counterexample (show got) (got == ["**a** *b* `c`"])

main :: IO Bool
main = and <$> sequenceA
    [ isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_emptyInput
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_plainParagraph
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_twoParagraphs
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_softBreakJoins
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_neverCrashes
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_h1
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_h3
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_unorderedList
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_orderedList
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_nestedList
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_fencedCode
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_fencedCodeLang
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_blockquote
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_thematicBreak
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_strong
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_emphasis
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_codeSpan
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_link
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_mixedInline
    ]
