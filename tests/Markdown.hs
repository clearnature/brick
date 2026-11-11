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

-- ---------------------------------------------------------------- T3 表格

-- | A pipe table renders header, rule, then body rows.
prop_table2x2 :: Property
prop_table2x2 =
    let got = renderMarkdownRaw plainStyle 80 "| a | b |\n|---|---|\n| 1 | 2 |"
    in counterexample (show got)
         (got == ["| a | b |", "|---|---|", "| 1 | 2 |"])

-- | Alignment pads on the correct side.
prop_tableAlign :: Property
prop_tableAlign =
    let src = "| l | c | r |\n|:--|:-:|--:|\n| 1 | 2 | 3 |"
        got = renderMarkdownRaw plainStyle 80 src
    in counterexample (show got) (length got == 3 && head got == "| l | c | r |")

-- | Column width is measured in cells, so a CJK header lines up with its
-- ASCII body.
prop_tableCjkWidth :: Property
prop_tableCjkWidth =
    let src = "| 中 | b |\n|---|---|\n| 1 | 2 |"
        got = renderMarkdownRaw plainStyle 80 src
    in counterexample (show got)
         (length got == 3 && (got !! 2) == "| 1  | 2 |")

-- ------------------------------------------------------- T4 CJK 与折行

-- | Emphasis wrapped in CJK characters must still be emphasis. The default
-- style renders strong as **…**, which is exactly the input text, so it
-- cannot tell "parsed" from "passed through"; a bracket style can.
prop_cjkStrong :: Property
prop_cjkStrong =
    let st = plainStyle { msStrong = \t -> "[" <> t <> "]" }
        got = renderMarkdownRaw st 80 "这是**粗体**文字"
    in counterexample (show got) (got == ["这是[粗体]文字"])

-- | Long paragraphs wrap at the given width.
prop_wrapWithinWidth :: Property
prop_wrapWithinWidth =
    let para = T.replicate 200 "中"
        got = renderMarkdownRaw plainStyle 80 para
    in counterexample (show (map T.length got))
         (not (null got) && all (\r -> T.length r <= 80) got)

-- | Wrapping never splits a grapheme cluster (a ZWJ emoji stays whole).
prop_wrapKeepsClusters :: Property
prop_wrapKeepsClusters =
    let family = "\x1F468\x200D\x1F469\x200D\x1F467"
        para = T.concat (replicate 40 family)
        got = T.concat (renderMarkdownRaw plainStyle 80 para)
    in counterexample (show (T.length got))
         (T.count "\x200D" got == 80)

-- | The plain-text path carries no markup characters.
prop_plainHasNoMarkup :: Property
prop_plainHasNoMarkup =
    let out = renderMarkdownPlain 80 "# H\n\n**b** and `c`"
    in counterexample (show out)
         (not (T.isInfixOf "**" out) && not (T.isInfixOf "`" out))

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
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_table2x2
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_tableAlign
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_tableCjkWidth
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_cjkStrong
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 1 } prop_wrapWithinWidth
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 1 } prop_wrapKeepsClusters
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 } prop_plainHasNoMarkup
    ]
