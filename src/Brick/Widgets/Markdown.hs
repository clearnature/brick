{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Render Markdown to Brick widgets.
--
-- Parsing is delegated to "Commonmark"; this module owns the step the parser
-- leaves to the caller: turning blocks and inlines into terminal rows.
--
-- The trick that keeps this short: 'IsBlock' and 'IsInline' are /construction/
-- interfaces, not traversal interfaces.  The parser calls @paragraph@,
-- @heading@, @list@… with the already-parsed children, so implementing those
-- two classes over a small intermediate type gives the document structure
-- without walking any AST.
--
-- Styling is a policy rather than a palette, so the module works in a plain
-- terminal and can be tested without a theme:
--
-- > renderMarkdownRaw plainStyle 80 "# Title"
-- > -- ["# Title"]
module Brick.Widgets.Markdown
  ( -- * Style
    MdStyle(..)
  , plainStyle

    -- * Rendering
  , renderMarkdownRaw
  , renderMarkdown
  , renderMarkdownPlain
  ) where

import Brick (Widget)
import Brick.Widgets.Core (txt, vBox)
import Brick.Widgets.TextWidth (graphemes, takeCells, textCells)
import Commonmark
import Data.Functor.Identity (Identity, runIdentity)
import Commonmark.Extensions (ColAlignment(..), HasPipeTable(..), pipeTableSpec)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T

-- ---------------------------------------------------------------- style

-- | How each Markdown construct becomes text.  A style never sees the AST;
-- it maps structure to a rendered string.
data MdStyle = MdStyle
  { msHeading  :: Int -> Text -> Text
  , msStrong   :: Text -> Text
  , msEm       :: Text -> Text
  , msCode     :: Text -> Text
  , msLink     :: Text -> Text -> Text
  , msBullet   :: Int -> Int -> Text
    -- ^ Nesting level and item number (0 = unordered).
  , msQuoteBar :: Text
  , msRule     :: Int -> Text
  , msIndent   :: Int -> Text
  }

-- | Structure with ASCII markers and no colour, so the module is usable and
-- testable without a terminal theme.
plainStyle :: MdStyle
plainStyle = MdStyle
  { msHeading  = \lvl t -> T.replicate lvl "#" <> " " <> t
  , msStrong   = \t -> "**" <> t <> "**"
  , msEm       = \t -> "*" <> t <> "*"
  , msCode     = \t -> "`" <> t <> "`"
  , msLink     = \t u -> t <> " (" <> u <> ")"
  , msBullet   = \_lvl n -> if n == 0 then "-" else T.pack (show n) <> "."
    -- 缩进由 msIndent 单独负责；这里再带一次会让嵌套列表多缩一层
  , msQuoteBar = "│ "
  , msRule     = \w -> T.replicate w "-"
  , msIndent   = \lvl -> T.replicate (lvl * 2) " "
  }

-- | A style that emits no markup at all: what 「copy this message」 should
-- produce.  Links keep their target in parentheses so no information is lost.
plainTextStyle :: MdStyle
plainTextStyle = plainStyle
  { msHeading = \_ t -> t
  , msStrong  = id
  , msEm      = id
  , msCode    = id
  }

-- ------------------------------------------------------- intermediate form

-- | A parsed inline.
data MdInline
  = InlText Text
  | InlStrong MdInline
  | InlEm MdInline
  | InlCode Text
  | InlLink MdInline Text
  | InlBreak
  deriving (Show)

newtype MdInlines = MdInlines { unInlines :: [MdInline] }

instance Semigroup MdInlines where
  MdInlines a <> MdInlines b = MdInlines (a <> b)

instance Monoid MdInlines where
  mempty = MdInlines []

instance Show MdInlines where
  show = show . unInlines

instance Rangeable MdInlines where
  ranged _ = id

instance HasAttributes MdInlines where
  addAttributes _ = id

-- | A parsed block, already reduced to the shape a terminal row needs.
data MdRow
  = RowHeading Int MdInlines
  | RowParagraph MdInlines
  | RowCodeBlock Text
  | RowQuote [MdRow]
  | RowBullet Int Int [MdRow]
    -- ^ Nesting level, item number (0 = unordered), and the item's blocks
    -- (a paragraph, possibly followed by a nested list).
  | RowTable [ColAlignment] [[MdInlines]]
    -- ^ Column alignments and the rows of cells (the first row is the header).
  | RowList [MdRow]
    -- ^ A whole list: its items are rows, but they are tight — no blank row
    -- belongs between them the way it does between blocks.
  | RowRule
  deriving (Show)

newtype MdDoc = MdDoc { unMdDoc :: [MdRow] }

instance Semigroup MdDoc where
  MdDoc a <> MdDoc b = MdDoc (a <> b)

instance Monoid MdDoc where
  mempty = MdDoc []

instance Show MdDoc where
  show = show . unMdDoc

instance Rangeable MdDoc where
  ranged _ = id

instance HasAttributes MdDoc where
  addAttributes _ = id

-- ------------------------------------------------------------ instances

instance IsInline MdInlines where
  lineBreak = MdInlines [InlBreak]
  softBreak = MdInlines [InlText " "]
  str t = MdInlines [InlText t]
  entity t = MdInlines [InlText t]
  escapedChar c = MdInlines [InlText (T.singleton c)]
  emph = \(MdInlines is) -> MdInlines [InlEm (foldInline is)]
  strong = \(MdInlines is) -> MdInlines [InlStrong (foldInline is)]
  link dest _title desc = case desc of
    MdInlines is -> MdInlines [InlLink (foldInline is) dest]
  image _ _ desc = desc
  code t = MdInlines [InlCode t]
  rawInline _ t = MdInlines [InlText t]

-- | Collapse an inline run into the single node a wrapper (emph/strong) holds.
foldInline :: [MdInline] -> MdInline
foldInline [i] = i
foldInline is  = InlText (concatInlines is)

instance IsBlock MdInlines MdDoc where
  paragraph ils = MdDoc [RowParagraph ils]
  plain ils = MdDoc [RowParagraph ils]
  thematicBreak = MdDoc [RowRule]
  blockQuote (MdDoc rows) = MdDoc [RowQuote rows]
  codeBlock _ t = MdDoc [RowCodeBlock t]
  heading lvl ils = MdDoc [RowHeading lvl ils]
  rawBlock _ t = MdDoc [RowCodeBlock t]
  referenceLinkDefinition _ _ = mempty
  list (BulletList _) _ items =
    MdDoc [RowList (zipWith (bullet 0) (repeat 0) items)]
  list (OrderedList start _ _) _ items =
    MdDoc [RowList (zipWith (bullet 0) [start ..] items)]

-- | Tables arrive through has-pipe-table, the same construction style as
-- 'IsBlock': no AST to walk.
instance HasPipeTable MdInlines MdDoc where
  pipeTable aligns headers rows = MdDoc [RowTable aligns (headers : rows)]

-- | Turn a parsed list item into a bulleted row.  An item holding a single
-- paragraph keeps its inlines (so styling survives); anything more complex
-- is flattened, which is what a one-row terminal list can show anyway.
bullet :: Int -> Int -> MdDoc -> MdRow
bullet lvl n (MdDoc rows) = RowBullet lvl n rows

-- ------------------------------------------------------------- rendering

-- | Flatten an inline run to text, ignoring styling (used where only the
-- content matters, e.g. inside a collapsed list item).
concatInlines :: [MdInline] -> Text
concatInlines = mconcat . map go
  where
    go (InlText t)     = t
    go (InlStrong i)   = go i
    go (InlEm i)       = go i
    go (InlCode t)     = t
    go (InlLink i _)   = go i
    go InlBreak        = " "

-- | Render one inline with the given style.
renderInline :: MdStyle -> MdInline -> Text
renderInline st = go
  where
    go (InlText t)   = t
    go (InlStrong i) = msStrong st (go i)
    go (InlEm i)     = msEm st (go i)
    go (InlCode t)   = msCode st t
    go (InlLink i u) = msLink st (go i) u
    go InlBreak      = "\n"

renderInlines :: MdStyle -> MdInlines -> Text
renderInlines st (MdInlines is) =
  T.replace "\x200B" "" (T.concat (map (renderInline st) is))

-- | Parse and render to rows of plain text.
--
-- Rows are returned in document order; a blockquote's rows are prefixed with
-- 'msQuoteBar'.  A parse error yields no rows rather than an exception.
renderMarkdownRaw :: MdStyle -> Int -> Text -> [Text]
renderMarkdownRaw st width input =
  -- commonmarkWith 返回 m (Either ParseError bl)；runIdentity 剥掉外层，
  -- 与 commonmark 自身的实现（runIdentity . parseCommonmarkWith）一致。
  case runIdentity
         (commonmarkWith
            ((defaultSyntaxSpec <> pipeTableSpec)
               :: SyntaxSpec Identity MdInlines MdDoc)
            "" (fixCjkEmphasis input)) of
    Left _     -> []
    Right doc  -> concatMap (wrapText width) (renderRows st width 0 (unMdDoc doc))

renderRows :: MdStyle -> Int -> Int -> [MdRow] -> [Text]
renderRows st width lvl rows =
  -- 块与块之间空一行，但末尾不留空行（"a\n\nb" → ["a","","b"]）
  intercalate [""] (map (renderRow st width lvl) rows)

renderRow :: MdStyle -> Int -> Int -> MdRow -> [Text]
renderRow st width lvl = \case
  RowHeading n ils  -> [msHeading st n (renderInlines st ils)]
  RowParagraph ils  -> [renderInlines st ils]
  RowCodeBlock t    -> T.lines t
  RowRule           -> [msRule st width]
  RowBullet n k rows ->
    let indent = msIndent st (lvl + n)
        marker = msBullet st (lvl + n) k
    in case rows of
         (RowParagraph ils : rest) ->
           (indent <> marker <> " " <> renderInlines st ils)
             : renderRows st width (lvl + n + 1) rest
         rest -> (indent <> marker)
                   : renderRows st width (lvl + n + 1) rest
  RowTable aligns rows -> renderTable st aligns rows
  RowList rows      -> concatMap (renderRow st width lvl) rows
  RowQuote rows     ->
    let bar r = msQuoteBar st <> r
    in map bar (renderRows st width (lvl + 1) rows)

-- | Render a table: measure every column in cells (not characters, so CJK
-- lines up), pad each cell to its column width honouring the alignment, and
-- put a rule under the header.
renderTable :: MdStyle -> [ColAlignment] -> [[MdInlines]] -> [Text]
renderTable st aligns rows = case cells of
  []              -> []
  (header : body) -> line header : rule : map line body
  where
    cells = map (map (renderInlines st)) rows
    ncols = maximum (0 : map length cells)
    widths = [ maximum (0 : map textCells [ r !! i | r <- cells, length r > i ])
             | i <- [0 .. ncols - 1] ]
    alignAt i = case drop i aligns of
      (a : _) -> a
      []      -> DefaultAlignedCol
    cellAt i r = if length r > i then r !! i else ""
    pad w a t =
      let gap = max 0 (w - textCells t)
      in case a of
           RightAlignedCol  -> T.replicate gap " " <> t
           CenterAlignedCol -> let l = gap `div` 2
                               in T.replicate l " " <> t <> T.replicate (gap - l) " "
           _                -> t <> T.replicate gap " "
    line r = "| " <> T.intercalate " | "
      [ pad (widths !! i) (alignAt i) (cellAt i r) | i <- [0 .. ncols - 1] ] <> " |"
    rule = "|" <> T.intercalate "|" [ T.replicate (w + 2) "-" | w <- widths ] <> "|"

-- | Render to a widget.
renderMarkdown :: MdStyle -> Int -> Text -> Widget n
renderMarkdown st width input =
  vBox (map txt (renderMarkdownRaw st width input))

-- | Render to plain text (the "copy this message" path).
renderMarkdownPlain :: Int -> Text -> Text
renderMarkdownPlain width input =
  T.intercalate "\n" (renderMarkdownRaw plainTextStyle width input)

-- | CommonMark requires the closing @**@ of emphasis not to be followed by a
-- letter or digit, and CJK characters count as letters.  So @这是**粗体**文字@
-- is /not/ emphasis under a conforming parser, which is not what a Chinese
-- writer means by it.
--
-- The fix is a zero-width space on each side of the delimiters: that
-- satisfies the "not adjacent to alphanumeric" rule without changing what the
-- reader sees.  The zero-width spaces are stripped again at render time.
fixCjkEmphasis :: Text -> Text
fixCjkEmphasis = insertBreaks
  where
    insertBreaks t = T.concat (go (T.unpack t))
    go [] = []
    go ('*' : '*' : rest)
      | Just (body, after) <- takeUntilClose rest =
          ["\x200B**"] <> map T.singleton body <> ["**\x200B"] <> go after
    go (c : cs) = T.singleton c : go cs
    takeUntilClose xs =
      let (body, rest) = breakOnClose xs
      in if null body then Nothing else Just (body, rest)
    breakOnClose [] = ([], [])
    breakOnClose ('*' : '*' : rest) = ([], rest)
    breakOnClose (c : cs) = let (b, r) = breakOnClose cs in (c : b, r)

-- | Greedy wrap at the given cell width, breaking on spaces where possible
-- and never inside a grapheme cluster.
wrapText :: Int -> Text -> [Text]
wrapText width t
  -- An empty row is a paragraph break, not "nothing to wrap": dropping it
  -- would silently glue two paragraphs together.
  | T.null t = [t]
  | width <= 0 = [t]
  | otherwise = go t
  where
    go s
      | T.null s = []
      | textCells s <= width = [s]
      | otherwise =
          let raw = takeCells width s
              -- A cluster wider than the whole budget must still consume
              -- input: advancing zero characters would recurse forever.
              (fits, dropped)
                | T.null raw =
                    let g = T.concat (take 1 (graphemes s))
                    in (g, T.drop (T.length g) s)
                | otherwise = (raw, T.drop (T.length raw) s)
          in case T.breakOn " " fits of
               -- No break opportunity in the fitted prefix: hard break.
               (_, "") -> fits : go dropped
               (line, _)
                 | T.null line -> go (T.stripStart s)
                 | otherwise   ->
                     T.stripEnd line
                       : go (T.stripStart (T.drop (T.length line) s))
