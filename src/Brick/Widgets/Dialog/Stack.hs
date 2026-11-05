-- | A stack of modal dialogs.
--
-- Brick's "Brick.Widgets.Dialog" provides a /single/ dialog: its title, body,
-- buttons, rendering, and focus handling.  What it does not provide is a
-- /stack/ of them -- the part every application with more than one modal
-- panel ends up writing: which dialog is on top, who owns the keyboard, and
-- what "close" means when the stack is empty.
--
-- This module is deliberately minimal and polymorphic: it never looks at the
-- dialog payload, so it composes with 'Brick.Widgets.Dialog.Dialog' or with
-- anything else an application uses as a dialog.
--
-- == Usage
--
-- > data MyDialog = Confirm | PickFile FilePath   -- your dialog type
-- >
-- > data AppState = AppState { appDialogs :: DialogStack MyDialog, ... }
-- >
-- > onEnter st = case top (appDialogs st) of
-- >   Nothing -> pure st
-- >   Just d  -> handleDialog d st
-- >
-- > onClose st = st { appDialogs = pop (appDialogs st) }
module Brick.Widgets.Dialog.Stack
  ( -- * The stack type
    DialogStack

    -- * Manipulating the stack
  , emptyStack
  , push
  , pop
  , top

    -- * Querying the stack
  , isNull
  ) where

-- | A stack of dialogs.  The head is the topmost dialog and owns keyboard
-- input until it is popped.
type DialogStack a = [a]

-- | The empty stack: no dialog open.
emptyStack :: DialogStack a
emptyStack = []

-- | Open a dialog on top of the stack.
push :: a
     -- ^ The dialog to open
     -> DialogStack a
     -- ^ The current stack
     -> DialogStack a
push = (:)

-- | Close the topmost dialog.  Popping an empty stack is a no-op, so callers
-- can pop unconditionally on an "escape" keypress.
pop :: DialogStack a -> DialogStack a
pop (_ : rest) = rest
pop []         = []

-- | The topmost dialog, if any.
top :: DialogStack a -> Maybe a
top (d : _) = Just d
top []      = Nothing

-- | Whether the stack is empty (no dialog open).
isNull :: DialogStack a -> Bool
isNull = null
