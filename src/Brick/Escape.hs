-- | Escape-key arbitration for layered TUI applications.
--
-- Escape is rarely just "close something".  In a TUI with several layers of
-- transient state it has to decide, per keypress, which layer the user meant:
-- an active text selection, an open dialog, a shell prompt, an autocomplete
-- popup, or an approval card.  Getting that order wrong produces the classic
-- bug where Escape dismisses a dialog /and/ cancels a running turn.
--
-- This module is the pure, testable half of that decision: given which
-- layers are active ('EscFlags'), say what Escape should do ('EscOutcome').
-- It does not touch the event monad, so it is trivially unit-testable -- and
-- it is the part every application ends up re-deriving.
--
-- == Usage
--
-- > case resolveEscape (EscFlags hasSel hasDialog hasShell hasComplete) of
-- >   EscClearSelection -> clearSelection
-- >   EscPopDialog      -> popDialog
-- >   EscExitShell      -> leaveShell
-- >   EscCloseComplete  -> closeComplete
-- >   EscApprovalDeny   -> denyApproval
--
-- == Double-press interruption
--
-- Interrupting a running turn is deliberately /not/ a plain Escape: it needs
-- a second press inside a short window, so a stray Escape (closing a popup,
-- say) never cancels work.  'escPress' and 'escWiring' implement that
-- protocol; 'escWiring' additionally refuses to arm while nothing is running,
-- which is what keeps three presses from firing twice.
module Brick.Escape
  ( -- * Inputs and outcomes
    EscFlags(..)
  , EscOutcome(..)

    -- * Priority resolution
  , resolveEscape

    -- * Double-press interruption
  , escPress
  , escWiring
  ) where

-- | Signals the escape key consults, in priority order.
data EscFlags = EscFlags
  { efSelection :: Bool
    -- ^ a text selection is active: copy/clear it first
  , efStack     :: Bool
    -- ^ a dialog is open: pop it
  , efShell     :: Bool
    -- ^ shell mode is active: exit it
  , efComplete  :: Bool
    -- ^ an autocomplete popup is open: close it
  } deriving (Show, Eq)

-- | What an Escape press means, given 'EscFlags'.
data EscOutcome
  = EscClearSelection
  | EscPopDialog
  | EscExitShell
  | EscCloseComplete
  | EscApprovalDeny
    -- ^ Escape on an approval card means reject.  Applications that treat
    -- Escape there as "ignore the prompt" should map this to their own
    -- no-op instead of denying.
  deriving (Show, Eq)

-- | Resolve Escape against the active layers.  The first matching flag wins,
-- so the order of the guards /is/ the priority order.
resolveEscape :: EscFlags -> EscOutcome
resolveEscape f
  | efSelection f = EscClearSelection
  | efStack f     = EscPopDialog
  | efShell f     = EscExitShell
  | efComplete f  = EscCloseComplete
  | otherwise     = EscApprovalDeny

-- | One Escape press over a double-press window.
--
-- Takes @(withinWindow, armed)@ and returns @(fired, armed')@: a press inside
-- the window while armed is the interrupting second press.  The caller disarms
-- on fire and on any other key, so the state machine stays two-valued.
escPress :: Bool
         -- ^ Is this press inside the window opened by the arming press?
         -> Bool
         -- ^ Is the interrupt armed?
         -> (Bool, Bool)
         -- ^ @(fired, still armed)@
escPress withinWindow armed = (fired, armed')
  where
    fired  = armed && withinWindow
    -- 任何一次未 fire 的按键都会留下 arm（含窗口外的「重新开始」），
    -- 只有 fire 本身才清空。这样：首击 arm → 窗口内二击 fire →
    -- 时间戳被调用方清空，第三击看到 withinWindow=False 便重新 arm。
    armed' = not (withinWindow && armed)

-- | Escape decision including "is there anything to interrupt?".
--
-- Only a running turn can be interrupted, and an idle press neither fires nor
-- leaves an arm behind.  Returns @(interrupt, keepArmed)@; the caller maps
-- @keepArmed@ to its own timestamp field (arm with the current time, or clear
-- it).
--
-- Clearing the timestamp on fire is what makes the third press a fresh first
-- press rather than an immediate second fire.
escWiring :: Bool
          -- ^ Is a turn running?
          -> Bool
          -- ^ Is this press inside the window?
          -> Bool
          -- ^ Is the interrupt armed?
          -> (Bool, Bool)
          -- ^ @(interrupt, keep armed)@
escWiring running withinWindow armed
  | not running = (False, False)
  | otherwise   = escPress withinWindow armed
