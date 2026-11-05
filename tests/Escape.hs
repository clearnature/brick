module Escape (main) where

import Brick.Escape
import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)

-- | The priority order is the whole contract: the first matching flag wins,
-- and with nothing active Escape means "deny the pending approval".
prop_resolvePriority :: Bool -> Bool -> Bool -> Bool -> Bool
prop_resolvePriority sel stk shl cmp =
    resolveEscape (EscFlags sel stk shl cmp) == expected
  where
    expected
      | sel       = EscClearSelection
      | stk       = EscPopDialog
      | shl       = EscExitShell
      | cmp       = EscCloseComplete
      | otherwise = EscApprovalDeny

-- | A second press inside the window fires only when the interrupt was
-- armed; a first press (not armed) never fires.
prop_escPressFiresIffArmed :: Bool -> Bool
prop_escPressFiresIffArmed armed = fst (escPress True armed) == armed

-- | A press outside the window never fires, armed or not.
prop_escPressQuietOutsideWindow :: Bool -> Bool
prop_escPressQuietOutsideWindow armed = not (fst (escPress False armed))

-- | Firing disarms, so a third press starts over rather than firing again.
prop_escPressDisarmsOnFire :: Bool
prop_escPressDisarmsOnFire = not (snd (escPress True True))

-- | The first press inside the window arms the interrupt (it does not fire).
-- This is the other half of the double-press protocol: press once to arm,
-- press again within the window to fire.
prop_escPressArmsOnFirstPress :: Bool
prop_escPressArmsOnFirstPress = snd (escPress True False)

-- | A press outside the window does not fire, but it /does/ (re-)arm: the
-- next press starts a fresh double-press window.  The caller clears its
-- timestamp on fire, which is what makes the third press a fresh first press.
prop_escPressOutsideWindowRearms :: Bool
prop_escPressOutsideWindowRearms = escPress False False == (False, True)

-- | With nothing running, Escape neither fires nor arms: three stray presses
-- must not cancel work that starts later.
prop_escWiringIdle :: Bool -> Bool -> Bool
prop_escWiringIdle withinWindow armed =
    escWiring False withinWindow armed == (False, False)

-- | While running, the wiring decision is exactly 'escPress'.
prop_escWiringRunning :: Bool -> Bool -> Bool
prop_escWiringRunning withinWindow armed =
    escWiring True withinWindow armed == escPress withinWindow armed

main :: IO Bool
main = and <$> sequenceA
    [ isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 }
                  prop_resolvePriority
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 }
                  prop_escPressFiresIffArmed
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 }
                  prop_escPressQuietOutsideWindow
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 }
                  prop_escPressDisarmsOnFire
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 }
                  prop_escPressArmsOnFirstPress
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 }
                  prop_escPressOutsideWindowRearms
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 }
                  prop_escWiringIdle
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 }
                  prop_escWiringRunning
    ]
