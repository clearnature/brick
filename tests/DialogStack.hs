module DialogStack (main) where

import Brick.Widgets.Dialog.Stack
import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)

-- | Pushing then looking at the top always yields the pushed dialog.
prop_pushTop :: Int -> [Int] -> Bool
prop_pushTop x s = top (push x s) == Just x

-- | Pushing grows the stack by exactly one.
prop_pushGrows :: Int -> [Int] -> Bool
prop_pushGrows x s = length (push x s) == length s + 1

-- | pop is the left inverse of push.
prop_popPush :: Int -> [Int] -> Bool
prop_popPush x s = pop (push x s) == s

-- | Popping an empty stack is a no-op (callers rely on this when Escape is
-- pressed with nothing open).
prop_popEmpty :: Bool
prop_popEmpty = pop (emptyStack :: DialogStack Int) == emptyStack

-- | The empty stack reports itself as empty.
prop_emptyIsNull :: Bool
prop_emptyIsNull = isNull (emptyStack :: DialogStack Int)

-- | A stack with anything pushed onto it is never empty.
prop_pushNotEmpty :: Int -> [Int] -> Bool
prop_pushNotEmpty x s = not (isNull (push x s))

-- | top of the empty stack is Nothing.
prop_topEmpty :: Bool
prop_topEmpty = top (emptyStack :: DialogStack Int) == Nothing

main :: IO Bool
main = and <$> sequenceA
    [ isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 }
                  prop_pushTop
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 }
                  prop_pushGrows
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 }
                  prop_popPush
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 }
                  prop_popEmpty
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 }
                  prop_emptyIsNull
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 }
                  prop_pushNotEmpty
    , isSuccess <$> quickCheckWithResult stdArgs { maxSuccess = 100 }
                  prop_topEmpty
    ]
