module DwmActions where

import           Data.List                 (intersect)
import           XMonad
import           XMonad.Actions.CopyWindow
import qualified XMonad.StackSet           as W

-- | Do an action on an arbitrary workspace
viewing :: (Eq s, Eq i)
        => i -> (W.StackSet i l a s sd -> W.StackSet i l a s sd)
        -> W.StackSet i l a s sd -> W.StackSet i l a s sd
viewing t f ss = W.view (W.currentTag ss) $ f $ W.view t ss

-- | Perform a function on all windows in current workspace.
-- Stolen from XMonad.Actions.WithAll -- but it's pure
withAll :: (a -> W.StackSet i l a sid sd -> W.StackSet i l a sid sd)
        -> W.StackSet i l a sid sd -> W.StackSet i l a sid sd
withAll f ss =
  let all' = W.integrate' . W.stack . W.workspace . W.current $ ss
   in foldr f ss all'

withAllOn :: (Eq s, Eq i)
          => i -> (a -> W.StackSet i l a s sd -> W.StackSet i l a s sd)
          -> W.StackSet i l a s sd -> W.StackSet i l a s sd
withAllOn t = viewing t . withAll

-- | Copy all windows from one workspace to the current one
viewTag :: (Eq s, Eq a, Eq i)
            => i -> W.StackSet i l a s sd -> W.StackSet i l a s sd
viewTag t ss | t == cur  = ss
             | otherwise = withAllOn t (\w -> copyWindow w cur) ss
  where cur = W.currentTag ss

-- | Delete all of a workspace's windows from the current workspace
unviewTag :: (Eq s, Eq a, Eq i)
          => i -> W.StackSet i l a s sd -> W.StackSet i l a s sd
unviewTag t ss | t == cur  = ss
               | otherwise = withAllOn t (\w -> delFrom w cur) ss
  where cur = W.currentTag ss
        delFrom w t' = viewing t' $ W.modify Nothing (W.filter (/= w))

toggleView :: (Eq s, Eq a, Eq i)
           => i -> W.StackSet i l a s sd -> W.StackSet i l a s sd
toggleView t ss =
  if not . null $ intersect (wsOn t ss) (wsOn cur ss)
  then unviewTag t ss
  else viewTag t ss
    where
      cur = W.currentTag ss

wsOn t' ss' = W.index . W.view t' $ ss'

toggleViewEmpty :: (Eq s, Eq a, Eq i)
                => i -> W.StackSet i l a s sd -> W.StackSet i l a s sd
toggleViewEmpty t ss | t == cur         = ss
                     | null $ wsOn t ss = W.view t ss
                     | otherwise        = toggleView t ss
                     where cur = W.currentTag ss
