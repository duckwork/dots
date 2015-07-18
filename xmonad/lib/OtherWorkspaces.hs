-- Do stuff on other workspaces.
module OtherWorkspaces where

import           XMonad
import           XMonad.Actions.CopyWindow
import qualified XMonad.StackSet           as W
import Control.Arrow ((&&&))

-- | Do an action on an arbitrary workspace
onWorkspace :: WorkspaceId -> (WindowSet -> WindowSet)
            -> WindowSet -> WindowSet
onWorkspace t f ss = W.view (W.currentTag ss) . f . W.view t $ ss

-- | Perform a function on all windows in current workspace.
-- Stolen from XMonad.Actions.WithAll -- but it's pure
withAll :: (a -> W.StackSet i l a sid sd -> W.StackSet i l a sid sd)
        -> t -> W.StackSet i l a sid sd -> W.StackSet i l a sid sd
withAll f w ss =
              let all' = W.integrate'
                       . W.stack
                       . W.workspace
                       . W.current
                       $ ss
               in foldr f ss all'

-- | Copy all windows from one workspace to the current one
copyAllFrom :: WorkspaceId -> WindowSet -> WindowSet
copyAllFrom t ss | t == W.currentTag ss = ss
                 | otherwise = onWorkspace t
                   (withAll
                     (\w -> copyWindow w (W.currentTag ss)) ss) ss

-- | Delete all a workspace's windows from the current workspace
-- (opposite of copyAllFrom)
delAllOf :: WorkspaceId -> WindowSet -> WindowSet
delAllOf t ss | t == W.currentTag ss = ss
              | otherwise = onWorkspace t
                (withAll
                  (\w -> delWinFromWorkspace w (W.currentTag ss)) ss) ss

-- | Delete a window from a specific workspace
delWinFromWorkspace :: Window -> WorkspaceId -> WindowSet -> WindowSet
delWinFromWorkspace w t = onWorkspace t $
                              W.modify Nothing (W.filter (/= w))

-- | If windows from workspace exist, delete them from current view;
-- else copy all to view
toggleView :: WorkspaceId -> WindowSet -> WindowSet
toggleView t ss | t == W.currentTag ss = ss
                | otherwise = onWorkspace t
                    (withAll
                      (\w -> delOrCopyWin w (W.currentTag ss)) ss) ss
  where delOrCopyWin w' t' = if t' `elem` (wsWithCopies w' ss)
                                -- The problem is DEF in this test --
                                -- it always returns False apparently.
                           then delWinFromWorkspace w' (W.currentTag ss)
                           else copyWindow w' (W.currentTag ss)

-- | A list of hidden workspaces containing a copy of the focused window.
wsWithCopies :: Window -> WindowSet -> [WorkspaceId]
wsWithCopies w ss = copiesOfOn w (taggedWindows $ W.hidden ss)

-- | Get a list of tuples (tag, [Window]) for each workspace.
taggedWindows :: [W.Workspace i l a] -> [(i, [a])]
taggedWindows = map $ W.tag &&& W.integrate' . W.stack

-- | Get tags with copies of the focused window (if present.)
copiesOfOn :: (Eq a) => a -> [(i, [a])] -> [i]
copiesOfOn foc tw = hasCopyOf foc
  where hasCopyOf f = map fst $ filter ((f `elem` ) . snd) tw
