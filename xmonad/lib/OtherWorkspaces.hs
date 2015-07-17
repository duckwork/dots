-- Do stuff on other workspaces.
module OtherWorkspaces where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Actions.CopyWindow

-- | Do an action on an arbitrary workspace
onWorkspace :: WorkspaceId -> (WindowSet -> WindowSet)
            -> WindowSet -> WindowSet
onWorkspace wsid f w = W.view (W.currentTag w) . f . W.view wsid $ w

-- | Perform a function on all windows in a workspace.
-- Stolen from XMonad.Actions.WithAll -- but it's pure
withAll :: (a -> W.StackSet i l a sid sd -> W.StackSet i l a sid sd)
        -> t -> W.StackSet i l a sid sd -> W.StackSet i l a sid sd
withAll f w ws =
              let all' = W.integrate' . W.stack . W.workspace . W.current $ ws
               in foldr f ws all'

-- | Copy all windows from one workspace to the current one
copyAllFrom :: WorkspaceId -> WindowSet -> WindowSet
copyAllFrom wsid ws = onWorkspace wsid
                      (withAll (\w -> copyWindow w (W.currentTag ws)) ws) ws

-- | Delete all copies of windows on a workspace from current workspace
-- (opposite of copyAllFrom)
delAllOf :: WorkspaceId -> WindowSet -> WindowSet
delAllOf wsid ws = onWorkspace wsid
                     (withAll
                      (\w -> delWinFromWorkspace w (W.currentTag ws)) ws) ws
                   where
                     delWinFromWorkspace win wid = onWorkspace wid $
                       W.modify Nothing (W.filter (/= win))

toggleView :: WorkspaceId -> WindowSet -> WindowSet
toggleView wsid ws = let cw = W.currentTag ws
                      in onWorkspace wsid (delAllOf cw) ws
