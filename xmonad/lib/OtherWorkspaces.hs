-- Do stuff on other workspaces.
module OtherWorkspaces where

import XMonad
import XMonad.StackSet
import XMonad.Actions.CopyWindow

-- | Do an action on an arbitrary workspace
onWorkspace :: WorkspaceId -> (WindowSet -> WindowSet)
            -> WindowSet -> WindowSet
onWorkspace wsid f w = view (currentTag w) . f . view wsid $ w

-- | Perform a function on all windows in a workspace.
-- Stolen from XMonad.Actions.WithAll -- but it's pure
withAll :: (a -> StackSet i l a sid sd -> StackSet i l a sid sd)
        -> t -> StackSet i l a sid sd -> StackSet i l a sid sd
withAll f w ws =
              let all' = integrate' . stack . workspace . current $ ws
               in foldr f ws all'

-- | Copy all windows from one workspace to the current one
copyAllFrom :: WorkspaceId -> WindowSet -> WindowSet
copyAllFrom wsid ws = onWorkspace wsid
                      (withAll (\w -> copyWindow w (currentTag ws)) ws) ws
