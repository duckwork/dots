{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module MyWmii where

import           XMonad                          hiding ((|||))

import qualified XMonad.Layout.Groups            as G
import           XMonad.Layout.Groups.Examples
import           XMonad.Layout.Groups.Helpers

import           XMonad.Layout.LayoutCombinators
import           XMonad.Layout.MessageControl
import           XMonad.Layout.Named
import           XMonad.Layout.Renamed
import           XMonad.Layout.Simplest
import           XMonad.Layout.Tabbed

wmii s t = G.group innerLayout zoomRowG
  where tall = named "tall" $ Tall 0 (1/50) (1/2)
        tabs = named "tabs" $ Simplest
        full = named "full" $ Full

        innerLayout = id
                    -- $ renamed [CutWordsLeft 3]
                    $ addTabs s t
                    $ ignore NextLayout
                    $ ignore (JumpToLayout "") $ unEscape
                      $ tall ||| tabs ||| full

-- | Increase the width of the focused group
zoomGroupIn :: X ()
zoomGroupIn = zoomColumnIn

-- | Decrease the size of the focused group
zoomGroupOut :: X ()
zoomGroupOut = zoomColumnOut

-- | Reset the size of the focused group to the default
zoomGroupReset :: X ()
zoomGroupReset = zoomColumnReset

-- | Toggle whether the currently focused group should be maximized
-- whenever it has focus.
toggleGroupFull :: X ()
toggleGroupFull = toggleGroupFull

-- | Rotate the layouts in the focused group.
groupToNextLayout :: X ()
groupToNextLayout = sendMessage $ escape NextLayout

-- | Switch the focused group to the \"maximized\" layout.
groupToFullLayout :: X ()
groupToFullLayout = sendMessage $ escape $ JumpToLayout "Full"

-- | Switch the focused group to the \"tabbed\" layout.
groupToTabbedLayout :: X ()
groupToTabbedLayout = sendMessage $ escape $ JumpToLayout "Tabs"

-- | Switch the focused group to the \"column\" layout.
groupToVerticalLayout :: X ()
groupToVerticalLayout = sendMessage $ escape $ JumpToLayout "Column"
