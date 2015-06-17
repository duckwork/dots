import XMonad
import XMonad.Actions.WindowBringer
import XMonad.Actions.DwmPromote
-- import XMonad.Actions.UpdatePointer
-- import XMonad.Actions.WindowGo
-- import XMonad.Actions.Search

-- import XMonad.Layout.WindowArranger
-- import XMonad.Layout.WindowNavigation
import XMonad.Layout.Tabbed
import XMonad.Layout.Accordion
import XMonad.Layout.Grid
-- import XMonad.Layout.MouseResizableTile

import XMonad.Hooks.DynamicLog

-- import XMonad.Prompt
-- import XMonad.Prompt.AppendFile
-- import XMonad.Prompt.RunOrRaise
-- import XMonad.Prompt.Window

import Data.Monoid
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

main = xmonad =<< xmobar myConfig
  where myConfig = defaultConfig {
         modMask            = mod1Mask -- Alt key
         -- modMask           = mod4Mask -- Win key
       , focusFollowsMouse  = True
       , terminal           = "termite"
       , workspaces         = (map show [1..9])
       , borderWidth        = 2
       , normalBorderColor  = "#262626"
       , focusedBorderColor = "#ff8700"
       , keys               = myKeys
       , layoutHook         = myLayout
       , manageHook         = myManageHook
       }

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [
      ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modm,               xK_p     ), spawn
                            "exe=`yeganesh -x` && eval \"exec $exe\"")
    , ((modm,               xK_c     ), kill)
    , ((modm,               xK_space ), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space ), setLayout $
                            XMonad.layoutHook conf)
    , ((modm,               xK_n     ), refresh)
    , ((modm,               xK_Tab   ), windows W.focusDown)
    , ((modm,               xK_j     ), windows W.focusDown)
    , ((modm,               xK_k     ), windows W.focusUp)
    , ((modm,               xK_m     ), windows W.focusMaster)
    , ((modm,               xK_Return), dwmpromote)
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown)
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp)
    , ((modm .|. shiftMask, xK_g     ), gotoMenu)
    , ((modm .|. shiftMask, xK_b     ), bringMenu)
    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm,               xK_l     ), sendMessage Expand)
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    , ((modm,               xK_comma ), sendMessage (IncMasterN 1))
    , ((modm,               xK_period), sendMessage (IncMasterN (-1)))
    -- , ((modm,               xK_b     ), sendMessage ToggleStruts)
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modm,               xK_q     ), spawn
                            "xmonad --recompile; xmonad-restart")
    ]
    ++
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ]

myLayout = tiled ||| Mirror tiled ||| simpleTabbed ||| Accordion ||| Grid
  where
      tiled = Tall nmaster delta ratio
      nmaster = 1
      ratio   = 0.61
      delta   = 1/50

myManageHook = composeAll
    [ className =? "MPlayer"    --> doFloat
    , className =? "Gimp"       --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    ]
