import qualified Data.Map                            as M
import           Data.Monoid
import           System.Exit
import           XMonad
import           XMonad.Actions.Promote
import           XMonad.Actions.Submap
import           XMonad.Actions.WindowBringer
import           XMonad.Hooks.DynamicLog
import           XMonad.Layout.Grid
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Spacing
import           XMonad.Layout.Spiral
import           XMonad.Layout.Tabbed
import           XMonad.Layout.WindowNavigation
-- import           XMonad.Layout.ZoomRow
import           XMonad.Prompt
import           XMonad.Prompt.RunOrRaise
import           XMonad.Prompt.Window
import qualified XMonad.StackSet                     as W
import           XMonad.Util.EZConfig
-- import XMonad.Actions.UpdatePointer
-- import XMonad.Actions.GroupNavigation
-- import XMonad.Actions.WindowGo
-- import XMonad.Actions.Search
-- import XMonad.Layout.Drawer

main = xmonad =<< xmobar myConfig where myConfig = defaultConfig {
         modMask            = mod1Mask -- Alt key
         -- modMask           = mod4Mask -- Win key
       , focusFollowsMouse  = True
       , terminal           = "termite"
       , workspaces         = (map show [1..9])
       , borderWidth        = 2
       -- , normalBorderColor  = "#262626"
       , normalBorderColor  = "#393939"
       , focusedBorderColor = "#ff8700"
       , keys               = ezKeys
       , layoutHook         = myLayout
       , manageHook         = myManageHook
       }

ezKeys = \c -> mkKeymap c $
    [ ("M-<Return>",   spawn $ terminal c)
    -- , ("M-;",          spawn "exe=`yeganesh -x` && eval \"exec $exe\"")
    , ("M-;",          runOrRaisePrompt myXPConfig)
    , ("M-q",          kill)
    , ("M-.",          sendMessage NextLayout)
    , ("M-,",          sendMessage FirstLayout)
    , ("M-<Tab>",      windows W.focusDown)
    , ("M-S-<Tab>",    windows W.focusUp)
    , ("M-j",          sendMessage $ Go D)
    , ("M-S-j",        sendMessage $ Swap D)
    , ("M-k",          sendMessage $ Go U)
    , ("M-S-k",        sendMessage $ Swap U)
    , ("M-h",          sendMessage $ Go L)
    , ("M-S-h",        sendMessage $ Swap L)
    , ("M-l",          sendMessage $ Go R)
    , ("M-S-l",        sendMessage $ Swap L)
    , ("M-m",          windows W.focusMaster)
    , ("M-S-m",        promote) -- swapMaster
    , ("M-x",          sendMessage $ Toggle MIRROR)
    , ("M-f",          sendMessage $ Toggle FULL)
    , ("M-<Space>",    windowPromptGoto myXPConfig)
    , ("M-S-<Space>",  windowPromptBring myXPConfig)
    , ("M-S-,",        sendMessage Shrink)
    , ("M-S-.",        sendMessage Expand)
    , ("M-t",          withFocused $ windows . W.sink)
    , ("M-=",          sendMessage (IncMasterN 1))
    , ("M--",          sendMessage (IncMasterN (-1)))
    , ("M-S-<Esc>",    io (exitWith ExitSuccess))
    , ("M-<Esc>",      spawn "xmonad --recompile && xmonad-restart")
    -- , ("M-S-=",        sendMessage zoomIn)
    -- , ("M-S--",        sendMessage zoomOut)
    -- , ("M-S-0",        sendMessage zoomReset)
    ]
    ++
    [(m ++ (show k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces c) [1..9]
        , (f, m) <- [(W.greedyView, "M-"), (W.shift, "M-S-")]]

myLayout = id
         . smartBorders
         . mkToggle (single FULL)
         . windowNavigation
         $
         (mkToggle (single MIRROR) . smartSpacing 2)
            ( tiled ||| Grid ||| spiral (6/7) )
         ||| tabbed shrinkText myTabConfig
  where tiled = Tall nmaster delta ratio
        nmaster = 1
        ratio   = 0.61
        delta   = 1/50

myManageHook = composeAll
    [ className =? "MPlayer"    --> doFloat
    , className =? "Gimp"       --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    ]

-- | Theme configuration
myXPConfig = defaultXPConfig
           { position = Top
           , historySize = 10000
           , defaultText = "\\"
           }

myTabConfig = defaultTheme
