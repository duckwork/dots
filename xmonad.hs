import           Data.List
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
import           XMonad.Util.Run
-- import XMonad.Actions.CopyWindow
-- import XMonad.Actions.UpdatePointer
-- import XMonad.Actions.WindowGo
-- import XMonad.Actions.Search
-- import XMonad.Layout.Drawer

main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

myConfig = defaultConfig {
         modMask            = mod1Mask -- Alt key
         -- modMask           = mod4Mask -- Win key
       , focusFollowsMouse  = True
       , terminal           = "termite"
       , workspaces         = (map show [1..9])
       , borderWidth        = 2
       , normalBorderColor  = myBg
       , focusedBorderColor = red'
       , keys               = ezKeys
       -- , logHook            = dynamicLogWithPP $ myXmobarPP
       , layoutHook         = myLayout
       , manageHook         = myManageHook
       }

toggleStrutsKey XConfig {XMonad.modMask = modm} = (modm, xK_b)

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
    , ("M-`",          setLayout $ XMonad.layoutHook c)
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
    ] ++
    [(m ++ (show k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces c) [1..9]
        , (f, m) <- [(W.greedyView, "M-"), (W.shift, "M-S-")]]

myLayout = id
         . smartBorders
         . mkToggle (single FULL)
         . windowNavigation
         $
         (mkToggle (single MIRROR) . smartSpacing 2)
            ( myTall ||| Grid ||| mySpiral )
         ||| tabbed shrinkText myTabConfig
  where myTall = Tall nmaster delta ratio
        nmaster = 1
        ratio   = 1/2
        delta   = 1/50
        mySpiral = spiral (6/7)

myManageHook = composeAll
    [
      className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    ]

-- | Theme configuration
-- Apprentice colorscheme: https://github.com/romainl/Apprentice
myBg     = "#262626"
myFg     = "#bcbcbc"
black    = "#1c1c1c"
black'   = "#444444"
red      = "#af5f5f"
red'     = "#ff8700"
green    = "#5f875f"
green'   = "#87af87"
yellow   = "#87875f"
yellow'  = "#ffffaf"
blue     = "#5f87af"
blue'    = "#8fafd7"
magenta  = "#5f5f87"
magenta' = "#8787af"
cyan     = "#5f8787"
cyan'    = "#5fafaf"
white    = "#6c6c6c"
white'   = "#ffffff"

myXPConfig = defaultXPConfig
           {
             position        = Top
           , bgColor         = myBg
           , fgColor         = myFg
           , borderColor     = myBg
           , fgHLight        = black
           , bgHLight        = green'
           , alwaysHighlight = True
           , historySize     = 10000
           , height          = 12
           , searchPredicate = isInfixOf
           }

myTabConfig = defaultTheme
            {
              activeColor         = myBg
            , inactiveColor       = black
            , activeBorderColor   = myBg
            , inactiveBorderColor = black'
            , activeTextColor     = red'
            , inactiveTextColor   = black'
            , urgentColor         = red
            , urgentBorderColor   = red'
            , urgentTextColor     = white'
            , decoHeight          = 14
            }

myBar = "xmobar"

myPP = defaultPP
           {
             ppCurrent         = xmobarColor red' myBg
           , ppVisible         = xmobarColor cyan myBg
           , ppHidden          = id
           , ppHiddenNoWindows = const "_"
           , ppUrgent          = xmobarColor "red" "yellow" . wrap "!" "!"
           , ppSep             = " | "
           , ppWsSep           = ""
           , ppTitle           = xmobarColor green' myBg . shorten 40
             -- ^ add `. ('}':)` when you figure out the xmobar escaping thing
           , ppLayout          = \s -> xmobarColor yellow "" $
                                 case s of -- TODO; use regexes?
                                   "SmartSpacing 2 Tall"          -> "|"
                                   "Mirror SmartSpacing 2 Tall"   -> "-,"
                                   "SmartSpacing 2 Grid"          -> "+"
                                   "Mirror SmartSpacing 2 Grid"   -> "="
                                   "SmartSpacing 2 Spiral"        -> "@"
                                   "Mirror SmartSpacing 2 Spiral" -> "e"
                                   "Tabbed Simplest"              -> "t"
                                   "Full"                         -> "_"
                                   _                              -> s
           , ppOrder           = id
           , ppExtras          = []
           -- , ppOutput = hPutStrLn h
           }
