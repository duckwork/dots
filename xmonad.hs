-- vim:fdm=marker
-- Imports {{{
import           Data.List
import qualified Data.Map                            as M
import           Data.Monoid
import           System.Exit
import           XMonad
import           XMonad.Actions.Commands
import           XMonad.Actions.CopyWindow
import           XMonad.Actions.CycleWS
import           XMonad.Actions.Promote
import           XMonad.Actions.Submap
import           XMonad.Actions.WindowBringer
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.FadeInactive
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.Accordion
import           XMonad.Layout.Grid
import           XMonad.Layout.LimitWindows
import           XMonad.Layout.Mosaic
import           XMonad.Layout.MouseResizableTile
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Spacing
import           XMonad.Layout.Spiral
import           XMonad.Layout.Tabbed
import           XMonad.Layout.WindowNavigation
import           XMonad.Prompt
import           XMonad.Prompt.RunOrRaise
import           XMonad.Prompt.Window
import qualified XMonad.StackSet                     as W
import           XMonad.Util.EZConfig
import           XMonad.Util.Run
-- import XMonad.Actions.WindowGo
-- import XMonad.Actions.Search
import           XMonad.Layout.Reflect
-- }}}
-- My types, functions, etc. {{{
data Colorscheme = -- {{{
     Colorscheme { fg       :: String
                 , bg       :: String
                 , black    :: String
                 , red      :: String
                 , green    :: String
                 , yellow   :: String
                 , blue     :: String
                 , magenta  :: String
                 , cyan     :: String
                 , white    :: String
                 , black'   :: String
                 , red'     :: String
                 , green'   :: String
                 , yellow'  :: String
                 , blue'    :: String
                 , magenta' :: String
                 , cyan'    :: String
                 , white'   :: String
                 }
-- }}}
-- }}}
-- Main {{{
main = do
    h <- spawnPipe myBar
    xmonad defaultConfig { modMask            = myModMask
                         , focusFollowsMouse  = True
                         , terminal           = "termite"
                         , workspaces         = myWS
                         , borderWidth        = 2
                         , normalBorderColor  = bg myCS
                         , focusedBorderColor = red' myCS
                         , keys               = myKeymap
                         , logHook            = myLogHook h
                         , layoutHook         = myLayoutHook
                         , manageHook         = myManageHook
                         , handleEventHook    = myHandleEventHook
                         , startupHook        = myStartupHook
                         }
-- }}}
-- {{{ Hooks = [ Layout, Log, Manage, HandleEvents, Startup ]
myWS = [ "web" -- web browser
       , "yak" -- communication (irc, email)
       , "txt" -- coding + writing
       , "vid" -- media ('vid' > 'med')
       , "dir" -- file manager
       ] ++ map show [6..9]

myLayoutHook = avoidStruts
             . smartBorders
             . mkToggle (single FULL)
             . windowNavigation
             $ ( -- ^ These modify every layout listed
                 mkToggle (single MIRROR)
               . mkToggle (single REFLECTX)
               . mkToggle (single REFLECTY)
                 $ -- ^ These ONLY modify the layouts here:
                     mouseResizableTile { nmaster = 1
                                        , masterFrac = 1/2
                                        , fracIncrement = 1/50
                                        }
                 ||| limitSlice 4 (Mirror Accordion)
                 ||| spiral (6/7)
                 ||| mosaic 2 [3,2]
             ) |||
               tabbed shrinkText myTabConfig

myLogHook h = do
    fadeInactiveLogHook 0.7
    copies <- wsContainingCopies
    let check ws | ws `elem` copies = xmobarColor (red myCS) "" $ ws
                 | otherwise = ws
     in dynamicLogWithPP myPP { ppHidden = check, ppOutput = hPutStrLn h }

myManageHook = composeAll
    [
      className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    ]
    <+> manageDocks

myHandleEventHook = docksEventHook

myStartupHook = return ()
-- }}}
-- {{{ Key & Mouse bindings
myModMask = mod1Mask        -- Alt
-- myModMask = mod4Mask        -- Windows key
myKeymap = \c -> mkKeymap c $
    [ ("M-<Return>",   spawn $ terminal c)
    -- , ("M-;",          spawn "exe=`yeganesh -x` && eval \"exec $exe\"")
    , ("M-;",          runOrRaisePrompt myPrompt)
    , ("M-S-r",        myCommands >>= runCommand) -- TODO: change binding
    , ("M-q",          kill1)
    , ("M-b",          sendMessage ToggleStruts)
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
    , ("M-C-j",        sendMessage ExpandSlave)
    , ("M-C-k",        sendMessage ShrinkSlave)
    , ("M-C-h",        sendMessage Shrink)
    , ("M-C-l",        sendMessage Expand)
    , ("M-<Down>",     nextWS)
    , ("M-<Up>",       prevWS)
    , ("M-S-<Down>",   shiftToNext >> nextWS)
    , ("M-S-<Up>",     shiftToPrev >> prevWS)
    , ("M-z",          toggleWS)
    , ("M-m",          windows W.focusMaster)
    , ("M-S-m",        promote) -- swapMaster
    , ("M-C-r",        sendMessage $ Toggle MIRROR)
    , ("M-C-x",        sendMessage $ Toggle REFLECTX)
    , ("M-C-y",        sendMessage $ Toggle REFLECTY)
    , ("M-f",          sendMessage $ Toggle FULL)
    , ("M-<Space>",    windowPromptGoto myPrompt)
    , ("M-S-<Space>",  windowPromptBring myPrompt)
    , ("M-`",          setLayout $ XMonad.layoutHook c)
    , ("M-t",          withFocused $ windows . W.sink)
    , ("M-=",          sendMessage (IncMasterN 1))
    , ("M--",          sendMessage (IncMasterN (-1)))
    , ("M-S-=",        sendMessage Taller)
    , ("M-S--",        sendMessage Wider)
    , ("M-S-<Esc>",    io (exitWith ExitSuccess))
    , ("M-<Esc>",      spawn "xmonad --recompile && xmonad-restart")
    ] ++
    [(m ++ (show k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces c) [1..9]
        , (f, m) <- [(W.greedyView, "M-"), (W.shift, "M-S-"), (copy, "M-C-")]]
-- }}}
-- {{{ Theming
-- Apprentice colorscheme: https://github.com/romainl/Apprentice
apprentice = Colorscheme { bg       = "#262626"
                         , fg       = "#bcbcbc"
                         , black    = "#1c1c1c"
                         , black'   = "#444444"
                         , red      = "#af5f5f"
                         , red'     = "#ff8700"
                         , green    = "#5f875f"
                         , green'   = "#87af87"
                         , yellow   = "#87875f"
                         , yellow'  = "#ffffaf"
                         , blue     = "#5f87af"
                         , blue'    = "#8fafd7"
                         , magenta  = "#5f5f87"
                         , magenta' = "#8787af"
                         , cyan     = "#5f8787"
                         , cyan'    = "#5fafaf"
                         , white    = "#6c6c6c"
                         , white'   = "#ffffff"
                         }
myCS   = apprentice
myFont = "-*-terminus-*-*-*-*-12-*-*-*-*-*-*-*"
-- }}}
-- {{{ XMobar
myBar = "xmobar" ++ concat myXmobargs
  where myXmobargs = [
                       " -B ", "'" ++ (bg myCS) ++ "'"
                     , " -F ", "'" ++ (fg myCS) ++ "'"
                     , " -f ", "'" ++ myFont ++ "'"
                     , " -a ", "'}{'"   -- chars to sep sections
                     , " -s ", "'%'"    -- char to sep text from commands
                     , " -o"            -- place at top, others = "-b"
                     ]

myPP = defaultPP
           {
             ppCurrent         = xmobarColor (red' myCS) ""
           -- , ppVisible         = xmobarColor (cyan myCS) ""
           , ppHidden          = xmobarColor (yellow myCS) ""
           , ppHiddenNoWindows = const ""
           , ppUrgent          = xmobarColor "red" "yellow" . wrap "!" "!"
           , ppSep             = xmobarColor (black' myCS) "" "<>"
           , ppWsSep           = xmobarColor (black' myCS) "" ","
           , ppTitle           = xmobarColor (green' myCS) "" . shorten 40
             -- ^ add `. ('}':)` when you figure out the xmobar escaping thing
           , ppLayout          = \s -> xmobarColor (magenta myCS) "" $
                                 case s of -- TODO; use regexes?
                                   "SmartSpacing 2 Tall"             -> "|"
                                   "Mirror SmartSpacing 2 Tall"      -> "-"
                                   "SmartSpacing 2 Mirror Accordion" -> "N"
                                   "Mirror SmartSpacing 2 Mirror Accordion"
                                                                     -> "Z"
                                   "SmartSpacing 2 Spiral"           -> "@"
                                   "Mirror SmartSpacing 2 Spiral"    -> "e"
                                   "Tabbed Simplest"                 -> "t"
                                   "Full"                            -> "_"
                                   _                                 -> s
           , ppOrder           = \(ws:l:t:_) -> [ws, l, t]
           , ppExtras          = []
           }
-- }}}
-- {{{ XMonad.Prompt config
myPrompt = defaultXPConfig
           {
             position        = Top
           , bgColor         = bg myCS
           , fgColor         = fg myCS
           , borderColor     = black' myCS
           , fgHLight        = black myCS
           , bgHLight        = green' myCS
           , font            = myFont
           , alwaysHighlight = True
           , historySize     = 10000
           , height          = 16
           , searchPredicate = isInfixOf
           }
-- }}}
-- {{{ XMonad.Layout.Tabbed config
myTabConfig = defaultTheme
            {
              activeColor         = bg myCS
            , inactiveColor       = black myCS
            , activeBorderColor   = bg myCS
            , inactiveBorderColor = black' myCS
            , activeTextColor     = red' myCS
            , inactiveTextColor   = black' myCS
            , urgentColor         = red myCS
            , urgentBorderColor   = red' myCS
            , urgentTextColor     = white' myCS
            , decoHeight          = 14
            , fontName            = myFont
            }
-- }}}
-- {{{ XMonad.Actions.Commands config
myCommands = defaultCommands
-- }}}
