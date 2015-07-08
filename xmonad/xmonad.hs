-- Imports {{{
import           Colorschemes
import           Control.Monad                       (liftM2)
import           Data.List
import qualified Data.Map                            as M
import           Data.Monoid
import           System.Exit
import           TermAppLauncher
import           XMonad                              hiding ((|||))
import           XMonad.Actions.Commands
import           XMonad.Actions.CopyWindow
import           XMonad.Actions.CycleWS
import           XMonad.Actions.Navigation2D
import           XMonad.Actions.Promote
import           XMonad.Actions.Search
import           XMonad.Actions.Submap
import           XMonad.Actions.WindowBringer
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.FadeInactive
import           XMonad.Hooks.InsertPosition
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.Accordion
import           XMonad.Layout.DragPane
import           XMonad.Layout.LayoutCombinators
import           XMonad.Layout.LimitWindows
import           XMonad.Layout.MouseResizableTile
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Renamed
import           XMonad.Layout.Tabbed
import           XMonad.Layout.WindowNavigation
import           XMonad.Prompt
import           XMonad.Prompt.AppLauncher
import           XMonad.Prompt.RunOrRaise
import           XMonad.Prompt.Shell
import           XMonad.Prompt.Window
import qualified XMonad.StackSet                     as W
import           XMonad.Util.EZConfig
import           XMonad.Util.Run
-- import XMonad.Actions.WindowGo
-- }}}
-- {{{ Default programs, etc.
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
-- TODO: use `env <- M.fromList `fmap` getEnvironment` for env vars
myTerm        = "termite"
myBrowser  = "uzbl-browser"
duckduckgo = searchEngine "duckduckgo" "https://duckduckgo.com/?q="
mySearch = duckduckgo
-- }}}
-- Main {{{
main = do
    h <- spawnPipe myBar
    xmonad $ withNavigation2DConfig defaultNavigation2DConfig
                         { defaultTiledNavigation = centerNavigation
                         , layoutNavigation   = [("Full", centerNavigation)]
                         , unmappedWindowRect = [("Full", singleWindowRect)]
                         }
           $ defaultConfig
                         { modMask            = myModMask
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
myWS = -- {{{
    [ "1" -- web browser & media
    , "2" -- coding + writing & files
    , "3" -- communication (irc, email)
    ] ++ map show [4..9]
-- }}}
myLayoutHook = -- {{{
               avoidStruts
             . smartBorders
             . mkToggle (single FULL)
             $ onWorkspace "1" myTabbed
             $ onWorkspace "2" (myMRTile ||| myMirroredMRTile)
             $
                   myMRTile
               ||| myMirroredMRTile
               ||| myTabbed
    where
          myMRTile    = renamed [Replace "Tiled"] $
                          mouseResizableTile { nmaster        = 1
                                             , masterFrac    = 1/2
                                             , fracIncrement = 1/50
                                             }
          myMirroredMRTile    = renamed [Replace "Mirror"] $
                                  mouseResizableTile { nmaster        = 1
                                                     , masterFrac    = 1/2
                                                     , fracIncrement = 1/50
                                                     , isMirrored    = True
                                                     }
          myTabbed    = renamed [Replace "Tabbed"] $
                              tabbed shrinkText myTabConfig
-- }}}
myLogHook h = do -- {{{
    fadeInactiveLogHook 0.7
    copies <- wsContainingCopies
    let check ws | ws `elem` copies = xmobarColor (red myCS) "" $ ws
                 | otherwise = xmobarColor (yellow myCS) "" $ ws
     in dynamicLogWithPP defaultPP
           {
             ppCurrent         = xmobarColor (red' myCS) "" . wrap "[" "]"
           , ppHidden          = check . pad
           , ppHiddenNoWindows = xmobarColor (black' myCS) "" . pad
           , ppUrgent          = xmobarColor (white myCS) (red' myCS)
           , ppSep             = xmobarColor (black' myCS) "" "//"
           , ppWsSep           = xmobarColor (black' myCS) "" ""
           , ppTitle           = xmobarColor (green' myCS) "" . shorten 40
           , ppLayout          = xmobarColor (magenta' myCS) ""
           , ppOrder           = \(ws:l:t:_) -> [ws]
           , ppExtras          = []
           , ppOutput          = hPutStrLn h
           }
-- }}}
myManageHook = -- {{{
        insertPosition Below Newer -- Xmonad default = Above Newer
    <+> manageDocks
    <+> (composeAll . concat $ -- {{{
        [ [isDialog --> doFloat]
        , [className =? c --> doFloat | c <- myFloats]
        , [resource  =? i --> doIgnore | i <- myIgnores]
        , [(className =? x <||> title =? x <||> resource =? x)
                                    --> doShiftAndGo "1" | x <- my1Shifts]
        , [(className =? x <||> title =? x <||> resource =? x)
                                    --> doShiftAndGo "2" | x <- my2Shifts]
        , [(className =? x <||> title =? x <||> resource =? x)
                                    --> doShiftAndGo "3" | x <- my3Shifts]
        , [(className =? x <||> title =? x <||> resource =? x)
                                    --> doShiftAndGo "4" | x <- my4Shifts]
        , [(className =? x <||> title =? x <||> resource =? x)
                                    --> doShiftAndGo "5" | x <- my5Shifts]
        , [(className =? x <||> title =? x <||> resource =? x)
                                    --> doShiftAndGo "6" | x <- my6Shifts]
        , [(className =? x <||> title =? x <||> resource =? x)
                                    --> doShiftAndGo "7" | x <- my7Shifts]
        , [(className =? x <||> title =? x <||> resource =? x)
                                    --> doShiftAndGo "8" | x <- my8Shifts]
        , [(className =? x <||> title =? x <||> resource =? x)
                                    --> doShiftAndGo "9" | x <- my9Shifts]
        ]) -- }}}
        where
            doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift
            myFloats = ["MPlayer"]
            myIgnores = ["desktop_window", "kdesktop"]
            my1Shifts = [ -- web browsers
                          ".uzbl-core-wrapped"
                        , "Navigator" , "Firefox"
                        , "surf", "Surf"
                        , "Chromium"
                        ]
            my2Shifts = [ -- coding + writing + files
                          "vim"
                        , "ranger"
                        , "Termite"
                        ]
            my3Shifts = []
            my4Shifts = []
            my5Shifts = []
            my6Shifts = []
            my7Shifts = []
            my8Shifts = []
            my9Shifts = []
-- }}}
myHandleEventHook = docksEventHook
myStartupHook = return ()
-- }}}
-- {{{ Key & Mouse bindings
myModMask = mod1Mask        -- Alt
-- myModMask = mod4Mask        -- Windows key
myKeymap = \c -> mkKeymap c $
    [ -- Manipulate windows & layouts {{{
      ("M-<Tab>",      windows W.focusDown)
    , ("M-S-<Tab>",    windows W.focusUp)
    , ("M-m",          windows W.focusMaster)
    , ("M-S-m",        windows W.swapMaster)
    , ("M-t",          withFocused $ windows . W.sink)
    , ("M-S-t",        switchLayer)
    , ("M-j",          windowGo   D True)
    , ("M-S-j",        windowSwap D True)
    , ("M-k",          windowGo   U True)
    , ("M-S-k",        windowSwap U True)
    , ("M-h",          windowGo   L True)
    , ("M-S-h",        windowSwap L True)
    , ("M-l",          windowGo   R True)
    , ("M-S-l",        windowSwap R True)
    , ("M-C-j",        sendMessage ExpandSlave)
    , ("M-C-k",        sendMessage ShrinkSlave)
    , ("M-C-h",        sendMessage Shrink)
    , ("M-C-l",        sendMessage Expand)
    , ("M-]",          sendMessage (IncMasterN 1))
    , ("M-[",          sendMessage (IncMasterN (-1)))
    , ("M-b",          sendMessage ToggleStruts)
    , ("M-/",          sendMessage NextLayout)
    , ("M-=",          sendMessage $ Toggle FULL)
    ] ++ -- }}}
    [ -- Workspaces {{{
      ("M-.",          moveTo Next NonEmptyWS)
    , ("M-,",          moveTo Prev NonEmptyWS)
    , ("M-S-.",        shiftToNext >> nextWS)
    , ("M-S-,",        shiftToPrev >> prevWS)
    ] ++
    [ -- (List comprehension for switching, shifting, etc.)
      (otherModMasks ++ "M-" ++ [key], action tag)
      | (tag, key) <- zip myWS (concat $ map show [1..9])
      , (otherModMasks, action) <- [ ("",   windows . W.greedyView)
                                   , ("S-", \w -> windows $ W.greedyView w . W.shift w)
                                   , ("C-", \w -> windows $ W.greedyView w . copy w)
                                   ]
    ] ++ -- }}}
    [ -- Spawn programs, windows, XMonad commands {{{
      ("M-<Return>",   spawn $ terminal c)
    -- , ("M-;",          spawn "exe=`yeganesh -x` && eval \"exec $exe\"")
    , ("M-e",          launchAppInTerm myPrompt "vim")
    , ("M-S-e",        spawn (myTerm ++ "-t vim -e vim"))
    , ("M-w",          promptSearchBrowser myPrompt myBrowser mySearch)
    , ("M-;",          runOrRaisePrompt myPrompt)  -- TODO: Combine
    , ("M-<Space>",    windowPromptGoto myPrompt)  --       all of these
    , ("M-S-<Space>",  windowPromptBring myPrompt) --       & do fuzzy search
    , ("M-S-r",        myCommands >>= runCommand)  -- TODO: change binding
    , ("M-q",          kill1)
    , ("M-S-<Esc>",    io (exitWith ExitSuccess))
    , ("M-<Esc>",      spawn "xmonad --recompile && xmonad-restart")
    ] -- }}}
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
            , inactiveTextColor   = fg myCS
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
-- vim:fdm=marker
