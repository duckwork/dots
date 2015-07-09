-- Imports {{{
import           Colorschemes
import           Control.Monad                        (liftM2)
import           Data.Char
import           Data.List
import qualified Data.Map                             as M
import           Data.Monoid
import           Fuzzy
import           System.Exit
import           TermAppLauncher
import           XMonad                               hiding ((|||))
import           XMonad.Actions.Commands
import           XMonad.Actions.CopyWindow
import           XMonad.Actions.CycleWS
import           XMonad.Actions.CycleWS
import qualified          XMonad.Actions.DynamicWorkspaceOrder as DO
import           XMonad.Actions.DynamicWorkspacesPure
import           XMonad.Actions.Navigation2D
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
import           XMonad.Prompt
import           XMonad.Prompt.AppLauncher
import           XMonad.Prompt.RunOrRaise
import           XMonad.Prompt.Shell
import           XMonad.Prompt.Window
import qualified XMonad.StackSet                      as W
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
    [ "home" -- beginning screen
    , "webs" -- web browser & media
    , "term" -- coding + writing
    , "talk" -- communication (irc, email)
    , "docs" -- gui editors (libreoffice, gimp, etc.)
    , "arts" -- media
    ]
    -- ++ map show [7..9]
-- }}}
myLayoutHook = -- {{{
               avoidStruts
             . smartBorders
             . mkToggle (single FULL)
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
             ppCurrent         = xmobarColor (red' myCS) "" . map toUpper
           , ppHidden          = check
           , ppHiddenNoWindows = xmobarColor (black' myCS) ""
           , ppUrgent          = xmobarColor (white myCS) (red' myCS)
           -- , ppSort            = DO.getSortByOrder
           , ppSep             = xmobarColor (black' myCS) "" "//"
           , ppWsSep           = xmobarColor (black' myCS) "" " "
           , ppTitle           = xmobarColor (green' myCS) "" . shorten 40
           , ppLayout          = xmobarColor (magenta' myCS) ""
           , ppOrder           = \(ws:l:t:_) -> [ws]
           , ppExtras          = []
           , ppOutput          = hPutStrLn h
           }
-- {{{ XMobar options
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
-- }}}
myManageHook = -- {{{
        insertPosition Below Newer -- Xmonad default = Above Newer
    <+> manageDocks
    <+> (composeAll . concat $ -- {{{
        [ [isDialog --> doFloat]
        , [className =? c --> doFloat | c <- myFloats]
        , [resource  =? i --> doIgnore | i <- myIgnores]
        , [(className =? x <||> title =? x <||> resource =? x)
                                    --> doShiftAndGo "home" | x <- my1Shifts]
        , [(className =? x <||> title =? x <||> resource =? x)
                                    --> doShiftAndGo "webs" | x <- my2Shifts]
        , [(className =? x <||> title =? x <||> resource =? x)
                                    --> doShiftAndGo "term" | x <- my3Shifts]
        , [(className =? x <||> title =? x <||> resource =? x)
                                    --> doShiftAndGo "talk" | x <- my4Shifts]
        , [(className =? x <||> title =? x <||> resource =? x)
                                    --> doShiftAndGo "docs" | x <- my5Shifts]
        , [(className =? x <||> title =? x <||> resource =? x)
                                    --> doShiftAndGo "arts" | x <- my6Shifts]
        -- , [(className =? x <||> title =? x <||> resource =? x)
        --                             --> doShiftAndGo "7" | x <- my7Shifts]
        -- , [(className =? x <||> title =? x <||> resource =? x)
        --                             --> doShiftAndGo "8" | x <- my8Shifts]
        -- , [(className =? x <||> title =? x <||> resource =? x)
        --                             --> doShiftAndGo "9" | x <- my9Shifts]
        ]) -- }}}
        where
            doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift
            -- doShiftAndGo ws = doF (addHiddenWorkspace' ws myLayoutHook) <+> doShift ws
            -- doShiftAndGo ws = liftX (asks (layoutHook . config)) >>=
            --                  \l -> doF (addHiddenWorkspace'' ws l) <+> doShift ws
            myFloats = ["MPlayer", "xmessage"]
            myIgnores = ["desktop_window", "kdesktop"]
            my1Shifts = []
            my2Shifts = [ -- web browsers
                          ".uzbl-core-wrapped"
                        , "Navigator" , "Firefox"
                        , "surf", "Surf"
                        , "Chromium"
                        ]
            my3Shifts = [ -- coding + writing + files
                          "vim"
                        , "ranger"
                        , "Termite"
                        ]
            my4Shifts = []
            my5Shifts = []
            my6Shifts = ["MPlayer"]
            -- my7Shifts = []
            -- my8Shifts = []
            -- my9Shifts = []
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
    , ("M-`",          withFocused $ windows . W.sink)
    , ("M-S-`",        withFocused $ windows . flip W.float --Middle of screen
                                     (W.RationalRect (1/4) (1/4) (1/2) (1/2)))
    , ("M-C-`",        switchLayer)
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
    , ("M-[",          sendMessage (IncMasterN (-1)))
    , ("M-]",          sendMessage (IncMasterN 1))
    , ("M-\\",         sendMessage NextLayout)
    , ("M-=",          sendMessage $ Toggle FULL)
    , ("M-S-=",        sendMessage ToggleStruts) -- extra FULLness
    -- , ("M-q",          kill1)
    , ("M-q",          removeEmptyWorkspaceAfterExcept ["home"] kill1)
    ] ++ -- }}}
    [ -- Workspaces {{{
      ("M-.",          moveTo Next NonEmptyWS)
    , ("M-,",          moveTo Prev NonEmptyWS)
    , ("M-S-.",        shiftToNext >> nextWS)
    , ("M-S-,",        shiftToPrev >> prevWS)
    , ("M-;",          addWorkspacePrompt myPrompt)
    ] ++
    -- zip ["M-1", "M-2", "M-3", "M-4", "M-5", "M-6", "M-7", "M-8", "M-9"] (map (withNthWorkspace W.greedyView) [0..])
    [ -- (List comprehension for switching, shifting, etc.)
      (otherModMasks ++ "M-" ++ [key], action tag)
      | (tag, key) <- zip myWS (concat $ map show [1..])
      , (otherModMasks, action) <- [ ("",   toggleOrView)
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
    , ("M-<Space>",    runOrRaisePrompt myPrompt)  -- TODO: Combine
    , ("M-/",          windowPromptGoto myPrompt)  --       all of these
    , ("M-S-/",        windowPromptBring myPrompt) --       & do fuzzy search
    , ("M-S-r",        myCommands >>= runCommand)  -- TODO: change binding
    , ("M-S-<Esc>",    io (exitWith ExitSuccess))
    , ("M-<Esc>",      spawn "xmonad --recompile && xmonad-restart")
    ] -- }}}
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
           , searchPredicate = fuzzyMatch
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
