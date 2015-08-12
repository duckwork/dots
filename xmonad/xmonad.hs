-- Imports {{{
import           Colorschemes
import           Colorschemes.Apprentice
import           Control.Monad                        (liftM2)
import           Data.Char
import           Data.List
import qualified Data.Map                             as M
import           Data.Monoid
import           Fuzzy
import           DwmActions
import           System.Exit
import           TermAppLauncher
import           XMonad                               hiding ((|||))
import           XMonad.Actions.CopyWindow
import           XMonad.Actions.CycleSelectedLayouts
import           XMonad.Actions.CycleWS
import           XMonad.Actions.CycleWS
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
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
import           XMonad.Layout.Fullscreen
import qualified XMonad.Layout.Groups                 as G
import XMonad.Layout.Groups.Examples
import           XMonad.Layout.Groups.Wmii
import           XMonad.Layout.LayoutCombinators
import           XMonad.Layout.LayoutHints
import           XMonad.Layout.LimitWindows
import           XMonad.Layout.MouseResizableTile
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
-- import XMonad.Actions.FloatSnap
-- import XMonad.Actions.WindowGo
-- }}}
-- {{{ Defaults
-- {{{ Theming
-- Apprentice colorscheme: https://github.com/romainl/Apprentice
myCS   = apprentice
myFont = "-*-terminus-*-*-*-*-12-*-*-*-*-*-*-*"
-- }}}
-- TODO: use `env <- M.fromList `fmap` getEnvironment` for env vars
myTerm     = "termite"
-- myBrowser  = "uzbl-browser"
myBrowser = "chromium-browser"
duckduckgo = searchEngine "duckduckgo" "https://duckduckgo.com/?q="
-- mySearch   = duckduckgo
mySearch = google
myDmenuOpts = unwords [
                        "-i"
                      , "-p '>'"
                      , "-fn '" ++ myFont ++ "'"
                      , "-nb '" ++ bg myCS ++ "'"
                      , "-nf '" ++ fg myCS ++ "'"
                      , "-sb '" ++ green myCS ++ "'"
                      , "-sf '" ++ bg myCS ++ "'"
                      ]
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
                         , terminal           = myTerm
                         , workspaces         = myWS
                         , borderWidth        = 1
                         , normalBorderColor  = bg myCS
                         , focusedBorderColor = green myCS
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
    [ my1 , my2 , my3 , my4 , my5 ]
my1 = "home" -- beginning screen
my2 = "surf" -- web browser
my3 = "edit" -- coding + writing
my4 = "docs" -- gui editors (libreoffice, gimp, etc.)
my5 = "view" -- media
-- }}}
myLayoutHook = -- {{{
               avoidStruts
             . smartBorders
             . layoutHints
             . fullscreenFocus
             $
                   myMRTile
               ||| myMirroredMRTile
               ||| myTabbed
               ||| myWmii
               ||| Full
    where -- renamed layouts should be <= 6 characters
          myWmii              = renamed [Prepend "wmii-", CutWordsRight 2] $
                                  wmii shrinkText myTabConfig
          myMRTile            = renamed [Replace "tiled"] $
                                  mouseResizableTile { nmaster        = 1
                                                     , masterFrac    = 1/2
                                                     , fracIncrement = 1/50
                                                     }
          myMirroredMRTile    = renamed [Replace "mirror"] $
                                  mouseResizableTile { nmaster        = 1
                                                     , masterFrac    = 1/2
                                                     , fracIncrement = 1/50
                                                     , isMirrored    = True
                                                     }
          myTabbed            = renamed [Replace "tabbed"] $
                                      tabbed shrinkText myTabConfig
-- }}}
myLogHook h = do -- {{{ ( & dynamicLogWithPP config
    fadeInactiveLogHook 0.7
    copies <- wsContainingCopies
    let checkCopy ws | ws `elem` copies = xmobarColor (red myCS) "" $ ws
                     | otherwise        = xmobarColor (yellow myCS) "" $ ws
        mkLengthL n xs | length xs < n = replicate (n - length xs) '_' ++ xs
                       | otherwise     = shorten n xs
        mkLengthR n xs | length xs < n = xs ++ replicate (n - length xs) '_'
                       | otherwise     = take n xs
        myTitleLength = 20
     in dynamicLogWithPP defaultPP
           {
             ppCurrent         = xmobarColor (red' myCS) ""
                                . map toUpper
           , ppHidden          = checkCopy
           , ppHiddenNoWindows = xmobarColor (black' myCS) ""
           , ppUrgent          = xmobarColor (white myCS) (red' myCS)
           , ppSep             = xmobarColor (black' myCS) "" " // "
           , ppWsSep           = " "
           , ppTitle           = xmobarColor (green' myCS) ""
                                . mkLengthL myTitleLength
           , ppLayout          = xmobarColor (magenta' myCS) ""
                                . mkLengthR myTitleLength
           , ppOrder           = \(ws:l:t:_) -> [t, ws, l]
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
    <+> fullscreenManageHook
    <+> manageDocks
    <+> (composeAll . concat $ -- {{{
        [ [isDialog --> doFloat]
        , [className =? c --> doFloat | c <- myFloats]
        , [resource  =? i --> doIgnore | i <- myIgnores]
        , [(className =? x <||> title =? x <||> resource =? x)
                                    --> doShiftAndGo my1 | x <- my1Shifts]
        , [(className =? x <||> title =? x <||> resource =? x)
                                    --> doShiftAndGo my2 | x <- my2Shifts]
        , [(className =? x <||> title =? x <||> resource =? x)
                                    --> doShiftAndGo my3 | x <- my3Shifts]
        , [(className =? x <||> title =? x <||> resource =? x)
                                    --> doShiftAndGo my4 | x <- my4Shifts]
        , [(className =? x <||> title =? x <||> resource =? x)
                                    --> doShiftAndGo my5 | x <- my5Shifts]
        ]) -- }}}
        where
            -- doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift
            doShiftAndGo = doShift
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
                        , "gvim", "Gvim"
                        ]
            my4Shifts = []
            my5Shifts = []
            my6Shifts = ["MPlayer"]
-- }}}
myHandleEventHook = -- {{{
      hintsEventHook
  <+> fullscreenEventHook
  <+> docksEventHook
-- }}}
myStartupHook = return ()
-- }}}
-- {{{ Key & Mouse bindings
myModMask = mod1Mask        -- Alt
-- myModMask = mod4Mask        -- Windows key
myKeymap = \c -> mkKeymap c $
    [ -- Manipulate windows & layouts {{{
      ("M-<Tab>",         windows W.focusDown)
    , ("M-S-<Tab>",       windows W.focusUp)
    , ("M-'",             withFocused $ windows . W.sink)
    , ("M-S-'",           withFocused $ windows . flip W.float
                                     (W.RationalRect (1/4) (1/4) (1/2) (1/2)))
    , ("M-C-'",           switchLayer) -- TODO: change these to check if window is floating and toggle
    , ("M-h",             declareMsg "goL")
    , ("M-S-h",           declareMsg "swapL")
    , ("M-j",             declareMsg "goD")
    , ("M-S-j",           declareMsg "swapD")
    , ("M-k",             declareMsg "goU")
    , ("M-S-k",           declareMsg "swapU")
    , ("M-l",             declareMsg "goR")
    , ("M-S-l",           declareMsg "swapR")
    , ("M-C-j",           declareMsg "resizeD")
    , ("M-C-k",           declareMsg "resizeU")
    , ("M-C-h",           declareMsg "resizeL")
    , ("M-C-l",           declareMsg "resizeR")
    , ("M-[",             declareMsg "nmDecOrPrevLayout")
    , ("M-]",             declareMsg "nmIncOrNextLayout")
    , ("M-\\",            windows W.focusMaster)
    , ("M-S-\\",          windows W.swapMaster)
    , ("M-<KP_Divide>",   sendMessage $ JumpToLayout "tabbed")
    , ("M-<KP_Multiply>", sendMessage $ JumpToLayout "Full")
    , ("M-S-<KP_Multiply>", sendMessage ToggleStruts)
    , ("M-<KP_Subtract>", sendMessage $ JumpToLayout "mirror")
    , ("M-<KP_Add>",      sendMessage $ JumpToLayout "tiled")
    , ("M-<KP_Enter>",    sendMessage $ JumpToLayout "wmii-Column")
    , ("M-q",             kill1)
    ] ++ -- }}}
    [ -- Workspaces {{{
      ("M-0",          moveTo Next NonEmptyWS)
    , ("M-9",          moveTo Prev NonEmptyWS)
    , ("M-S-0",        shiftToNext >> nextWS)
    , ("M-S-9",        shiftToPrev >> prevWS)
    ] ++
    [ -- (List comprehension for switching, shifting, etc.)
      (otherModMasks ++ "M-" ++ [key], action tag)
      | (tag, key) <- zip myWS (concat $ map show [1..])
      , (otherModMasks, action) <- [ ("",   \t -> windows $ toggleViewEmpty t)
                                   , ("S-", \t -> windows $ W.shift t)
                                   , ("C-", toggleOrView)
                                   ]
    ] ++ -- }}}
    [ -- Spawn programs, windows, XMonad commands {{{
      ("M-<Return>",   spawn $ terminal c)
    , ("M-<Space>",    spawn $ "exe=`yeganesh -x -p exe -- " ++ myDmenuOpts ++
                               "` && eval \"exec $exe\"")
    , ("M-e",          launchApp myPrompt "gvim")
    , ("M-S-e",        spawn "gvim")
    -- , ("M-w",          promptSearchBrowser myPrompt myBrowser mySearch)
    -- , ("M-<Space>",    runOrRaisePrompt myPrompt)  -- TODO: Combine
    , ("M-/",          windowPromptGoto myPrompt)  --       all of these
    , ("M-S-/",        windowPromptBring myPrompt) --       & do fuzzy search
    , ("M-S-<Esc>",    io (exitWith ExitSuccess))
    , ("M-<Esc>",      spawn "xmonad --recompile && xmonad-restart")
    ] -- }}}
  where declareMsg msg = do -- {{{
          winset <- gets windowset
          let ld = description . W.layout . W.workspace . W.current $ winset
          case msg of
            "goL"        -> case ld of
                             "tabbed"      -> windows W.focusUp
                             "wmii-Column" -> focusGroupUp
                             "wmii-Tabs"   -> focusGroupUp
                             "wmii-Full"   -> focusGroupUp
                             _             -> windowGo L True
            "swapL"      -> case ld of
                             "tabbed"      -> windows W.swapUp
                             "wmii-Column" -> moveToGroupUp False
                             "wmii-Tabs"   -> moveToGroupUp False
                             "wmii-Full"   -> moveToGroupUp False
                             _             -> windowSwap L True
            "resizeL"    -> case ld of
                             "mirror"      -> sendMessage ShrinkSlave
                             "wmii-Column" -> zoomGroupOut
                             "wmii-Tabs"   -> zoomGroupOut
                             "wmii-Full"   -> zoomGroupOut
                             _             -> sendMessage Shrink
            "goD"        -> case ld of
                             "tabbed"      -> windows W.focusDown
                             "wmii-Column" -> focusDown
                             "wmii-Tabs"   -> focusDown
                             "wmii-Full"   -> focusDown
                             _             -> windowGo D True
            "swapD"      -> case ld of
                             "tabbed"      -> windows W.swapDown
                             "wmii-Column" -> swapDown
                             "wmii-Tabs"   -> swapDown
                             "wmii-Full"   -> swapDown
                             _             -> windowSwap D True
            "resizeD"    -> case ld of
                             "mirror"      -> sendMessage Expand
                             "wmii-Column" -> swapGroupDown
                             "wmii-Tabs"   -> swapGroupDown
                             "wmii-Full"   -> swapGroupDown
                             _             -> sendMessage ExpandSlave
            "goU"        -> case ld of
                             "tabbed"      -> windows W.focusUp
                             "wmii-Column" -> focusUp
                             "wmii-Tabs"   -> focusUp
                             "wmii-Full"   -> focusUp
                             _             -> windowGo U True
            "swapU"      -> case ld of
                             "tabbed"      -> windows W.swapUp
                             "wmii-Column" -> swapUp
                             "wmii-Tabs"   -> swapUp
                             "wmii-Full"   -> swapUp
                             _             -> windowSwap U True
            "resizeU"    -> case ld of
                             "mirror"      -> sendMessage Shrink
                             "wmii-Column" -> swapGroupUp
                             "wmii-Tabs"   -> swapGroupUp
                             "wmii-Full"   -> swapGroupUp
                             _             -> sendMessage ShrinkSlave
            "goR"        -> case ld of
                             "tabbed"      -> windows W.focusDown
                             "wmii-Column" -> focusGroupDown
                             "wmii-Tabs"   -> focusGroupDown
                             "wmii-Full"   -> focusGroupDown
                             _             -> windowGo R True
            "swapR"      -> case ld of
                             "tabbed"      -> windows W.swapDown
                             "wmii-Column" -> moveToGroupDown False
                             "wmii-Tabs"   -> moveToGroupDown False
                             "wmii-Full"   -> moveToGroupDown False
                             _             -> windowSwap R True
            "resizeR"    -> case ld of
                             "mirror"      -> sendMessage ExpandSlave
                             "wmii-Column" -> zoomGroupIn
                             "wmii-Tabs"   -> zoomGroupIn
                             "wmii-Full"   -> zoomGroupIn
                             _             -> sendMessage Expand
            "nmIncOrNextLayout" -> case ld of
                             "wmii-Column" -> groupToNextLayout
                             "wmii-Tabs"   -> groupToNextLayout
                             "wmii-Full"   -> groupToNextLayout
                             _             -> sendMessage $ IncMasterN 1
            "nmDecOrPrevLayout" -> case ld of
                             "wmii-Column" -> groupToFullLayout
                             "wmii-Tabs"   -> groupToVerticalLayout
                             "wmii-Full"   -> groupToTabbedLayout
                             _             -> sendMessage $ IncMasterN (-1)
    -- }}}
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
-- vim:fdm=marker
